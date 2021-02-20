/*
 *  Copyright 2019-2021 Diligent Graphics LLC
 *  Copyright 2015-2019 Egor Yusov
 *  
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *      http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *  In no event and under no legal theory, whether in tort (including negligence), 
 *  contract, or otherwise, unless required by applicable law (such as deliberate 
 *  and grossly negligent acts) or agreed to in writing, shall any Contributor be
 *  liable for any damages, including any direct, indirect, special, incidental, 
 *  or consequential damages of any character arising as a result of this License or 
 *  out of the use or inability to use the software (including but not limited to damages 
 *  for loss of goodwill, work stoppage, computer failure or malfunction, or any and 
 *  all other commercial damages or losses), even if such Contributor has been advised 
 *  of the possibility of such damages.
 */

#include "pch.h"
#include "PipelineResourceSignatureGLImpl.hpp"
#include "RenderDeviceGLImpl.hpp"
#include "ShaderResourceBindingGLImpl.hpp"
#include "GLPipelineResourceLayout.hpp"

namespace Diligent
{
namespace
{

inline bool ResourcesCompatible(const PipelineResourceSignatureGLImpl::ResourceAttribs& lhs,
                                const PipelineResourceSignatureGLImpl::ResourceAttribs& rhs)
{
    // Ignore sampler index.
    // clang-format off
    return lhs.CacheOffset          == rhs.CacheOffset &&
           lhs.ImtblSamplerAssigned == rhs.ImtblSamplerAssigned;
    // clang-format on
}
} // namespace


PipelineResourceSignatureGLImpl::PipelineResourceSignatureGLImpl(IReferenceCounters*                        pRefCounters,
                                                                 RenderDeviceGLImpl*                        pDeviceGL,
                                                                 const PipelineResourceSignatureCreateInfo& CreateInfo,
                                                                 bool                                       bIsDeviceInternal) :
    TPipelineResourceSignatureBase{pRefCounters, pDeviceGL, CreateInfo.Desc, bIsDeviceInternal}
{
    const auto& Desc = CreateInfo.Desc;
    try
    {
        FixedLinearAllocator MemPool{GetRawAllocator()};

        // Reserve at least 1 element because m_pResourceAttribs must hold a pointer to memory
        MemPool.AddSpace<ResourceAttribs>(std::max(1u, Desc.NumResources));
        MemPool.AddSpace<SamplerPtr>(Desc.NumImmutableSamplers);

        ReserveSpaceForDescription(MemPool, Desc);

        const auto NumStaticResStages = GetNumStaticResStages();
        if (NumStaticResStages > 0)
        {
            MemPool.AddSpace<GLProgramResourceCache>(1);
            MemPool.AddSpace<GLPipelineResourceLayout>(NumStaticResStages);
        }

        MemPool.Reserve();

        m_pResourceAttribs  = MemPool.Allocate<ResourceAttribs>(std::max(1u, m_Desc.NumResources));
        m_ImmutableSamplers = MemPool.ConstructArray<SamplerPtr>(m_Desc.NumImmutableSamplers);

        // The memory is now owned by PipelineResourceSignatureGLImpl and will be freed by Destruct().
        auto* Ptr = MemPool.ReleaseOwnership();
        VERIFY_EXPR(Ptr == m_pResourceAttribs);
        (void)Ptr;

        CopyDescription(MemPool, Desc);

        if (NumStaticResStages > 0)
        {
            m_StaticResourceCache   = MemPool.Construct<GLProgramResourceCache>();
            m_StaticResourceLayouts = MemPool.ConstructArray<GLPipelineResourceLayout>(NumStaticResStages, std::ref(*this), std::ref(*m_StaticResourceCache));
        }

        CreateLayouts(CreateInfo);

        if (NumStaticResStages > 0)
        {
            constexpr SHADER_RESOURCE_VARIABLE_TYPE AllowedVarTypes[] = {SHADER_RESOURCE_VARIABLE_TYPE_STATIC};
            for (Uint32 i = 0; i < m_StaticResStageIndex.size(); ++i)
            {
                Int8 Idx = m_StaticResStageIndex[i];
                if (Idx >= 0)
                {
                    VERIFY_EXPR(static_cast<Uint32>(Idx) < NumStaticResStages);
                    const auto ShaderType = GetShaderTypeFromPipelineIndex(i, GetPipelineType());
                    m_StaticResourceLayouts[Idx].Initialize(*this, AllowedVarTypes, _countof(AllowedVarTypes), ShaderType);
                }
            }
        }

        m_Hash = CalculateHash();
    }
    catch (...)
    {
        Destruct();
        throw;
    }
}

SHADER_RESOURCE_RANGE PipelineResourceToShaderResourceRange(const PipelineResourceDesc& Desc)
{
    static_assert(SHADER_RESOURCE_TYPE_LAST == SHADER_RESOURCE_TYPE_ACCEL_STRUCT, "AZ TODO");
    switch (Desc.ResourceType)
    {
        // clang-format off
        case SHADER_RESOURCE_TYPE_CONSTANT_BUFFER: return SHADER_RESOURCE_RANGE_CONSTANT_BUFFER;
        case SHADER_RESOURCE_TYPE_TEXTURE_SRV:     return SHADER_RESOURCE_RANGE_TEXTURE_SRV;
        case SHADER_RESOURCE_TYPE_BUFFER_SRV:      return (Desc.Flags & PIPELINE_RESOURCE_FLAG_FORMATTED_BUFFER) ? SHADER_RESOURCE_RANGE_TEXTURE_SRV : SHADER_RESOURCE_RANGE_BUFFER_UAV;
        case SHADER_RESOURCE_TYPE_TEXTURE_UAV:     return SHADER_RESOURCE_RANGE_TEXTURE_UAV;
        case SHADER_RESOURCE_TYPE_BUFFER_UAV:      return (Desc.Flags & PIPELINE_RESOURCE_FLAG_FORMATTED_BUFFER) ? SHADER_RESOURCE_RANGE_TEXTURE_UAV : SHADER_RESOURCE_RANGE_BUFFER_UAV;
            // clang-format on
        case SHADER_RESOURCE_TYPE_SAMPLER:
        case SHADER_RESOURCE_TYPE_INPUT_ATTACHMENT:
        case SHADER_RESOURCE_TYPE_ACCEL_STRUCT:
        default:
            return SHADER_RESOURCE_RANGE_UNKNOWN;
    }
}

void PipelineResourceSignatureGLImpl::CreateLayouts(const PipelineResourceSignatureCreateInfo& CreateInfo)
{
    std::array<Uint32, SHADER_RESOURCE_RANGE_LAST + 1> StaticCounter = {};

    for (Uint32 s = 0; s < _countof(CreateInfo.BindingOffsets); ++s)
    {
        for (Uint32 r = 0; r < _countof(CreateInfo.BindingOffsets[s]); ++r)
        {
            const auto   Range    = static_cast<SHADER_RESOURCE_RANGE>(r);
            const Uint32 Offset   = CreateInfo.BindingOffsets[s][r];
            m_FirstBinding[Range] = std::max(m_FirstBinding[Range], Offset);
        }
    }

    for (Uint32 s = 0; s < m_Desc.NumImmutableSamplers; ++s)
        GetDevice()->CreateSampler(m_Desc.ImmutableSamplers[s].Desc, &m_ImmutableSamplers[s]);

    for (Uint32 i = 0; i < m_Desc.NumResources; ++i)
    {
        const auto& ResDesc = m_Desc.Resources[i];
        VERIFY(i == 0 || ResDesc.VarType >= m_Desc.Resources[i - 1].VarType, "Resources must be sorted by variable type");

        if (ResDesc.ResourceType == SHADER_RESOURCE_TYPE_SAMPLER)
        {
            Int32 ImtblSamplerIdx = FindImmutableSampler(m_Desc.ImmutableSamplers, m_Desc.NumImmutableSamplers, ResDesc.ShaderStages, ResDesc.Name, nullptr);

            new (m_pResourceAttribs + i) ResourceAttribs //
                {
                    ResourceAttribs::InvalidCacheOffset,
                    ImtblSamplerIdx < 0 ? ResourceAttribs::InvalidSamplerInd : static_cast<Uint32>(ImtblSamplerIdx),
                    ImtblSamplerIdx >= 0 //
                };
        }
        else
        {
            const auto Range = PipelineResourceToShaderResourceRange(ResDesc);
            VERIFY_EXPR(Range != SHADER_RESOURCE_RANGE_UNKNOWN);

            const Uint32 CacheOffset     = m_BindingCount[Range];
            Uint32       SamplerIdx      = ResourceAttribs::InvalidSamplerInd;
            Int32        ImtblSamplerIdx = -1;

            if (ResDesc.ResourceType == SHADER_RESOURCE_TYPE_TEXTURE_SRV)
            {
                ImtblSamplerIdx = FindImmutableSampler(m_Desc.ImmutableSamplers, m_Desc.NumImmutableSamplers, ResDesc.ShaderStages, ResDesc.Name, nullptr);
                if (ImtblSamplerIdx < 0)
                    SamplerIdx = FindAssignedSampler(ResDesc, ResourceAttribs::InvalidSamplerInd);
                else
                    SamplerIdx = static_cast<Uint32>(ImtblSamplerIdx);
            }

            new (m_pResourceAttribs + i) ResourceAttribs //
                {
                    CacheOffset,
                    SamplerIdx,
                    ImtblSamplerIdx >= 0 //
                };

            m_BindingCount[Range] += ResDesc.ArraySize;

            if (ResDesc.VarType == SHADER_RESOURCE_VARIABLE_TYPE_STATIC)
                StaticCounter[Range] += ResDesc.ArraySize;
        }
    }

    if (m_StaticResourceCache)
    {
        m_StaticResourceCache->Initialize(StaticCounter[SHADER_RESOURCE_RANGE_CONSTANT_BUFFER],
                                          StaticCounter[SHADER_RESOURCE_RANGE_TEXTURE_SRV],
                                          StaticCounter[SHADER_RESOURCE_RANGE_TEXTURE_UAV],
                                          StaticCounter[SHADER_RESOURCE_RANGE_BUFFER_UAV],
                                          GetRawAllocator());
        // Set immutable samplers for static resources.
        const auto ResIdxRange = GetResourceIndexRange(SHADER_RESOURCE_VARIABLE_TYPE_STATIC);

        for (Uint32 r = ResIdxRange.first; r < ResIdxRange.second; ++r)
        {
            const auto& ResDesc = GetResourceDesc(r);
            const auto& ResAttr = GetResourceAttribs(r);

            if (ResDesc.ResourceType != SHADER_RESOURCE_TYPE_TEXTURE_SRV || !ResAttr.IsSamplerAssigned())
                continue;

            ISampler* Sampler = nullptr;
            if (ResAttr.IsImmutableSamplerAssigned())
            {
                VERIFY_EXPR(ResAttr.SamplerInd < GetImmutableSamplerCount());

                Sampler = m_ImmutableSamplers[ResAttr.SamplerInd].RawPtr();
            }
            else
            {
                const auto& SampAttr = GetResourceAttribs(ResAttr.SamplerInd);
                if (!SampAttr.IsImmutableSamplerAssigned())
                    continue;

                Sampler = m_ImmutableSamplers[SampAttr.SamplerInd].RawPtr();
            }

            for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                m_StaticResourceCache->SetSampler(ResAttr.CacheOffset + ArrInd, Sampler);
        }
    }

#ifdef DILIGENT_DEVELOPMENT
    GLint MaxUniformBlocks = 0;
    GLint MaxStorageBlock  = 0;
    GLint MaxTextureUnits  = 0;
    GLint MaxImagesUnits   = 0;
    glGetIntegerv(GL_MAX_UNIFORM_BUFFER_BINDINGS, &MaxUniformBlocks);
    glGetIntegerv(GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS, &MaxStorageBlock);
    glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, &MaxTextureUnits);
    glGetIntegerv(GL_MAX_IMAGE_UNITS, &MaxImagesUnits);
    CHECK_GL_ERROR("glGetIntegerv failed");

    DEV_CHECK_ERR(m_FirstBinding[SHADER_RESOURCE_RANGE_CONSTANT_BUFFER] + m_BindingCount[SHADER_RESOURCE_RANGE_CONSTANT_BUFFER] <= static_cast<Uint32>(MaxUniformBlocks), "");
    DEV_CHECK_ERR(m_FirstBinding[SHADER_RESOURCE_RANGE_TEXTURE_SRV] + m_BindingCount[SHADER_RESOURCE_RANGE_TEXTURE_SRV] <= static_cast<Uint32>(MaxTextureUnits), "");
    DEV_CHECK_ERR(m_FirstBinding[SHADER_RESOURCE_RANGE_TEXTURE_UAV] + m_BindingCount[SHADER_RESOURCE_RANGE_TEXTURE_UAV] <= static_cast<Uint32>(MaxImagesUnits), "");
    DEV_CHECK_ERR(m_FirstBinding[SHADER_RESOURCE_RANGE_BUFFER_UAV] + m_BindingCount[SHADER_RESOURCE_RANGE_BUFFER_UAV] <= static_cast<Uint32>(MaxStorageBlock), "");
#endif
}

size_t PipelineResourceSignatureGLImpl::CalculateHash() const
{
    if (m_Desc.NumResources == 0 && m_Desc.NumImmutableSamplers == 0)
        return 0;

    auto Hash = CalculatePipelineResourceSignatureDescHash(m_Desc);
    for (Uint32 i = 0; i < m_Desc.NumResources; ++i)
    {
        const auto& Attr = m_pResourceAttribs[i];
        HashCombine(Hash, Attr.CacheOffset);
    }

    return Hash;
}

PipelineResourceSignatureGLImpl::~PipelineResourceSignatureGLImpl()
{
    Destruct();
}

void PipelineResourceSignatureGLImpl::Destruct()
{
    auto& RawAllocator = GetRawAllocator();

    if (m_ImmutableSamplers != nullptr)
    {
        for (Uint32 s = 0; s < m_Desc.NumImmutableSamplers; ++s)
            m_ImmutableSamplers[s].~SamplerPtr();

        m_ImmutableSamplers = nullptr;
    }

    if (m_StaticResourceLayouts)
    {
        for (auto Idx : m_StaticResStageIndex)
        {
            if (Idx >= 0)
                m_StaticResourceLayouts[Idx].~GLPipelineResourceLayout();
        }
        m_StaticResourceLayouts = nullptr;
    }

    if (m_StaticResourceCache)
    {
        m_StaticResourceCache->Destroy(RawAllocator);
        m_StaticResourceCache = nullptr;
    }

    if (void* pRawMem = m_pResourceAttribs)
    {
        RawAllocator.Free(pRawMem);
        m_pResourceAttribs = nullptr;
    }

    TPipelineResourceSignatureBase::Destruct();
}

void PipelineResourceSignatureGLImpl::ApplyBindings(GLObjectWrappers::GLProgramObj& GLProgram,
                                                    GLContextState&                 State,
                                                    SHADER_TYPE                     Stages) const
{
    VERIFY(GLProgram != 0, "Null GL program");
    State.SetProgram(GLProgram);

    for (Uint32 r = 0; r < GetTotalResourceCount(); ++r)
    {
        const auto& ResDesc = m_Desc.Resources[r];
        const auto& ResAttr = m_pResourceAttribs[r];
        const auto  Range   = PipelineResourceToShaderResourceRange(ResDesc);

        if (Range == SHADER_RESOURCE_RANGE_UNKNOWN)
            continue;

        if ((ResDesc.ShaderStages & Stages) == 0)
            continue;

        //VERIFY_EXPR((Stages & ResDesc.ShaderStages) == ResDesc.ShaderStages);

        static_assert(SHADER_RESOURCE_RANGE_LAST == SHADER_RESOURCE_RANGE_SAMPLER, "AZ TODO");
        switch (Range)
        {
            case SHADER_RESOURCE_RANGE_CONSTANT_BUFFER:
            {
                auto UniformBlockIndex = glGetUniformBlockIndex(GLProgram, ResDesc.Name);
                DEV_CHECK_ERR(UniformBlockIndex != GL_INVALID_INDEX, "Failed to get uniform block index for '", ResDesc.Name, "'");

                for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                {
                    glUniformBlockBinding(GLProgram, UniformBlockIndex + ArrInd, GetFirstUBBinding() + ResAttr.CacheOffset + ArrInd);
                    CHECK_GL_ERROR("glUniformBlockBinding() failed");
                }
                break;
            }
            case SHADER_RESOURCE_RANGE_TEXTURE_SRV:
            {
                auto UniformLocation = glGetUniformLocation(GLProgram, ResDesc.Name);
                DEV_CHECK_ERR(UniformLocation >= 0, "Failed to get uniform location for '", ResDesc.Name, "'");
                for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                {
                    glUniform1i(UniformLocation + ArrInd, GetFirstTextureBinding() + ResAttr.CacheOffset + ArrInd);
                    CHECK_GL_ERROR("Failed to set binding point for sampler uniform '", ResDesc.Name, '\'');
                }
                break;
            }
#if GL_ARB_shader_image_load_store
            case SHADER_RESOURCE_RANGE_TEXTURE_UAV:
            {
                auto UniformLocation = glGetUniformLocation(GLProgram, ResDesc.Name);
                DEV_CHECK_ERR(UniformLocation >= 0, "Failed to get uniform location for '", ResDesc.Name, "'");
                for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                {
                    glUniform1i(UniformLocation + ArrInd, GetFirstImageBinding() + ResAttr.CacheOffset + ArrInd);
                    CHECK_GL_ERROR("Failed to set binding point for image uniform '", ResDesc.Name, '\'');
                }
                break;
            }
#endif
#if GL_ARB_shader_storage_buffer_object
            case SHADER_RESOURCE_RANGE_BUFFER_UAV:
            {
                auto SBIndex = glGetProgramResourceIndex(GLProgram, GL_SHADER_STORAGE_BLOCK, ResDesc.Name);
                DEV_CHECK_ERR(SBIndex != GL_INVALID_INDEX, "Failed to get storage block index for '", ResDesc.Name, "'");

                if (glShaderStorageBlockBinding)
                {
                    for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                    {
                        glShaderStorageBlockBinding(GLProgram, SBIndex + ArrInd, GetFirstSSBBinding() + ResAttr.CacheOffset + ArrInd);
                        CHECK_GL_ERROR("glShaderStorageBlockBinding() failed");
                    }
                }
                else
                {
                    // AZ TODO
                }
                break;
            }
#endif
            default:
                UNEXPECTED("Unsupported shader resource range type.");
        }
    }

    State.SetProgram(GLObjectWrappers::GLProgramObj::Null());
}

void PipelineResourceSignatureGLImpl::CreateShaderResourceBinding(IShaderResourceBinding** ppShaderResourceBinding,
                                                                  bool                     InitStaticResources)
{
    auto* pRenderDeviceGL = GetDevice();
    auto& SRBAllocator    = pRenderDeviceGL->GetSRBAllocator();
    auto  pResBinding     = NEW_RC_OBJ(SRBAllocator, "ShaderResourceBindingGLImpl instance", ShaderResourceBindingGLImpl)(this);
    if (InitStaticResources)
        pResBinding->InitializeStaticResourcesWithSignature(this);
    pResBinding->QueryInterface(IID_ShaderResourceBinding, reinterpret_cast<IObject**>(ppShaderResourceBinding));
}

Uint32 PipelineResourceSignatureGLImpl::GetStaticVariableCount(SHADER_TYPE ShaderType) const
{
    return GetStaticVariableCountImpl(ShaderType, m_StaticResourceLayouts);
}

IShaderResourceVariable* PipelineResourceSignatureGLImpl::GetStaticVariableByName(SHADER_TYPE ShaderType, const Char* Name)
{
    return GetStaticVariableByNameImpl(ShaderType, Name, m_StaticResourceLayouts);
}

IShaderResourceVariable* PipelineResourceSignatureGLImpl::GetStaticVariableByIndex(SHADER_TYPE ShaderType, Uint32 Index)
{
    return GetStaticVariableByIndexImpl(ShaderType, Index, m_StaticResourceLayouts);
}

void PipelineResourceSignatureGLImpl::BindStaticResources(Uint32            ShaderFlags,
                                                          IResourceMapping* pResMapping,
                                                          Uint32            Flags)
{
    BindStaticResourcesImpl(ShaderFlags, pResMapping, Flags, m_StaticResourceLayouts);
}

void PipelineResourceSignatureGLImpl::InitializeStaticSRBResources(GLProgramResourceCache& DstResourceCache) const
{
    // SrcResourceCache contains only static resources.
    // DstResourceCache contains static, mutable and dynamic resources.
    const auto& SrcResourceCache = *m_StaticResourceCache;
    const auto  ResIdxRange      = GetResourceIndexRange(SHADER_RESOURCE_VARIABLE_TYPE_STATIC);

    for (Uint32 r = ResIdxRange.first; r < ResIdxRange.second; ++r)
    {
        const auto& ResDesc = GetResourceDesc(r);
        const auto& ResAttr = GetResourceAttribs(r);
        VERIFY_EXPR(ResDesc.VarType == SHADER_RESOURCE_VARIABLE_TYPE_STATIC);

        if (ResDesc.ResourceType == SHADER_RESOURCE_TYPE_SAMPLER)
            continue; // Skip separate samplers

        static_assert(SHADER_RESOURCE_RANGE_LAST == SHADER_RESOURCE_RANGE_SAMPLER, "AZ TODO");
        switch (PipelineResourceToShaderResourceRange(ResDesc))
        {
            case SHADER_RESOURCE_RANGE_CONSTANT_BUFFER:
                for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                {
                    const auto& SrcCachedRes = SrcResourceCache.GetConstUB(ResAttr.CacheOffset + ArrInd);
                    if (!SrcCachedRes.pBuffer)
                        LOG_ERROR_MESSAGE("No resource is assigned to static shader variable '", GetShaderResourcePrintName(ResDesc, ArrInd), "' in pipeline resource signature '", m_Desc.Name, "'.");

                    DstResourceCache.SetUniformBuffer(ResAttr.CacheOffset + ArrInd, RefCntAutoPtr<BufferGLImpl>{SrcCachedRes.pBuffer});
                }
                break;
            case SHADER_RESOURCE_RANGE_BUFFER_UAV:
                for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                {
                    const auto& SrcCachedRes = SrcResourceCache.GetConstSSBO(ResAttr.CacheOffset + ArrInd);
                    if (!SrcCachedRes.pBufferView)
                        LOG_ERROR_MESSAGE("No resource is assigned to static shader variable '", GetShaderResourcePrintName(ResDesc, ArrInd), "' in pipeline resource signature '", m_Desc.Name, "'.");

                    DstResourceCache.SetSSBO(ResAttr.CacheOffset + ArrInd, RefCntAutoPtr<BufferViewGLImpl>{SrcCachedRes.pBufferView});
                }
                break;
            case SHADER_RESOURCE_RANGE_TEXTURE_SRV:
                for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                {
                    const auto& SrcCachedRes = SrcResourceCache.GetConstTexture(ResAttr.CacheOffset + ArrInd);
                    if (!SrcCachedRes.pView)
                        LOG_ERROR_MESSAGE("No resource is assigned to static shader variable '", GetShaderResourcePrintName(ResDesc, ArrInd), "' in pipeline resource signature '", m_Desc.Name, "'.");

                    DstResourceCache.CopyTexture(ResAttr.CacheOffset + ArrInd, SrcCachedRes);
                }
                break;
            case SHADER_RESOURCE_RANGE_TEXTURE_UAV:
                for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
                {
                    const auto& SrcCachedRes = SrcResourceCache.GetConstImage(ResAttr.CacheOffset + ArrInd);
                    if (!SrcCachedRes.pView)
                        LOG_ERROR_MESSAGE("No resource is assigned to static shader variable '", GetShaderResourcePrintName(ResDesc, ArrInd), "' in pipeline resource signature '", m_Desc.Name, "'.");

                    DstResourceCache.CopyImage(ResAttr.CacheOffset + ArrInd, SrcCachedRes);
                }
                break;
            default:
                UNEXPECTED("Unsupported shader resource range type.");
        }
    }

    // Copy immutable samplers.
    for (Uint32 r = 0; r < m_Desc.NumResources; ++r)
    {
        const auto& ResDesc = GetResourceDesc(r);
        const auto& ResAttr = GetResourceAttribs(r);

        if (ResDesc.ResourceType != SHADER_RESOURCE_TYPE_TEXTURE_SRV ||
            ResDesc.VarType == SHADER_RESOURCE_VARIABLE_TYPE_STATIC)
            continue;

        if (!ResAttr.IsSamplerAssigned())
            continue;

        ISampler* Sampler = nullptr;
        if (ResAttr.IsImmutableSamplerAssigned())
        {
            VERIFY_EXPR(ResAttr.SamplerInd < GetImmutableSamplerCount());

            Sampler = m_ImmutableSamplers[ResAttr.SamplerInd].RawPtr();
        }
        else
        {
            const auto& SampAttr = GetResourceAttribs(ResAttr.SamplerInd);
            if (!SampAttr.IsImmutableSamplerAssigned())
                continue;

            Sampler = m_ImmutableSamplers[SampAttr.SamplerInd].RawPtr();
        }

        for (Uint32 ArrInd = 0; ArrInd < ResDesc.ArraySize; ++ArrInd)
            DstResourceCache.SetSampler(ResAttr.CacheOffset + ArrInd, Sampler);
    }
}

void PipelineResourceSignatureGLImpl::InitSRBResourceCache(GLProgramResourceCache& ResourceCache) const
{
    ResourceCache.Initialize(m_BindingCount[SHADER_RESOURCE_RANGE_CONSTANT_BUFFER],
                             m_BindingCount[SHADER_RESOURCE_RANGE_TEXTURE_SRV],
                             m_BindingCount[SHADER_RESOURCE_RANGE_TEXTURE_UAV],
                             m_BindingCount[SHADER_RESOURCE_RANGE_BUFFER_UAV],
                             GetRawAllocator());
}

bool PipelineResourceSignatureGLImpl::IsCompatibleWith(const PipelineResourceSignatureGLImpl& Other) const
{
    if (this == &Other)
        return true;

    if (GetHash() != Other.GetHash())
        return false;

    if (m_FirstBinding != Other.m_FirstBinding ||
        m_BindingCount != Other.m_BindingCount)
        return false;

    if (!PipelineResourceSignaturesCompatible(GetDesc(), Other.GetDesc()))
        return false;

    const auto ResCount = GetTotalResourceCount();
    VERIFY_EXPR(ResCount == Other.GetTotalResourceCount());
    for (Uint32 r = 0; r < ResCount; ++r)
    {
        if (!ResourcesCompatible(GetResourceAttribs(r), Other.GetResourceAttribs(r)))
            return false;
    }

    return true;
}

} // namespace Diligent
