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
#include <array>
#include <sstream>
#include <d3dcompiler.h>

#include "PipelineStateD3D12Impl.hpp"
#include "ShaderD3D12Impl.hpp"
#include "D3D12TypeConversions.hpp"
#include "RenderDeviceD3D12Impl.hpp"
#include "DXGITypeConversions.hpp"
#include "ShaderResourceBindingD3D12Impl.hpp"
#include "CommandContext.hpp"
#include "EngineMemory.h"
#include "StringTools.hpp"
#include "ShaderVariableD3D12.hpp"
#include "DynamicLinearAllocator.hpp"
#include "DXBCUtils.hpp"
#include "DXCompiler.hpp"
#include "dxc/dxcapi.h"

namespace Diligent
{
namespace
{
#ifdef _MSC_VER
#    pragma warning(push)
#    pragma warning(disable : 4324) //  warning C4324: structure was padded due to alignment specifier
#endif

template <typename InnerStructType, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE SubObjType>
struct alignas(void*) PSS_SubObject
{
    const D3D12_PIPELINE_STATE_SUBOBJECT_TYPE Type{SubObjType};
    InnerStructType                           Obj{};

    PSS_SubObject() noexcept {}

    PSS_SubObject& operator=(const InnerStructType& obj)
    {
        Obj = obj;
        return *this;
    }

    InnerStructType* operator->() { return &Obj; }
    InnerStructType* operator&() { return &Obj; }
    InnerStructType& operator*() { return Obj; }
};

#ifdef _MSC_VER
#    pragma warning(pop)
#endif


class PrimitiveTopology_To_D3D12_PRIMITIVE_TOPOLOGY_TYPE
{
public:
    PrimitiveTopology_To_D3D12_PRIMITIVE_TOPOLOGY_TYPE()
    {
        // clang-format off
        m_Map[PRIMITIVE_TOPOLOGY_UNDEFINED]      = D3D12_PRIMITIVE_TOPOLOGY_TYPE_UNDEFINED;
        m_Map[PRIMITIVE_TOPOLOGY_TRIANGLE_LIST]  = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        m_Map[PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP] = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        m_Map[PRIMITIVE_TOPOLOGY_POINT_LIST]     = D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT;
        m_Map[PRIMITIVE_TOPOLOGY_LINE_LIST]      = D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE;
        m_Map[PRIMITIVE_TOPOLOGY_LINE_STRIP]     = D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE;
        // clang-format on
        for (int t = static_cast<int>(PRIMITIVE_TOPOLOGY_1_CONTROL_POINT_PATCHLIST); t < static_cast<int>(PRIMITIVE_TOPOLOGY_NUM_TOPOLOGIES); ++t)
            m_Map[t] = D3D12_PRIMITIVE_TOPOLOGY_TYPE_PATCH;
    }

    D3D12_PRIMITIVE_TOPOLOGY_TYPE operator[](PRIMITIVE_TOPOLOGY Topology) const
    {
        return m_Map[static_cast<int>(Topology)];
    }

private:
    std::array<D3D12_PRIMITIVE_TOPOLOGY_TYPE, PRIMITIVE_TOPOLOGY_NUM_TOPOLOGIES> m_Map;
};

template <typename TShaderStages>
void BuildRTPipelineDescription(const RayTracingPipelineStateCreateInfo& CreateInfo,
                                std::vector<D3D12_STATE_SUBOBJECT>&      Subobjects,
                                DynamicLinearAllocator&                  TempPool,
                                TShaderStages&                           ShaderStages) noexcept(false)
{
#define LOG_PSO_ERROR_AND_THROW(...) LOG_ERROR_AND_THROW("Description of ray tracing PSO '", (CreateInfo.PSODesc.Name ? CreateInfo.PSODesc.Name : ""), "' is invalid: ", ##__VA_ARGS__)

    Uint32 UnnamedExportIndex = 0;

    std::unordered_map<IShader*, LPCWSTR> UniqueShaders;

    std::array<typename TShaderStages::value_type*, MAX_SHADERS_IN_PIPELINE> StagesPtr     = {};
    std::array<Uint32, MAX_SHADERS_IN_PIPELINE>                              ShaderIndices = {};

    // prepare
    for (auto& Stage : ShaderStages)
    {
        const auto Idx = GetShaderTypePipelineIndex(Stage.Type, PIPELINE_TYPE_RAY_TRACING);
        VERIFY_EXPR(StagesPtr[Idx] == nullptr);
        StagesPtr[Idx] = &Stage;
    }

    const auto AddDxilLib = [&](IShader* pShader, const char* Name) -> LPCWSTR {
        if (pShader == nullptr)
            return nullptr;

        auto it_inserted = UniqueShaders.emplace(pShader, nullptr);
        if (it_inserted.second)
        {
            const auto  StageIdx    = GetShaderTypePipelineIndex(pShader->GetDesc().ShaderType, PIPELINE_TYPE_RAY_TRACING);
            const auto& Stage       = *StagesPtr[StageIdx];
            auto&       ShaderIndex = ShaderIndices[StageIdx];

            // shaders must be in same order as in ExtractShaders()
            VERIFY_EXPR(Stage.Shaders[ShaderIndex] == pShader);

            auto&       LibDesc      = *TempPool.Construct<D3D12_DXIL_LIBRARY_DESC>();
            auto&       ExportDesc   = *TempPool.Construct<D3D12_EXPORT_DESC>();
            const auto* pShaderD3D12 = ValidatedCast<ShaderD3D12Impl>(pShader);
            const auto& pBlob        = Stage.ByteCodes[ShaderIndex];
            ++ShaderIndex;

            LibDesc.DXILLibrary.BytecodeLength  = pBlob->GetBufferSize();
            LibDesc.DXILLibrary.pShaderBytecode = pBlob->GetBufferPointer();
            LibDesc.NumExports                  = 1;
            LibDesc.pExports                    = &ExportDesc;

            ExportDesc.Flags          = D3D12_EXPORT_FLAG_NONE;
            ExportDesc.ExportToRename = TempPool.CopyWString(pShaderD3D12->GetEntryPoint());

            if (Name != nullptr)
                ExportDesc.Name = TempPool.CopyWString(Name);
            else
            {
                std::stringstream ss;
                ss << "__Shader_" << std::setfill('0') << std::setw(4) << UnnamedExportIndex++;
                ExportDesc.Name = TempPool.CopyWString(ss.str());
            }

            Subobjects.push_back({D3D12_STATE_SUBOBJECT_TYPE_DXIL_LIBRARY, &LibDesc});

            it_inserted.first->second = ExportDesc.Name;
            return ExportDesc.Name;
        }
        else
            return it_inserted.first->second;
    };

    for (Uint32 i = 0; i < CreateInfo.GeneralShaderCount; ++i)
    {
        const auto& GeneralShader = CreateInfo.pGeneralShaders[i];
        AddDxilLib(GeneralShader.pShader, GeneralShader.Name);
    }

    for (Uint32 i = 0; i < CreateInfo.TriangleHitShaderCount; ++i)
    {
        const auto& TriHitShader = CreateInfo.pTriangleHitShaders[i];

        auto& HitGroupDesc                    = *TempPool.Construct<D3D12_HIT_GROUP_DESC>();
        HitGroupDesc.HitGroupExport           = TempPool.CopyWString(TriHitShader.Name);
        HitGroupDesc.Type                     = D3D12_HIT_GROUP_TYPE_TRIANGLES;
        HitGroupDesc.ClosestHitShaderImport   = AddDxilLib(TriHitShader.pClosestHitShader, nullptr);
        HitGroupDesc.AnyHitShaderImport       = AddDxilLib(TriHitShader.pAnyHitShader, nullptr);
        HitGroupDesc.IntersectionShaderImport = nullptr;

        Subobjects.push_back({D3D12_STATE_SUBOBJECT_TYPE_HIT_GROUP, &HitGroupDesc});
    }

    for (Uint32 i = 0; i < CreateInfo.ProceduralHitShaderCount; ++i)
    {
        const auto& ProcHitShader = CreateInfo.pProceduralHitShaders[i];

        auto& HitGroupDesc                    = *TempPool.Construct<D3D12_HIT_GROUP_DESC>();
        HitGroupDesc.HitGroupExport           = TempPool.CopyWString(ProcHitShader.Name);
        HitGroupDesc.Type                     = D3D12_HIT_GROUP_TYPE_PROCEDURAL_PRIMITIVE;
        HitGroupDesc.ClosestHitShaderImport   = AddDxilLib(ProcHitShader.pClosestHitShader, nullptr);
        HitGroupDesc.AnyHitShaderImport       = AddDxilLib(ProcHitShader.pAnyHitShader, nullptr);
        HitGroupDesc.IntersectionShaderImport = AddDxilLib(ProcHitShader.pIntersectionShader, nullptr);

        Subobjects.push_back({D3D12_STATE_SUBOBJECT_TYPE_HIT_GROUP, &HitGroupDesc});
    }

    constexpr Uint32 DefaultPayloadSize = sizeof(float) * 8;

    auto& PipelineConfig = *TempPool.Construct<D3D12_RAYTRACING_PIPELINE_CONFIG>();

    PipelineConfig.MaxTraceRecursionDepth = CreateInfo.RayTracingPipeline.MaxRecursionDepth;
    Subobjects.push_back({D3D12_STATE_SUBOBJECT_TYPE_RAYTRACING_PIPELINE_CONFIG, &PipelineConfig});

    auto& ShaderConfig                   = *TempPool.Construct<D3D12_RAYTRACING_SHADER_CONFIG>();
    ShaderConfig.MaxAttributeSizeInBytes = CreateInfo.MaxAttributeSize == 0 ? D3D12_RAYTRACING_MAX_ATTRIBUTE_SIZE_IN_BYTES : CreateInfo.MaxAttributeSize;
    ShaderConfig.MaxPayloadSizeInBytes   = CreateInfo.MaxPayloadSize == 0 ? DefaultPayloadSize : CreateInfo.MaxPayloadSize;
    Subobjects.push_back({D3D12_STATE_SUBOBJECT_TYPE_RAYTRACING_SHADER_CONFIG, &ShaderConfig});
#undef LOG_PSO_ERROR_AND_THROW
}

template <typename TNameToGroupIndexMap>
void GetShaderIdentifiers(ID3D12DeviceChild*                       pSO,
                          const RayTracingPipelineStateCreateInfo& CreateInfo,
                          const TNameToGroupIndexMap&              NameToGroupIndex,
                          Uint8*                                   ShaderData,
                          Uint32                                   ShaderIdentifierSize)
{
    CComPtr<ID3D12StateObjectProperties> pStateObjectProperties;

    auto hr = pSO->QueryInterface(IID_PPV_ARGS(&pStateObjectProperties));
    if (FAILED(hr))
        LOG_ERROR_AND_THROW("Failed to get state object properties");

    for (Uint32 i = 0; i < CreateInfo.GeneralShaderCount; ++i)
    {
        const auto& GeneralShader = CreateInfo.pGeneralShaders[i];

        auto iter = NameToGroupIndex.find(GeneralShader.Name);
        VERIFY(iter != NameToGroupIndex.end(),
               "Can't find general shader '", GeneralShader.Name,
               "'. This looks to be a bug as NameToGroupIndex is initialized by "
               "CopyRTShaderGroupNames() that processes the same general shaders.");

        const auto* ShaderID = pStateObjectProperties->GetShaderIdentifier(WidenString(GeneralShader.Name).c_str());
        if (ShaderID == nullptr)
            LOG_ERROR_AND_THROW("Failed to get shader identifier for general shader group '", GeneralShader.Name, "'");

        std::memcpy(&ShaderData[ShaderIdentifierSize * iter->second], ShaderID, ShaderIdentifierSize);
    }

    for (Uint32 i = 0; i < CreateInfo.TriangleHitShaderCount; ++i)
    {
        const auto& TriHitShader = CreateInfo.pTriangleHitShaders[i];

        auto iter = NameToGroupIndex.find(TriHitShader.Name);
        VERIFY(iter != NameToGroupIndex.end(),
               "Can't find triangle hit group '", TriHitShader.Name,
               "'. This looks to be a bug as NameToGroupIndex is initialized by "
               "CopyRTShaderGroupNames() that processes the same hit groups.");

        const auto* ShaderID = pStateObjectProperties->GetShaderIdentifier(WidenString(TriHitShader.Name).c_str());
        if (ShaderID == nullptr)
            LOG_ERROR_AND_THROW("Failed to get shader identifier for triangle hit group '", TriHitShader.Name, "'");

        std::memcpy(&ShaderData[ShaderIdentifierSize * iter->second], ShaderID, ShaderIdentifierSize);
    }

    for (Uint32 i = 0; i < CreateInfo.ProceduralHitShaderCount; ++i)
    {
        const auto& ProcHitShader = CreateInfo.pProceduralHitShaders[i];

        auto iter = NameToGroupIndex.find(ProcHitShader.Name);
        VERIFY(iter != NameToGroupIndex.end(),
               "Can't find procedural hit group '", ProcHitShader.Name,
               "'. This looks to be a bug as NameToGroupIndex is initialized by "
               "CopyRTShaderGroupNames() that processes the same hit groups.");

        const auto* ShaderID = pStateObjectProperties->GetShaderIdentifier(WidenString(ProcHitShader.Name).c_str());
        if (ShaderID == nullptr)
            LOG_ERROR_AND_THROW("Failed to get shader identifier for procedural hit shader group '", ProcHitShader.Name, "'");

        std::memcpy(&ShaderData[ShaderIdentifierSize * iter->second], ShaderID, ShaderIdentifierSize);
    }
}

void GetShaderResourceTypeAndFlags(const D3DShaderResourceAttribs& Attribs,
                                   SHADER_RESOURCE_TYPE&           OutType,
                                   PIPELINE_RESOURCE_FLAGS&        OutFlags)
{
    OutFlags = PIPELINE_RESOURCE_FLAG_UNKNOWN;

    switch (static_cast<Uint32>(Attribs.GetInputType()))
    {
        case D3D_SIT_CBUFFER:
            OutType = SHADER_RESOURCE_TYPE_CONSTANT_BUFFER;
            break;
        case D3D_SIT_TBUFFER:
            UNSUPPORTED("TBuffers are not supported");
            OutType = SHADER_RESOURCE_TYPE_TEXTURE_SRV;
            break;
        case D3D_SIT_TEXTURE:
            if (Attribs.GetSRVDimension() == D3D_SRV_DIMENSION_BUFFER)
            {
                OutType  = SHADER_RESOURCE_TYPE_BUFFER_SRV;
                OutFlags = PIPELINE_RESOURCE_FLAG_FORMATTED_BUFFER;
            }
            else
                OutType = SHADER_RESOURCE_TYPE_TEXTURE_SRV;
            break;
        case D3D_SIT_SAMPLER:
            OutType = SHADER_RESOURCE_TYPE_SAMPLER;
            break;
        case D3D_SIT_UAV_RWTYPED:
            if (Attribs.GetSRVDimension() == D3D_SRV_DIMENSION_BUFFER)
            {
                OutType  = SHADER_RESOURCE_TYPE_BUFFER_UAV;
                OutFlags = PIPELINE_RESOURCE_FLAG_FORMATTED_BUFFER;
            }
            else
                OutType = SHADER_RESOURCE_TYPE_TEXTURE_UAV;
            break;
        case D3D_SIT_STRUCTURED:
        case D3D_SIT_BYTEADDRESS:
            OutType = SHADER_RESOURCE_TYPE_BUFFER_SRV;
            break;
        case D3D_SIT_UAV_RWSTRUCTURED:
        case D3D_SIT_UAV_RWBYTEADDRESS:
        case D3D_SIT_UAV_APPEND_STRUCTURED:
        case D3D_SIT_UAV_CONSUME_STRUCTURED:
        case D3D_SIT_UAV_RWSTRUCTURED_WITH_COUNTER:
            OutType = SHADER_RESOURCE_TYPE_BUFFER_UAV;
            break;
        case D3D_SIT_RTACCELERATIONSTRUCTURE:
            OutType = SHADER_RESOURCE_TYPE_ACCEL_STRUCT;
            break;
        default:
            UNEXPECTED("Unknown HLSL resource type");
            OutType = SHADER_RESOURCE_TYPE_UNKNOWN;
            break;
    }
}

void VerifyResourceMerge(const D3DShaderResourceAttribs& ExistingRes,
                         const D3DShaderResourceAttribs& NewResAttribs)
{
    DEV_CHECK_ERR(ExistingRes.GetInputType() == NewResAttribs.GetInputType(),
                  "Shader variable '", NewResAttribs.Name,
                  "' exists in multiple shaders from the same shader stage, but its input type is not consistent between "
                  "shaders. All variables with the same name from the same shader stage must have the same input type.");

    DEV_CHECK_ERR(ExistingRes.GetSRVDimension() == NewResAttribs.GetSRVDimension(),
                  "Shader variable '", NewResAttribs.Name,
                  "' exists in multiple shaders from the same shader stage, but its SRV dimension is not consistent between "
                  "shaders. All variables with the same name from the same shader stage must have the same SRV dimension.");

    DEV_CHECK_ERR(ExistingRes.BindCount == NewResAttribs.BindCount,
                  "Shader variable '", NewResAttribs.Name,
                  "' exists in multiple shaders from the same shader stage, but its array size is not consistent between "
                  "shaders. All variables with the same name from the same shader stage must have the same array size.");
}
} // namespace


PipelineStateD3D12Impl::ShaderStageInfo::ShaderStageInfo(ShaderD3D12Impl* _pShader) :
    Type{_pShader->GetDesc().ShaderType},
    Shaders{_pShader},
    ByteCodes{_pShader->GetShaderByteCode()}
{
}

void PipelineStateD3D12Impl::ShaderStageInfo::Append(ShaderD3D12Impl* pShader)
{
    VERIFY_EXPR(pShader != nullptr);
    VERIFY(std::find(Shaders.begin(), Shaders.end(), pShader) == Shaders.end(),
           "Shader '", pShader->GetDesc().Name, "' already exists in the stage. Shaders must be deduplicated.");

    const auto NewShaderType = pShader->GetDesc().ShaderType;
    if (Type == SHADER_TYPE_UNKNOWN)
    {
        VERIFY_EXPR(Shaders.empty());
        Type = NewShaderType;
    }
    else
    {
        VERIFY(Type == NewShaderType, "The type (", GetShaderTypeLiteralName(NewShaderType),
               ") of shader '", pShader->GetDesc().Name, "' being added to the stage is incosistent with the stage type (",
               GetShaderTypeLiteralName(Type), ").");
    }

    Shaders.push_back(pShader);
    ByteCodes.push_back(pShader->GetShaderByteCode());
}

size_t PipelineStateD3D12Impl::ShaderStageInfo::Count() const
{
    VERIFY_EXPR(Shaders.size() == ByteCodes.size());
    return Shaders.size();
}


void PipelineStateD3D12Impl::CreateDefaultResourceSignature(const PipelineStateCreateInfo& CreateInfo,
                                                            TShaderStages&                 ShaderStages,
                                                            LocalRootSignatureD3D12*       pLocalRootSig,
                                                            IPipelineResourceSignature**   ppImplicitSignature)
{
    struct UniqueResource
    {
        D3DShaderResourceAttribs const* Attribs   = nullptr;
        Uint32                          DescIndex = ~0u;
    };
    using ResourceNameToIndex_t = std::unordered_map<HashMapStringKey, UniqueResource, HashMapStringKey::Hasher>;

    std::vector<PipelineResourceDesc> Resources;
    ResourceNameToIndex_t             UniqueNames;
    const char*                       pCombinedSamplerSuffix = nullptr;
    const auto&                       LayoutDesc             = CreateInfo.PSODesc.ResourceLayout;

    for (auto& Stage : ShaderStages)
    {
        UniqueNames.clear();
        for (auto* pShader : Stage.Shaders)
        {
            const auto DefaultVarType  = LayoutDesc.DefaultVariableType;
            auto&      ShaderResources = *pShader->GetShaderResources();
            const auto HandleResource  = [&](const D3DShaderResourceAttribs& Res, Uint32) //
            {
                if (pLocalRootSig != nullptr && pLocalRootSig->IsShaderRecord(Res))
                    return;

                auto IterAndAssigned = UniqueNames.emplace(HashMapStringKey{Res.Name}, UniqueResource{&Res, static_cast<Uint32>(Resources.size())});
                if (IterAndAssigned.second)
                {
                    SHADER_RESOURCE_TYPE    Type;
                    PIPELINE_RESOURCE_FLAGS Flags;
                    GetShaderResourceTypeAndFlags(Res, Type, Flags);

                    if (Res.BindCount == 0)
                    {
                        LOG_ERROR_AND_THROW("Is shader '", pShader->GetDesc().Name, "' resource '", Res.Name, "' uses runtime sized array, ",
                                            "you must explicitlly set resource signature to specify array size");
                    }

                    Resources.emplace_back(Stage.Type, Res.Name, Res.BindCount, Type, DefaultVarType, Flags);
                }
                else
                {
                    VerifyResourceMerge(*IterAndAssigned.first->second.Attribs, Res);
                }
            };

            ShaderResources.ProcessResources(HandleResource, HandleResource, HandleResource, HandleResource, HandleResource, HandleResource, HandleResource);

            // merge combined sampler suffixes
            if (ShaderResources.IsUsingCombinedTextureSamplers() && ShaderResources.GetNumSamplers() > 0)
            {
                if (pCombinedSamplerSuffix != nullptr)
                {
                    if (strcmp(pCombinedSamplerSuffix, ShaderResources.GetCombinedSamplerSuffix()) != 0)
                        LOG_ERROR_AND_THROW("CombinedSamplerSuffix is not compatible between shaders");
                }
                else
                {
                    pCombinedSamplerSuffix = ShaderResources.GetCombinedSamplerSuffix();
                }
            }

            for (Uint32 i = 0; i < LayoutDesc.NumVariables; ++i)
            {
                const auto& Var = LayoutDesc.Variables[i];
                if (Var.ShaderStages & Stage.Type)
                {
                    auto Iter = UniqueNames.find(HashMapStringKey{Var.Name});
                    if (Iter != UniqueNames.end())
                    {
                        auto& Res   = Resources[Iter->second.DescIndex];
                        Res.VarType = Var.Type;

                        // apply new variable type to sampler too
                        if (ShaderResources.IsUsingCombinedTextureSamplers() && Res.ResourceType == SHADER_RESOURCE_TYPE_TEXTURE_SRV)
                        {
                            String SampName = String{Var.Name} + ShaderResources.GetCombinedSamplerSuffix();
                            auto   SampIter = UniqueNames.find(HashMapStringKey{SampName.c_str()});
                            if (SampIter != UniqueNames.end())
                                Resources[SampIter->second.DescIndex].VarType = Var.Type;
                        }
                    }
                }
            }
        }
    }

    if (Resources.size())
    {
        PipelineResourceSignatureCreateInfo ResSignCI = {};

        ResSignCI.Desc.Resources                  = Resources.data();
        ResSignCI.Desc.NumResources               = static_cast<Uint32>(Resources.size());
        ResSignCI.Desc.ImmutableSamplers          = LayoutDesc.ImmutableSamplers;
        ResSignCI.Desc.NumImmutableSamplers       = LayoutDesc.NumImmutableSamplers;
        ResSignCI.Desc.BindingIndex               = 0;
        ResSignCI.Desc.SRBAllocationGranularity   = CreateInfo.PSODesc.SRBAllocationGranularity;
        ResSignCI.Desc.UseCombinedTextureSamplers = pCombinedSamplerSuffix != nullptr;
        ResSignCI.Desc.CombinedSamplerSuffix      = pCombinedSamplerSuffix;

        GetDevice()->CreatePipelineResourceSignature(ResSignCI, ppImplicitSignature, true);

        if (*ppImplicitSignature == nullptr)
            LOG_ERROR_AND_THROW("Failed to create resource signature for pipeline state");
    }
}

PipelineStateD3D12Impl::ResourceInfo PipelineStateD3D12Impl::GetResourceInfo(const char* Name, SHADER_TYPE Stage) const
{
    ResourceInfo Info;
    for (Uint32 sign = 0, SignCount = GetSignatureCount(); sign < SignCount && !Info; ++sign)
    {
        auto* const pSignature = GetSignature(sign);
        if (pSignature == nullptr)
            continue;

        for (Uint32 r = 0, ResCount = pSignature->GetTotalResourceCount(); r < ResCount; ++r)
        {
            const auto& ResDesc = pSignature->GetResourceDesc(r);

            if ((ResDesc.ShaderStages & Stage) != 0 && strcmp(ResDesc.Name, Name) == 0)
            {
                Info.Signature = pSignature;
                Info.ResDesc   = &ResDesc;
                break;
            }
        }
    }
    return Info;
}

void PipelineStateD3D12Impl::InitRootSignature(const PipelineStateCreateInfo& CreateInfo,
                                               TShaderStages&                 ShaderStages,
                                               LocalRootSignatureD3D12*       pLocalRootSig)
{
    const Uint32                              SignatureCount = CreateInfo.ResourceSignaturesCount;
    RefCntAutoPtr<IPipelineResourceSignature> pImplicitSignature;

    if (SignatureCount == 0 || CreateInfo.ppResourceSignatures == nullptr)
    {
        CreateDefaultResourceSignature(CreateInfo, ShaderStages, pLocalRootSig, &pImplicitSignature);

        if (pImplicitSignature != nullptr)
        {
            VERIFY_EXPR(pImplicitSignature->GetDesc().BindingIndex == 0);
            m_Signatures[0]  = ValidatedCast<PipelineResourceSignatureD3D12Impl>(pImplicitSignature.RawPtr());
            m_SignatureCount = 1;
        }
    }
    else
    {
        PipelineResourceSignatureD3D12Impl::CopyResourceSignatures(CreateInfo.PSODesc.PipelineType, SignatureCount, CreateInfo.ppResourceSignatures, m_Signatures, m_SignatureCount);
    }

    m_RootSig = GetDevice()->GetRootSignatureCache().GetRootSig(m_Signatures.data(), m_SignatureCount);
    if (!m_RootSig)
        LOG_ERROR_AND_THROW("Failed to create root signature");

    if (pLocalRootSig != nullptr && pLocalRootSig->IsDefined())
    {
        if (!pLocalRootSig->Create(GetDevice()->GetD3D12Device(), m_RootSig->GetTotalSpaces()))
            LOG_ERROR_AND_THROW("Failed to create local root signature");
    }

    // Verify that pipeline layout is compatible with shader resources and
    // remap resource bindings.
    auto* compiler = GetDevice()->GetDxCompiler();

    for (size_t s = 0; s < ShaderStages.size(); ++s)
    {
        const auto& Shaders           = ShaderStages[s].Shaders;
        auto&       ByteCodes         = ShaderStages[s].ByteCodes;
        const auto  ShaderType        = ShaderStages[s].Type;
        bool        HasImtblSampArray = false;

        ResourceBinding::TMap ResourceMap;
        for (Uint32 Sig = 0, SigCount = GetSignatureCount(); Sig < SigCount; ++Sig)
        {
            auto* pSignature = GetSignature(Sig);
            if (pSignature != nullptr)
            {
                const Uint32 FirstSpace = m_RootSig->GetFirstRegisterSpace(Sig);

                for (Uint32 r = 0, ResCount = pSignature->GetTotalResourceCount(); r < ResCount; ++r)
                {
                    const auto& ResDesc = pSignature->GetResourceDesc(r);
                    const auto& Attribs = pSignature->GetResourceAttribs(r);

                    if (ResDesc.ShaderStages & ShaderType)
                    {
                        auto IsUnique = ResourceMap.emplace(HashMapStringKey{ResDesc.Name}, ResourceBinding::BindInfo{Attribs.Register, Attribs.Space + FirstSpace, ResDesc.ArraySize}).second;
                        VERIFY(IsUnique, "resource name must be unique");
                    }
                }

                for (Uint32 samp = 0, SampCount = pSignature->GetImmutableSamplerCount(); samp < SampCount; ++samp)
                {
                    const auto&               ImtblSam = pSignature->GetImmutableSamplerDesc(samp);
                    const auto&               SampAttr = pSignature->GetImmutableSamplerAttribs(samp);
                    ResourceBinding::BindInfo BindInfo{SampAttr.ShaderRegister, SampAttr.RegisterSpace + FirstSpace, SampAttr.ArraySize};

                    if (ImtblSam.ShaderStages & ShaderType)
                    {
                        HasImtblSampArray = HasImtblSampArray || (SampAttr.ArraySize > 1);

                        String SampName{ImtblSam.SamplerOrTextureName};
                        if (pSignature->IsUsingCombinedSamplers())
                            SampName += pSignature->GetCombinedSamplerSuffix();

                        auto it_inserted = ResourceMap.emplace(HashMapStringKey{SampName}, BindInfo);
#ifdef DILIGENT_DEBUG
                        if (!it_inserted.second)
                        {
                            const auto& ExistingBindInfo = it_inserted.first->second;
                            VERIFY(ExistingBindInfo.BindPoint == BindInfo.BindPoint,
                                   "Bind point defined by the immutable sampler attribs is inconsistent with the bind point defined by the sampler resource.");
                            VERIFY(ExistingBindInfo.Space == BindInfo.Space,
                                   "Register space defined by the immutable sampler attribs is inconsistent with the bind point defined by the sampler resource.");
                        }
#endif
                    }
                }
            }
        }

        if (pLocalRootSig != nullptr && pLocalRootSig->IsDefined())
        {
            bool IsUnique = ResourceMap.emplace(HashMapStringKey{pLocalRootSig->GetName()}, ResourceBinding::BindInfo{pLocalRootSig->GetShaderRegister(), pLocalRootSig->GetRegisterSpace(), 1}).second;
            if (!IsUnique)
                LOG_ERROR_AND_THROW("Shader record constant buffer is already exist in resource signature");
        }

        for (size_t i = 0; i < Shaders.size(); ++i)
        {
            auto*             pShader   = Shaders[i];
            auto&             pBytecode = ByteCodes[i];
            CComPtr<ID3DBlob> pBlob;
            Uint32            VerMajor, VerMinor;

            pShader->GetShaderResources()->GetShaderModel(VerMajor, VerMinor);
            const bool IsSM51orAbove = ((VerMajor == 5 && VerMinor >= 1) || VerMajor >= 6);

            if (HasImtblSampArray && IsSM51orAbove)
            {
                LOG_ERROR_AND_THROW("One of resource signatures uses immutable sampler array that is not allowed in shader model 5.1 and above.");
            }

            if (m_RootSig->GetTotalSpaces() > 1 && !IsSM51orAbove)
            {
                LOG_ERROR_AND_THROW("Shader '", pShader->GetDesc().Name,
                                    "' is compiled using SM5.0 or below that only supports single register space. "
                                    "Compile the shader using SM5.1+ or change the resource layout to use only one space.");
            }

            if (IsDXILBytecode(pBytecode->GetBufferPointer(), pBytecode->GetBufferSize()))
            {
                if (!compiler)
                    LOG_ERROR_AND_THROW("DXC compiler is not exists, can not remap resource bindings");

                if (!compiler->RemapResourceBindings(ResourceMap, reinterpret_cast<IDxcBlob*>(pBytecode.p), reinterpret_cast<IDxcBlob**>(&pBlob)))
                    LOG_ERROR_AND_THROW("Failed to remap resource bindings in shader '", pShader->GetDesc().Name, "'.");
            }
            else
            {
                D3DCreateBlob(pBytecode->GetBufferSize(), &pBlob);
                memcpy(pBlob->GetBufferPointer(), pBytecode->GetBufferPointer(), pBytecode->GetBufferSize());

                if (!DXBCUtils::RemapResourceBindings(ResourceMap, pBlob->GetBufferPointer(), pBlob->GetBufferSize()))
                    LOG_ERROR_AND_THROW("Failed to remap resource bindings in shader '", pShader->GetDesc().Name, "'.");
            }
            pBytecode = pBlob;

#ifdef DILIGENT_DEVELOPMENT
            const auto& pShaderResources = pShader->GetShaderResources();
            m_ShaderResources.emplace_back(pShaderResources);

            // Check compatibility between shader resources and resource signature.
            const auto HandleResource = [&](const D3DShaderResourceAttribs& Attribs, Uint32) //
            {
                if (pLocalRootSig != nullptr && pLocalRootSig->IsShaderRecord(Attribs))
                    return;

                if (Attribs.GetInputType() == D3D_SIT_SAMPLER)
                    return;

                auto Info = GetResourceInfo(Attribs.Name, ShaderType);
                if (!Info)
                {
                    LOG_ERROR_AND_THROW("Shader '", pShader->GetDesc().Name, "' contains resource with name '", Attribs.Name,
                                        "' that is not present in any pipeline resource signature that is used to create pipeline state '",
                                        m_Desc.Name, "'.");
                }

                SHADER_RESOURCE_TYPE    Type;
                PIPELINE_RESOURCE_FLAGS Flags;
                GetShaderResourceTypeAndFlags(Attribs, Type, Flags);
                if (Type != Info.ResDesc->ResourceType)
                {
                    LOG_ERROR_AND_THROW("Shader '", pShader->GetDesc().Name, "' contains resource with name '", Attribs.Name,
                                        "' and type '", GetShaderResourceTypeLiteralName(Type), "' that is not compatible with type '",
                                        GetShaderResourceTypeLiteralName(Info.ResDesc->ResourceType), "' in pipeline resource signature '", Info.Signature->GetDesc().Name, "'.");
                }

                if ((Flags & PIPELINE_RESOURCE_FLAG_FORMATTED_BUFFER) != (Info.ResDesc->Flags & PIPELINE_RESOURCE_FLAG_FORMATTED_BUFFER))
                {
                    LOG_ERROR_AND_THROW("Shader '", pShader->GetDesc().Name, "' contains resource '", Attribs.Name,
                                        "' that is", ((Flags & PIPELINE_RESOURCE_FLAG_FORMATTED_BUFFER) ? "" : " not"),
                                        " labeled as formatted buffer, while the same resource specified by the pipeline resource signature '",
                                        Info.Signature->GetDesc().Name, "' is", ((Info.ResDesc->Flags & PIPELINE_RESOURCE_FLAG_FORMATTED_BUFFER) ? "" : " not"),
                                        " labeled as such.");
                }

                if (Attribs.BindCount == 0)
                {
                    if ((Info.ResDesc->Flags & PIPELINE_RESOURCE_FLAG_RUNTIME_ARRAY) != 0)
                    {
                        LOG_ERROR_AND_THROW("Shader '", pShader->GetDesc().Name, "' contains resource with name '", Attribs.Name,
                                            "' that is runtime-sized array, but in resource signature '", Info.Signature->GetDesc().Name,
                                            "' resource defined without PIPELINE_RESOURCE_FLAG_RUNTIME_ARRAY flag.");
                    }
                }
                else
                {
                    if (Info.ResDesc->ArraySize < Attribs.BindCount)
                    {
                        LOG_ERROR_AND_THROW("Shader '", pShader->GetDesc().Name, "' contains resource '", Attribs.Name,
                                            "' whose array size (", Attribs.BindCount, ") is greater than the array size (",
                                            Info.ResDesc->ArraySize, ") specified by the pipeline resource signature '", Info.Signature->GetDesc().Name, "'.");
                    }

                    if (Info.ResDesc->Flags & PIPELINE_RESOURCE_FLAG_RUNTIME_ARRAY)
                    {
                        LOG_WARNING_MESSAGE("Shader '", pShader->GetDesc().Name, "' contains resource with name '", Attribs.Name,
                                            "' that defined in resource signature '", Info.Signature->GetDesc().Name,
                                            "' with flag PIPELINE_RESOURCE_FLAG_RUNTIME_ARRAY, but resource is not a runtime-sized array.");
                    }
                }
            };
            pShaderResources->ProcessResources(HandleResource, HandleResource, HandleResource, HandleResource, HandleResource, HandleResource, HandleResource);
#endif
        }
    }
}

template <typename PSOCreateInfoType>
void PipelineStateD3D12Impl::InitInternalObjects(const PSOCreateInfoType& CreateInfo,
                                                 TShaderStages&           ShaderStages,
                                                 LocalRootSignatureD3D12* pLocalRootSig)
{
    ExtractShaders<ShaderD3D12Impl>(CreateInfo, ShaderStages);

    FixedLinearAllocator MemPool{GetRawAllocator()};

    ReserveSpaceForPipelineDesc(CreateInfo, MemPool);

    MemPool.Reserve();

    InitializePipelineDesc(CreateInfo, MemPool);

    // It is important to construct all objects before initializing them because if an exception is thrown,
    // destructors will be called for all objects

    InitRootSignature(CreateInfo, ShaderStages, pLocalRootSig);
}


PipelineStateD3D12Impl::PipelineStateD3D12Impl(IReferenceCounters*                    pRefCounters,
                                               RenderDeviceD3D12Impl*                 pDeviceD3D12,
                                               const GraphicsPipelineStateCreateInfo& CreateInfo) :
    TPipelineStateBase{pRefCounters, pDeviceD3D12, CreateInfo}
{
    try
    {
        TShaderStages ShaderStages;
        InitInternalObjects(CreateInfo, ShaderStages);

        auto* pd3d12Device = pDeviceD3D12->GetD3D12Device();
        if (m_Desc.PipelineType == PIPELINE_TYPE_GRAPHICS)
        {
            const auto& GraphicsPipeline = GetGraphicsPipelineDesc();

            D3D12_GRAPHICS_PIPELINE_STATE_DESC d3d12PSODesc = {};

            for (const auto& Stage : ShaderStages)
            {
                VERIFY_EXPR(Stage.Count() == 1);
                const auto& pByteCode = Stage.ByteCodes[0];

                D3D12_SHADER_BYTECODE* pd3d12ShaderBytecode = nullptr;
                switch (Stage.Type)
                {
                    // clang-format off
                    case SHADER_TYPE_VERTEX:   pd3d12ShaderBytecode = &d3d12PSODesc.VS; break;
                    case SHADER_TYPE_PIXEL:    pd3d12ShaderBytecode = &d3d12PSODesc.PS; break;
                    case SHADER_TYPE_GEOMETRY: pd3d12ShaderBytecode = &d3d12PSODesc.GS; break;
                    case SHADER_TYPE_HULL:     pd3d12ShaderBytecode = &d3d12PSODesc.HS; break;
                    case SHADER_TYPE_DOMAIN:   pd3d12ShaderBytecode = &d3d12PSODesc.DS; break;
                    // clang-format on
                    default: UNEXPECTED("Unexpected shader type");
                }

                pd3d12ShaderBytecode->pShaderBytecode = pByteCode->GetBufferPointer();
                pd3d12ShaderBytecode->BytecodeLength  = pByteCode->GetBufferSize();
            }

            d3d12PSODesc.pRootSignature = m_RootSig->GetD3D12RootSignature();

            memset(&d3d12PSODesc.StreamOutput, 0, sizeof(d3d12PSODesc.StreamOutput));

            BlendStateDesc_To_D3D12_BLEND_DESC(GraphicsPipeline.BlendDesc, d3d12PSODesc.BlendState);
            // The sample mask for the blend state.
            d3d12PSODesc.SampleMask = GraphicsPipeline.SampleMask;

            RasterizerStateDesc_To_D3D12_RASTERIZER_DESC(GraphicsPipeline.RasterizerDesc, d3d12PSODesc.RasterizerState);
            DepthStencilStateDesc_To_D3D12_DEPTH_STENCIL_DESC(GraphicsPipeline.DepthStencilDesc, d3d12PSODesc.DepthStencilState);

            std::vector<D3D12_INPUT_ELEMENT_DESC, STDAllocatorRawMem<D3D12_INPUT_ELEMENT_DESC>> d312InputElements(STD_ALLOCATOR_RAW_MEM(D3D12_INPUT_ELEMENT_DESC, GetRawAllocator(), "Allocator for vector<D3D12_INPUT_ELEMENT_DESC>"));

            const auto& InputLayout = GetGraphicsPipelineDesc().InputLayout;
            if (InputLayout.NumElements > 0)
            {
                LayoutElements_To_D3D12_INPUT_ELEMENT_DESCs(InputLayout, d312InputElements);
                d3d12PSODesc.InputLayout.NumElements        = static_cast<UINT>(d312InputElements.size());
                d3d12PSODesc.InputLayout.pInputElementDescs = d312InputElements.data();
            }
            else
            {
                d3d12PSODesc.InputLayout.NumElements        = 0;
                d3d12PSODesc.InputLayout.pInputElementDescs = nullptr;
            }

            d3d12PSODesc.IBStripCutValue = D3D12_INDEX_BUFFER_STRIP_CUT_VALUE_DISABLED;
            static const PrimitiveTopology_To_D3D12_PRIMITIVE_TOPOLOGY_TYPE PrimTopologyToD3D12TopologyType;
            d3d12PSODesc.PrimitiveTopologyType = PrimTopologyToD3D12TopologyType[GraphicsPipeline.PrimitiveTopology];

            d3d12PSODesc.NumRenderTargets = GraphicsPipeline.NumRenderTargets;
            for (Uint32 rt = 0; rt < GraphicsPipeline.NumRenderTargets; ++rt)
                d3d12PSODesc.RTVFormats[rt] = TexFormatToDXGI_Format(GraphicsPipeline.RTVFormats[rt]);
            for (Uint32 rt = GraphicsPipeline.NumRenderTargets; rt < _countof(d3d12PSODesc.RTVFormats); ++rt)
                d3d12PSODesc.RTVFormats[rt] = DXGI_FORMAT_UNKNOWN;
            d3d12PSODesc.DSVFormat = TexFormatToDXGI_Format(GraphicsPipeline.DSVFormat);

            d3d12PSODesc.SampleDesc.Count   = GraphicsPipeline.SmplDesc.Count;
            d3d12PSODesc.SampleDesc.Quality = GraphicsPipeline.SmplDesc.Quality;

            // For single GPU operation, set this to zero. If there are multiple GPU nodes,
            // set bits to identify the nodes (the device's physical adapters) for which the
            // graphics pipeline state is to apply. Each bit in the mask corresponds to a single node.
            d3d12PSODesc.NodeMask = 0;

            d3d12PSODesc.CachedPSO.pCachedBlob           = nullptr;
            d3d12PSODesc.CachedPSO.CachedBlobSizeInBytes = 0;

            // The only valid bit is D3D12_PIPELINE_STATE_FLAG_TOOL_DEBUG, which can only be set on WARP devices.
            d3d12PSODesc.Flags = D3D12_PIPELINE_STATE_FLAG_NONE;

            HRESULT hr = pd3d12Device->CreateGraphicsPipelineState(&d3d12PSODesc, IID_PPV_ARGS(&m_pd3d12PSO));
            if (FAILED(hr))
                LOG_ERROR_AND_THROW("Failed to create pipeline state");
        }
#ifdef D3D12_H_HAS_MESH_SHADER
        else if (m_Desc.PipelineType == PIPELINE_TYPE_MESH)
        {
            const auto& GraphicsPipeline = GetGraphicsPipelineDesc();

            struct MESH_SHADER_PIPELINE_STATE_DESC
            {
                PSS_SubObject<D3D12_PIPELINE_STATE_FLAGS, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_FLAGS>            Flags;
                PSS_SubObject<UINT, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_NODE_MASK>                              NodeMask;
                PSS_SubObject<ID3D12RootSignature*, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_ROOT_SIGNATURE>         pRootSignature;
                PSS_SubObject<D3D12_SHADER_BYTECODE, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_PS>                    PS;
                PSS_SubObject<D3D12_SHADER_BYTECODE, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_AS>                    AS;
                PSS_SubObject<D3D12_SHADER_BYTECODE, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_MS>                    MS;
                PSS_SubObject<D3D12_BLEND_DESC, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_BLEND>                      BlendState;
                PSS_SubObject<D3D12_DEPTH_STENCIL_DESC, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL>      DepthStencilState;
                PSS_SubObject<D3D12_RASTERIZER_DESC, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_RASTERIZER>            RasterizerState;
                PSS_SubObject<DXGI_SAMPLE_DESC, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_SAMPLE_DESC>                SampleDesc;
                PSS_SubObject<UINT, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_SAMPLE_MASK>                            SampleMask;
                PSS_SubObject<DXGI_FORMAT, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_DEPTH_STENCIL_FORMAT>            DSVFormat;
                PSS_SubObject<D3D12_RT_FORMAT_ARRAY, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_RENDER_TARGET_FORMATS> RTVFormatArray;
                PSS_SubObject<D3D12_CACHED_PIPELINE_STATE, D3D12_PIPELINE_STATE_SUBOBJECT_TYPE_CACHED_PSO>      CachedPSO;
            };
            MESH_SHADER_PIPELINE_STATE_DESC d3d12PSODesc = {};

            for (const auto& Stage : ShaderStages)
            {
                VERIFY_EXPR(Stage.Count() == 1);
                const auto& pByteCode = Stage.ByteCodes[0];

                D3D12_SHADER_BYTECODE* pd3d12ShaderBytecode = nullptr;
                switch (Stage.Type)
                {
                    // clang-format off
                    case SHADER_TYPE_AMPLIFICATION: pd3d12ShaderBytecode = &d3d12PSODesc.AS; break;
                    case SHADER_TYPE_MESH:          pd3d12ShaderBytecode = &d3d12PSODesc.MS; break;
                    case SHADER_TYPE_PIXEL:         pd3d12ShaderBytecode = &d3d12PSODesc.PS; break;
                    // clang-format on
                    default: UNEXPECTED("Unexpected shader type");
                }

                pd3d12ShaderBytecode->pShaderBytecode = pByteCode->GetBufferPointer();
                pd3d12ShaderBytecode->BytecodeLength  = pByteCode->GetBufferSize();
            }

            d3d12PSODesc.pRootSignature = m_RootSig->GetD3D12RootSignature();

            BlendStateDesc_To_D3D12_BLEND_DESC(GraphicsPipeline.BlendDesc, *d3d12PSODesc.BlendState);
            d3d12PSODesc.SampleMask = GraphicsPipeline.SampleMask;

            RasterizerStateDesc_To_D3D12_RASTERIZER_DESC(GraphicsPipeline.RasterizerDesc, *d3d12PSODesc.RasterizerState);
            DepthStencilStateDesc_To_D3D12_DEPTH_STENCIL_DESC(GraphicsPipeline.DepthStencilDesc, *d3d12PSODesc.DepthStencilState);

            d3d12PSODesc.RTVFormatArray->NumRenderTargets = GraphicsPipeline.NumRenderTargets;
            for (Uint32 rt = 0; rt < GraphicsPipeline.NumRenderTargets; ++rt)
                d3d12PSODesc.RTVFormatArray->RTFormats[rt] = TexFormatToDXGI_Format(GraphicsPipeline.RTVFormats[rt]);
            for (Uint32 rt = GraphicsPipeline.NumRenderTargets; rt < _countof(d3d12PSODesc.RTVFormatArray->RTFormats); ++rt)
                d3d12PSODesc.RTVFormatArray->RTFormats[rt] = DXGI_FORMAT_UNKNOWN;
            d3d12PSODesc.DSVFormat = TexFormatToDXGI_Format(GraphicsPipeline.DSVFormat);

            d3d12PSODesc.SampleDesc->Count   = GraphicsPipeline.SmplDesc.Count;
            d3d12PSODesc.SampleDesc->Quality = GraphicsPipeline.SmplDesc.Quality;

            // For single GPU operation, set this to zero. If there are multiple GPU nodes,
            // set bits to identify the nodes (the device's physical adapters) for which the
            // graphics pipeline state is to apply. Each bit in the mask corresponds to a single node.
            d3d12PSODesc.NodeMask = 0;

            d3d12PSODesc.CachedPSO->pCachedBlob           = nullptr;
            d3d12PSODesc.CachedPSO->CachedBlobSizeInBytes = 0;

            // The only valid bit is D3D12_PIPELINE_STATE_FLAG_TOOL_DEBUG, which can only be set on WARP devices.
            d3d12PSODesc.Flags = D3D12_PIPELINE_STATE_FLAG_NONE;

            D3D12_PIPELINE_STATE_STREAM_DESC streamDesc;
            streamDesc.SizeInBytes                   = sizeof(d3d12PSODesc);
            streamDesc.pPipelineStateSubobjectStream = &d3d12PSODesc;

            auto*   device2 = pDeviceD3D12->GetD3D12Device2();
            HRESULT hr      = device2->CreatePipelineState(&streamDesc, IID_PPV_ARGS(&m_pd3d12PSO));
            if (FAILED(hr))
                LOG_ERROR_AND_THROW("Failed to create pipeline state");
        }
#endif // D3D12_H_HAS_MESH_SHADER
        else
        {
            LOG_ERROR_AND_THROW("Unsupported pipeline type");
        }

        if (*m_Desc.Name != 0)
        {
            m_pd3d12PSO->SetName(WidenString(m_Desc.Name).c_str());
        }
    }
    catch (...)
    {
        Destruct();
        throw;
    }
}

PipelineStateD3D12Impl::PipelineStateD3D12Impl(IReferenceCounters*                   pRefCounters,
                                               RenderDeviceD3D12Impl*                pDeviceD3D12,
                                               const ComputePipelineStateCreateInfo& CreateInfo) :
    TPipelineStateBase{pRefCounters, pDeviceD3D12, CreateInfo}
{
    try
    {
        TShaderStages ShaderStages;
        InitInternalObjects(CreateInfo, ShaderStages);

        auto* pd3d12Device = pDeviceD3D12->GetD3D12Device();

        D3D12_COMPUTE_PIPELINE_STATE_DESC d3d12PSODesc = {};

        VERIFY_EXPR(ShaderStages[0].Type == SHADER_TYPE_COMPUTE);
        VERIFY_EXPR(ShaderStages[0].Count() == 1);
        const auto& pByteCode           = ShaderStages[0].ByteCodes[0];
        d3d12PSODesc.CS.pShaderBytecode = pByteCode->GetBufferPointer();
        d3d12PSODesc.CS.BytecodeLength  = pByteCode->GetBufferSize();

        // For single GPU operation, set this to zero. If there are multiple GPU nodes,
        // set bits to identify the nodes (the device's physical adapters) for which the
        // graphics pipeline state is to apply. Each bit in the mask corresponds to a single node.
        d3d12PSODesc.NodeMask = 0;

        d3d12PSODesc.CachedPSO.pCachedBlob           = nullptr;
        d3d12PSODesc.CachedPSO.CachedBlobSizeInBytes = 0;

        // The only valid bit is D3D12_PIPELINE_STATE_FLAG_TOOL_DEBUG, which can only be set on WARP devices.
        d3d12PSODesc.Flags = D3D12_PIPELINE_STATE_FLAG_NONE;

        d3d12PSODesc.pRootSignature = m_RootSig->GetD3D12RootSignature();

        HRESULT hr = pd3d12Device->CreateComputePipelineState(&d3d12PSODesc, IID_PPV_ARGS(&m_pd3d12PSO));
        if (FAILED(hr))
            LOG_ERROR_AND_THROW("Failed to create pipeline state");

        if (*m_Desc.Name != 0)
        {
            m_pd3d12PSO->SetName(WidenString(m_Desc.Name).c_str());
        }
    }
    catch (...)
    {
        Destruct();
        throw;
    }
}

PipelineStateD3D12Impl::PipelineStateD3D12Impl(IReferenceCounters*                      pRefCounters,
                                               RenderDeviceD3D12Impl*                   pDeviceD3D12,
                                               const RayTracingPipelineStateCreateInfo& CreateInfo) :
    TPipelineStateBase{pRefCounters, pDeviceD3D12, CreateInfo}
{
    try
    {
        LocalRootSignatureD3D12 LocalRootSig{CreateInfo.pShaderRecordName, CreateInfo.RayTracingPipeline.ShaderRecordSize};
        TShaderStages           ShaderStages;
        InitInternalObjects(CreateInfo, ShaderStages, &LocalRootSig);

        auto* pd3d12Device = pDeviceD3D12->GetD3D12Device5();

        DynamicLinearAllocator             TempPool{GetRawAllocator(), 4 << 10};
        std::vector<D3D12_STATE_SUBOBJECT> Subobjects;
        BuildRTPipelineDescription(CreateInfo, Subobjects, TempPool, ShaderStages);

        D3D12_GLOBAL_ROOT_SIGNATURE GlobalRoot = {m_RootSig->GetD3D12RootSignature()};
        Subobjects.push_back({D3D12_STATE_SUBOBJECT_TYPE_GLOBAL_ROOT_SIGNATURE, &GlobalRoot});

        D3D12_LOCAL_ROOT_SIGNATURE LocalRoot = {LocalRootSig.GetD3D12RootSignature()};
        if (LocalRoot.pLocalRootSignature)
            Subobjects.push_back({D3D12_STATE_SUBOBJECT_TYPE_LOCAL_ROOT_SIGNATURE, &LocalRoot});

        D3D12_STATE_OBJECT_DESC RTPipelineDesc = {};
        RTPipelineDesc.Type                    = D3D12_STATE_OBJECT_TYPE_RAYTRACING_PIPELINE;
        RTPipelineDesc.NumSubobjects           = static_cast<UINT>(Subobjects.size());
        RTPipelineDesc.pSubobjects             = Subobjects.data();

        HRESULT hr = pd3d12Device->CreateStateObject(&RTPipelineDesc, IID_PPV_ARGS(&m_pd3d12PSO));
        if (FAILED(hr))
            LOG_ERROR_AND_THROW("Failed to create ray tracing state object");

        // Extract shader identifiers from ray tracing pipeline and store them in ShaderHandles
        GetShaderIdentifiers(m_pd3d12PSO, CreateInfo, m_pRayTracingPipelineData->NameToGroupIndex,
                             m_pRayTracingPipelineData->ShaderHandles, m_pRayTracingPipelineData->ShaderHandleSize);

        if (*m_Desc.Name != 0)
        {
            m_pd3d12PSO->SetName(WidenString(m_Desc.Name).c_str());
        }
    }
    catch (...)
    {
        Destruct();
        throw;
    }
}

PipelineStateD3D12Impl::~PipelineStateD3D12Impl()
{
    Destruct();
}

void PipelineStateD3D12Impl::Destruct()
{
    m_Signatures.fill({});
    m_RootSig.Release();

    if (m_pd3d12PSO)
    {
        // D3D12 object can only be destroyed when it is no longer used by the GPU
        m_pDevice->SafeReleaseDeviceObject(std::move(m_pd3d12PSO), m_Desc.CommandQueueMask);
    }

    TPipelineStateBase::Destruct();
}

bool PipelineStateD3D12Impl::IsCompatibleWith(const IPipelineState* pPSO) const
{
    VERIFY_EXPR(pPSO != nullptr);

    if (pPSO == this)
        return true;

    return (m_RootSig == ValidatedCast<const PipelineStateD3D12Impl>(pPSO)->m_RootSig);
}

} // namespace Diligent
