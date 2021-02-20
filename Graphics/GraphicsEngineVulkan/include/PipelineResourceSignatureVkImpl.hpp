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

#pragma once

/// \file
/// Declaration of Diligent::PipelineResourceSignatureVkImpl class

#include <array>

#include "PipelineResourceSignatureBase.hpp"
#include "VulkanUtilities/VulkanObjectWrappers.hpp"
#include "SRBMemoryAllocator.hpp"

namespace Diligent
{

class RenderDeviceVkImpl;
class ShaderResourceCacheVk;
class ShaderVariableManagerVk;
struct SPIRVShaderResourceAttribs;

enum class DescriptorType : Uint8
{
    Sampler,
    CombinedImageSampler,
    SeparateImage,
    StorageImage,
    UniformTexelBuffer,
    StorageTexelBuffer,
    StorageTexelBuffer_ReadOnly,
    UniformBuffer,
    UniformBufferDynamic,
    StorageBuffer,
    StorageBuffer_ReadOnly,
    StorageBufferDynamic,
    StorageBufferDynamic_ReadOnly,
    InputAttachment,
    AccelerationStructure,
    Count,
    Unknown = 0xFF,
};

RESOURCE_STATE DescriptorTypeToResourceState(DescriptorType Type);


/// Implementation of the Diligent::PipelineResourceSignatureVkImpl class
class PipelineResourceSignatureVkImpl final : public PipelineResourceSignatureBase<IPipelineResourceSignature, RenderDeviceVkImpl>
{
public:
    using TPipelineResourceSignatureBase = PipelineResourceSignatureBase<IPipelineResourceSignature, RenderDeviceVkImpl>;

    // Descriptor set identifier (this is not the descriptor set index in the set layout!)
    enum DESCRIPTOR_SET_ID : size_t
    {
        // Static/mutable variables descriptor set id
        DESCRIPTOR_SET_ID_STATIC_MUTABLE = 0,

        // Dynamic variables descriptor set id
        DESCRIPTOR_SET_ID_DYNAMIC,

        DESCRIPTOR_SET_ID_NUM_SETS
    };

    // Static/mutable and dynamic descriptor sets
    static constexpr Uint32 MAX_DESCRIPTOR_SETS = DESCRIPTOR_SET_ID_NUM_SETS;

    PipelineResourceSignatureVkImpl(IReferenceCounters*                        pRefCounters,
                                    RenderDeviceVkImpl*                        pDevice,
                                    const PipelineResourceSignatureCreateInfo& CreateInfo,
                                    bool                                       bIsDeviceInternal = false);
    ~PipelineResourceSignatureVkImpl();

    Uint32 GetDynamicOffsetCount() const { return m_DynamicUniformBufferCount + m_DynamicStorageBufferCount; }
    Uint32 GetDynamicUniformBufferCount() const { return m_DynamicUniformBufferCount; }
    Uint32 GetDynamicStorageBufferCount() const { return m_DynamicStorageBufferCount; }
    Uint32 GetNumDescriptorSets() const
    {
        static_assert(DESCRIPTOR_SET_ID_NUM_SETS == 2, "Please update this method with new descriptor set id");
        return (HasDescriptorSet(DESCRIPTOR_SET_ID_STATIC_MUTABLE) ? 1 : 0) + (HasDescriptorSet(DESCRIPTOR_SET_ID_DYNAMIC) ? 1 : 0);
    }

    enum class CacheContentType
    {
        Signature = 0, // only static resources
        SRB       = 1  // in SRB
    };

    // sizeof(ResourceAttribs) == 16, x64
    struct ResourceAttribs
    {
    private:
        static constexpr Uint32 _BindingIndexBits    = 16;
        static constexpr Uint32 _SamplerIndBits      = 16;
        static constexpr Uint32 _ArraySizeBits       = 26;
        static constexpr Uint32 _DescrTypeBits       = 4;
        static constexpr Uint32 _DescrSetBits        = 1;
        static constexpr Uint32 _SamplerAssignedBits = 1;

        static_assert((_BindingIndexBits + _ArraySizeBits + _SamplerIndBits + _DescrTypeBits + _DescrSetBits + _SamplerAssignedBits) % 32 == 0, "Bits are not optimally packed");

        static_assert((1u << _DescrTypeBits) >= static_cast<Uint32>(DescriptorType::Count), "Not enough bits to store DescriptorType values");
        static_assert((1u << _DescrSetBits) >= MAX_DESCRIPTOR_SETS, "Not enough bits to store descriptor set index");
        static_assert((1u << _BindingIndexBits) >= MAX_RESOURCES_IN_SIGNATURE, "Not enough bits to store resource binding index");
        static_assert((1u << _SamplerIndBits) >= MAX_RESOURCES_IN_SIGNATURE, "Not enough bits to store sampler resource index");

    public:
        static constexpr Uint32 InvalidSamplerInd = (1u << _SamplerIndBits) - 1;

        // clang-format off
        const Uint32  BindingIndex         : _BindingIndexBits;    // Binding in the descriptor set
        const Uint32  SamplerInd           : _SamplerIndBits;      // Index of the assigned sampler in m_Desc.Resources and m_pResourceAttribs
        const Uint32  ArraySize            : _ArraySizeBits;       // Array size
        const Uint32  DescrType            : _DescrTypeBits;       // Descriptor type (DescriptorType)
        const Uint32  DescrSet             : _DescrSetBits;        // Descriptor set (0 or 1)
        const Uint32  ImtblSamplerAssigned : _SamplerAssignedBits; // Immutable sampler flag

        const Uint32  SRBCacheOffset;                              // Offset in the SRB resource cache
        const Uint32  StaticCacheOffset;                           // Offset in the static resource cache
        // clang-format on

        ResourceAttribs(Uint32         _BindingIndex,
                        Uint32         _SamplerInd,
                        Uint32         _ArraySize,
                        DescriptorType _DescrType,
                        Uint32         _DescrSet,
                        bool           _ImtblSamplerAssigned,
                        Uint32         _SRBCacheOffset,
                        Uint32         _StaticCacheOffset) noexcept :
            // clang-format off
            BindingIndex         {_BindingIndex                  },  
            SamplerInd           {_SamplerInd                    },
            ArraySize            {_ArraySize                     },
            DescrType            {static_cast<Uint32>(_DescrType)},
            DescrSet             {_DescrSet                      },
            ImtblSamplerAssigned {_ImtblSamplerAssigned ? 1u : 0u},
            SRBCacheOffset       {_SRBCacheOffset                },
            StaticCacheOffset    {_StaticCacheOffset             }
        // clang-format on
        {
            VERIFY(BindingIndex == _BindingIndex, "Binding index (", _BindingIndex, ") exceeds maximum representable value");
            VERIFY(ArraySize == _ArraySize, "Array size (", _ArraySize, ") exceeds maximum representable value");
            VERIFY(SamplerInd == _SamplerInd, "Sampler index (", _SamplerInd, ") exceeds maximum representable value");
            VERIFY(GetDescriptorType() == _DescrType, "Descriptor type (", static_cast<Uint32>(_DescrType), ") exceeds maximum representable value");
            VERIFY(DescrSet == _DescrSet, "Descriptor set (", _DescrSet, ") exceeds maximum representable value");
        }

        Uint32 CacheOffset(CacheContentType CacheType) const
        {
            return CacheType == CacheContentType::SRB ? SRBCacheOffset : StaticCacheOffset;
        }

        DescriptorType GetDescriptorType() const { return static_cast<DescriptorType>(DescrType); }
        bool           IsImmutableSamplerAssigned() const { return ImtblSamplerAssigned != 0; }
        bool           IsCombinedWithSampler() const { return SamplerInd != InvalidSamplerInd; }
    };

    const ResourceAttribs& GetResourceAttribs(Uint32 ResIndex) const
    {
        VERIFY_EXPR(ResIndex < m_Desc.NumResources);
        return m_pResourceAttribs[ResIndex];
    }

    const PipelineResourceDesc& GetResourceDesc(Uint32 ResIndex) const
    {
        VERIFY_EXPR(ResIndex < m_Desc.NumResources);
        return m_Desc.Resources[ResIndex];
    }

    struct ImmutableSamplerAttribs
    {
        RefCntAutoPtr<ISampler> Ptr;

        Uint32 DescrSet     = ~0u;
        Uint32 BindingIndex = ~0u;
    };

    const ImmutableSamplerAttribs& GetImmutableSamplerAttribs(Uint32 SampIndex) const
    {
        VERIFY_EXPR(SampIndex < m_Desc.NumImmutableSamplers);
        return m_ImmutableSamplers[SampIndex];
    }

    const ImmutableSamplerDesc& GetImmutableSamplerDesc(Uint32 SampIndex) const
    {
        VERIFY_EXPR(SampIndex < m_Desc.NumImmutableSamplers);
        return m_Desc.ImmutableSamplers[SampIndex];
    }

    VkDescriptorSetLayout GetVkDescriptorSetLayout(DESCRIPTOR_SET_ID SetId) const { return m_VkDescrSetLayouts[SetId]; }

    bool HasDescriptorSet(DESCRIPTOR_SET_ID SetId) const { return m_VkDescrSetLayouts[SetId] != VK_NULL_HANDLE; }

    /// Implementation of IPipelineResourceSignature::CreateShaderResourceBinding.
    virtual void DILIGENT_CALL_TYPE CreateShaderResourceBinding(IShaderResourceBinding** ppShaderResourceBinding,
                                                                bool                     InitStaticResources) override final;

    /// Implementation of IPipelineResourceSignature::GetStaticVariableByName.
    virtual IShaderResourceVariable* DILIGENT_CALL_TYPE GetStaticVariableByName(SHADER_TYPE ShaderType, const Char* Name) override final;

    /// Implementation of IPipelineResourceSignature::GetStaticVariableByIndex.
    virtual IShaderResourceVariable* DILIGENT_CALL_TYPE GetStaticVariableByIndex(SHADER_TYPE ShaderType, Uint32 Index) override final;

    /// Implementation of IPipelineResourceSignature::GetStaticVariableCount.
    virtual Uint32 DILIGENT_CALL_TYPE GetStaticVariableCount(SHADER_TYPE ShaderType) const override final;

    /// Implementation of IPipelineResourceSignature::BindStaticResources.
    virtual void DILIGENT_CALL_TYPE BindStaticResources(Uint32            ShaderFlags,
                                                        IResourceMapping* pResourceMapping,
                                                        Uint32            Flags) override final;

    /// Implementation of IPipelineResourceSignature::IsCompatibleWith.
    virtual bool DILIGENT_CALL_TYPE IsCompatibleWith(const IPipelineResourceSignature* pPRS) const override final
    {
        VERIFY_EXPR(pPRS != nullptr);
        return IsCompatibleWith(*ValidatedCast<const PipelineResourceSignatureVkImpl>(pPRS));
    }

    SRBMemoryAllocator& GetSRBMemoryAllocator()
    {
        return m_SRBMemAllocator;
    }

    void InitSRBResourceCache(ShaderResourceCacheVk& ResourceCache,
                              IMemoryAllocator&      CacheMemAllocator,
                              const char*            DbgPipelineName) const;

    // Copies static resources from the static resource cache to the destination cache
    void InitializeStaticSRBResources(ShaderResourceCacheVk& ResourceCache) const;

    // Binds object pObj to resource with index ResIndex in m_Desc.Resources and
    // array index ArrayIndex.
    void BindResource(IDeviceObject*         pObj,
                      Uint32                 ArrayIndex,
                      Uint32                 ResIndex,
                      ShaderResourceCacheVk& ResourceCache) const;

    bool IsBound(Uint32                       ArrayIndex,
                 Uint32                       ResIndex,
                 const ShaderResourceCacheVk& ResourceCache) const;

    // Commits dynamic resources from ResourceCache to vkDynamicDescriptorSet
    void CommitDynamicResources(const ShaderResourceCacheVk& ResourceCache,
                                VkDescriptorSet              vkDynamicDescriptorSet) const;

    bool IsCompatibleWith(const PipelineResourceSignatureVkImpl& Other) const;

    bool IsIncompatibleWith(const PipelineResourceSignatureVkImpl& Other) const
    {
        return GetHash() != Other.GetHash();
    }

#ifdef DILIGENT_DEVELOPMENT
    /// Verifies committed resource attribs using the SPIRV resource attributes from the PSO.
    bool DvpValidateCommittedResource(const SPIRVShaderResourceAttribs& SPIRVAttribs,
                                      Uint32                            ResIndex,
                                      ShaderResourceCacheVk&            ResourceCache) const;
#endif

    // Returns the descriptor set index in the resource cache
    template <DESCRIPTOR_SET_ID SetId>
    Uint32 GetDescriptorSetIndex() const;

private:
    // Resource cache group identifier
    enum CACHE_GROUP : size_t
    {
        CACHE_GROUP_DYN_UB = 0,         // Uniform buffer with dynamic offset
        CACHE_GROUP_DYN_SB,             // Storage buffer with dynamic offset
        CACHE_GROUP_OTHER,              // Other resource type
        CACHE_GROUP_COUNT_PER_VAR_TYPE, // Cache group count per shader variable type

        CACHE_GROUP_DYN_UB_STAT_VAR = CACHE_GROUP_DYN_UB, // Uniform buffer with dynamic offset, static variable
        CACHE_GROUP_DYN_SB_STAT_VAR = CACHE_GROUP_DYN_SB, // Storage buffer with dynamic offset, static variable
        CACHE_GROUP_OTHER_STAT_VAR  = CACHE_GROUP_OTHER,  // Other resource type, static variable

        CACHE_GROUP_DYN_UB_DYN_VAR, // Uniform buffer with dynamic offset, dynamic variable
        CACHE_GROUP_DYN_SB_DYN_VAR, // Storage buffer with dynamic offset, dynamic variable
        CACHE_GROUP_OTHER_DYN_VAR,  // Other resource type, dynamic variable

        CACHE_GROUP_COUNT
    };
    static_assert(CACHE_GROUP_COUNT == CACHE_GROUP_COUNT_PER_VAR_TYPE * MAX_DESCRIPTOR_SETS, "Inconsistent cache group count");

    using CacheOffsetsType = std::array<Uint32, CACHE_GROUP_COUNT>; // [dynamic uniform buffers, dynamic storage buffers, other] x [descriptor sets] including ArraySize
    using BindingCountType = std::array<Uint32, CACHE_GROUP_COUNT>; // [dynamic uniform buffers, dynamic storage buffers, other] x [descriptor sets] not counting ArraySize

    void Destruct();

    void CreateSetLayouts(const CacheOffsetsType& CacheSizes,
                          const BindingCountType& BindingCount);

    size_t CalculateHash() const;

    static inline CACHE_GROUP       GetResourceCacheGroup(const PipelineResourceDesc& Res);
    static inline DESCRIPTOR_SET_ID VarTypeToDescriptorSetId(SHADER_RESOURCE_VARIABLE_TYPE VarType);

private:
    std::array<VulkanUtilities::DescriptorSetLayoutWrapper, DESCRIPTOR_SET_ID_NUM_SETS> m_VkDescrSetLayouts;

    // Descriptor set sizes indexed by the set index in the layout (not DESCRIPTOR_SET_ID!)
    std::array<Uint32, MAX_DESCRIPTOR_SETS> m_DescriptorSetSizes = {~0U, ~0U};

    ResourceAttribs* m_pResourceAttribs = nullptr; // [m_Desc.NumResources]

    // The total number of uniform buffers with dynamic offsets in both descriptor sets,
    // accounting for array size.
    Uint16 m_DynamicUniformBufferCount = 0;
    // The total number storage buffers with dynamic offsets in both descriptor sets,
    // accounting for array size.
    Uint16 m_DynamicStorageBufferCount = 0;

    // Static resource cache for all static resources
    ShaderResourceCacheVk* m_pStaticResCache = nullptr;
    // Static variables manager for every shader stage
    ShaderVariableManagerVk* m_StaticVarsMgrs = nullptr; // [m_NumShaderStages]

    ImmutableSamplerAttribs* m_ImmutableSamplers = nullptr; // [m_Desc.NumImmutableSamplers]

    SRBMemoryAllocator m_SRBMemAllocator;
};

template <> Uint32 PipelineResourceSignatureVkImpl::GetDescriptorSetIndex<PipelineResourceSignatureVkImpl::DESCRIPTOR_SET_ID_STATIC_MUTABLE>() const;
template <> Uint32 PipelineResourceSignatureVkImpl::GetDescriptorSetIndex<PipelineResourceSignatureVkImpl::DESCRIPTOR_SET_ID_DYNAMIC>() const;

} // namespace Diligent
