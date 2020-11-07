/*
 *  Copyright 2019-2020 Diligent Graphics LLC
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
/// Definition of the Diligent::TopLevelASVkImpl class

#include "RenderDeviceVk.h"
#include "RenderDeviceVkImpl.hpp"
#include "TopLevelASVk.h"
#include "TopLevelASBase.hpp"
#include "BottomLevelASVkImpl.hpp"
#include "VulkanUtilities/VulkanObjectWrappers.hpp"

namespace Diligent
{

class TopLevelASVkImpl final : public TopLevelASBase<ITopLevelASVk, BottomLevelASVkImpl, RenderDeviceVkImpl>
{
public:
    using TTopLevelASBase = TopLevelASBase<ITopLevelASVk, BottomLevelASVkImpl, RenderDeviceVkImpl>;

    TopLevelASVkImpl(IReferenceCounters*   pRefCounters,
                     RenderDeviceVkImpl*   pRenderDeviceVk,
                     const TopLevelASDesc& Desc);
    TopLevelASVkImpl(IReferenceCounters*        pRefCounters,
                     RenderDeviceVkImpl*        pRenderDeviceVk,
                     const TopLevelASDesc&      Desc,
                     RESOURCE_STATE             InitialState,
                     VkAccelerationStructureKHR vkTLAS);
    ~TopLevelASVkImpl();

    IMPLEMENT_QUERY_INTERFACE_IN_PLACE(IID_TopLevelASVk, TTopLevelASBase);

    /// Implementation of ITopLevelAS::GetNativeHandle() in Vulkan backend.
    virtual void* DILIGENT_CALL_TYPE GetNativeHandle() override final
    {
        auto Handle = GetVkTLAS();
        return reinterpret_cast<void*>(Handle); // AZ TODO: fix for 32 bit platform
    }

    /// Implementation of ITopLevelASVk::GetVkTLAS().
    virtual VkAccelerationStructureKHR DILIGENT_CALL_TYPE GetVkTLAS() const override { return m_VulkanTLAS; }

    const VkAccelerationStructureKHR* GetVkTLASPtr() const { return &m_VulkanTLAS; }

private:
    VulkanUtilities::AccelStructWrapper     m_VulkanTLAS;
    VulkanUtilities::VulkanMemoryAllocation m_MemoryAllocation;
    VkDeviceSize                            m_MemoryAlignedOffset = 0;
};

} // namespace Diligent
