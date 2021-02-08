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

#include <thread>
#include <array>

#include "TestingEnvironment.hpp"
#include "TestingSwapChainBase.hpp"
#include "BasicMath.hpp"
#include "MapHelper.hpp"

#include "gtest/gtest.h"

namespace Diligent
{

namespace Testing
{

#if D3D11_SUPPORTED
void RenderDrawCommandReferenceD3D11(ISwapChain* pSwapChain, const float* pClearColor);
#endif

#if D3D12_SUPPORTED
void RenderDrawCommandReferenceD3D12(ISwapChain* pSwapChain, const float* pClearColor);
#endif

#if GL_SUPPORTED || GLES_SUPPORTED
void RenderDrawCommandReferenceGL(ISwapChain* pSwapChain, const float* pClearColor);
#endif

#if VULKAN_SUPPORTED
void RenderDrawCommandReferenceVk(ISwapChain* pSwapChain, const float* pClearColor);
#endif

#if METAL_SUPPORTED
void RenderDrawCommandReferenceMtl(ISwapChain* pSwapChain, const float* pClearColor);
#endif

void RenderDrawCommandReference(ISwapChain* pSwapChain, const float* pClearColor = nullptr)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pDevice  = pEnv->GetDevice();
    auto* pContext = pEnv->GetDeviceContext();

    RefCntAutoPtr<ITestingSwapChain> pTestingSwapChain{pSwapChain, IID_TestingSwapChain};
    if (pTestingSwapChain)
    {
        pContext->Flush();
        pContext->InvalidateState();

        auto deviceType = pDevice->GetDeviceCaps().DevType;
        switch (deviceType)
        {
#if D3D11_SUPPORTED
            case RENDER_DEVICE_TYPE_D3D11:
                RenderDrawCommandReferenceD3D11(pSwapChain, pClearColor);
                break;
#endif

#if D3D12_SUPPORTED
            case RENDER_DEVICE_TYPE_D3D12:
                RenderDrawCommandReferenceD3D12(pSwapChain, pClearColor);
                break;
#endif

#if GL_SUPPORTED || GLES_SUPPORTED
            case RENDER_DEVICE_TYPE_GL:
            case RENDER_DEVICE_TYPE_GLES:
                RenderDrawCommandReferenceGL(pSwapChain, pClearColor);
                break;

#endif

#if VULKAN_SUPPORTED
            case RENDER_DEVICE_TYPE_VULKAN:
                RenderDrawCommandReferenceVk(pSwapChain, pClearColor);
                break;
#endif

#if METAL_SUPPORTED
            case RENDER_DEVICE_TYPE_METAL:
                RenderDrawCommandReferenceMtl(pSwapChain, pClearColor);
                break;
#endif

            default:
                LOG_ERROR_AND_THROW("Unsupported device type");
        }

        pTestingSwapChain->TakeSnapshot();
    }
}

} // namespace Testing

} // namespace Diligent

using namespace Diligent;
using namespace Diligent::Testing;

#include "InlineShaders/DrawCommandTestHLSL.h"

namespace
{

namespace HLSL
{

// clang-format off
const std::string DrawTest_VS{
R"(
struct PSInput 
{ 
    float4 Pos   : SV_POSITION; 
    float3 Color : COLOR; 
};

struct VSInput
{
    float4 Pos   : ATTRIB0;
    float3 Color : ATTRIB1; 
};

void main(in  VSInput VSIn,
          out PSInput PSIn) 
{
    PSIn.Pos   = VSIn.Pos;
    PSIn.Color = VSIn.Color;
}
)"
};


const std::string DrawTest_VSInstanced{
R"(
struct PSInput 
{ 
    float4 Pos   : SV_POSITION; 
    float3 Color : COLOR; 
};

struct VSInput
{
    float4 Pos       : ATTRIB0;
    float3 Color     : ATTRIB1; 
    float4 ScaleBias : ATTRIB2; 
};

void main(in  VSInput VSIn,
          out PSInput PSIn) 
{
    PSIn.Pos.xy = VSIn.Pos.xy * VSIn.ScaleBias.xy + VSIn.ScaleBias.zw;
    PSIn.Pos.zw = VSIn.Pos.zw;
    PSIn.Color  = VSIn.Color;
}
)"
};

const std::string DrawTest_DynamicBuffers{
R"(

cbuffer DynamicCB0
{
    float4 Positions[4];
}

cbuffer DynamicCB1
{
    float4 Colors[4];
}

cbuffer ImmutableCB
{
    float4 PositionZW;
}

struct PSInput 
{ 
    float4 Pos   : SV_POSITION; 
    float3 Color : COLOR; 
};

void main(in  uint    VertId : SV_VertexID,
          out PSInput PSIn) 
{
    PSIn.Pos   = float4(Positions[VertId].xy, PositionZW.xy);
    PSIn.Color = Colors[VertId];
}
)"
};
// clang-format on

} // namespace HLSL

struct Vertex
{
    float4 Pos;
    float3 Color;
};

// clang-format off
float4 Pos[] = 
{
    float4(-1.0f,  -0.5f,  0.f,  1.f),
    float4(-0.5f,  +0.5f,  0.f,  1.f),
    float4( 0.0f,  -0.5f,  0.f,  1.f),

    float4(+0.0f,  -0.5f,  0.f,  1.f),
    float4(+0.5f,  +0.5f,  0.f,  1.f),
    float4(+1.0f,  -0.5f,  0.f,  1.f)
};

float3 Color[] =
{
    float3(1.f,  0.f,  0.f),
    float3(0.f,  1.f,  0.f),
    float3(0.f,  0.f,  1.f),
};

Vertex Vert[] = 
{
    {Pos[0], Color[0]},
    {Pos[1], Color[1]},
    {Pos[2], Color[2]},

    {Pos[3], Color[0]},
    {Pos[4], Color[1]},
    {Pos[5], Color[2]}
};

Vertex VertInst[] = 
{
    {float4(-1.0,  0.0,  0.0,  1.0), Color[0]},
    {float4( 0.0, +2.0,  0.0,  1.0), Color[1]},
    {float4(+1.0,  0.0,  0.0,  1.0), Color[2]}
};
// clang-format on

class DrawCommandTest : public ::testing::Test
{
protected:
    static void SetUpTestSuite()
    {
        auto* pEnv       = TestingEnvironment::GetInstance();
        auto* pDevice    = pEnv->GetDevice();
        auto* pSwapChain = pEnv->GetSwapChain();

        TestingEnvironment::ScopedReleaseResources AutoreleaseResources;

        RenderDrawCommandReference(pSwapChain);

        GraphicsPipelineStateCreateInfo PSOCreateInfo;

        auto& PSODesc          = PSOCreateInfo.PSODesc;
        auto& GraphicsPipeline = PSOCreateInfo.GraphicsPipeline;

        PSODesc.Name = "Draw command test - procedural triangles";

        PSODesc.PipelineType                          = PIPELINE_TYPE_GRAPHICS;
        GraphicsPipeline.NumRenderTargets             = 1;
        GraphicsPipeline.RTVFormats[0]                = pSwapChain->GetDesc().ColorBufferFormat;
        GraphicsPipeline.PrimitiveTopology            = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
        GraphicsPipeline.RasterizerDesc.CullMode      = CULL_MODE_NONE;
        GraphicsPipeline.DepthStencilDesc.DepthEnable = False;

        ShaderCreateInfo ShaderCI;
        ShaderCI.SourceLanguage             = SHADER_SOURCE_LANGUAGE_HLSL;
        ShaderCI.ShaderCompiler             = pEnv->GetDefaultCompiler(ShaderCI.SourceLanguage);
        ShaderCI.UseCombinedTextureSamplers = true;

        RefCntAutoPtr<IShader> pProceduralVS;
        {
            ShaderCI.Desc.ShaderType = SHADER_TYPE_VERTEX;
            ShaderCI.EntryPoint      = "main";
            ShaderCI.Desc.Name       = "Draw command test procedural vertex shader";
            ShaderCI.Source          = HLSL::DrawTest_ProceduralTriangleVS.c_str();
            pDevice->CreateShader(ShaderCI, &pProceduralVS);
            ASSERT_NE(pProceduralVS, nullptr);
        }

        RefCntAutoPtr<IShader> pVS;
        {
            ShaderCI.Desc.ShaderType = SHADER_TYPE_VERTEX;
            ShaderCI.EntryPoint      = "main";
            ShaderCI.Desc.Name       = "Draw command test vertex shader";
            ShaderCI.Source          = HLSL::DrawTest_VS.c_str();
            pDevice->CreateShader(ShaderCI, &pVS);
            ASSERT_NE(pVS, nullptr);
        }

        RefCntAutoPtr<IShader> pInstancedVS;
        {
            ShaderCI.Desc.ShaderType = SHADER_TYPE_VERTEX;
            ShaderCI.EntryPoint      = "main";
            ShaderCI.Desc.Name       = "Draw command test instanced vertex shader";
            ShaderCI.Source          = HLSL::DrawTest_VSInstanced.c_str();
            pDevice->CreateShader(ShaderCI, &pInstancedVS);
            ASSERT_NE(pInstancedVS, nullptr);
        }

        RefCntAutoPtr<IShader> pPS;
        {
            ShaderCI.Desc.ShaderType = SHADER_TYPE_PIXEL;
            ShaderCI.EntryPoint      = "main";
            ShaderCI.Desc.Name       = "Draw command test pixel shader";
            ShaderCI.Source          = HLSL::DrawTest_PS.c_str();
            pDevice->CreateShader(ShaderCI, &pPS);
            ASSERT_NE(pPS, nullptr);
        }

        PSODesc.Name = "Draw command test - procedural tris";

        PSOCreateInfo.pVS = pProceduralVS;
        PSOCreateInfo.pPS = pPS;
        pDevice->CreateGraphicsPipelineState(PSOCreateInfo, &sm_pDrawProceduralPSO);
        ASSERT_NE(sm_pDrawProceduralPSO, nullptr);

        PSODesc.Name = "Draw command test";

        InputLayoutDesc LayoutDesc;
        // clang-format off
        LayoutElement Elems[] =
        {
            LayoutElement{ 0, 0, 4, VT_FLOAT32},
            LayoutElement{ 1, 0, 3, VT_FLOAT32}
        };
        // clang-format on

        GraphicsPipeline.InputLayout.LayoutElements = Elems;
        GraphicsPipeline.InputLayout.NumElements    = _countof(Elems);

        PSOCreateInfo.pVS                  = pVS;
        PSOCreateInfo.pPS                  = pPS;
        GraphicsPipeline.PrimitiveTopology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
        pDevice->CreateGraphicsPipelineState(PSOCreateInfo, &sm_pDrawPSO);
        ASSERT_NE(sm_pDrawPSO, nullptr);


        PSODesc.Name = "Draw command test - 2x VB stride";

        Elems[0].Stride = sizeof(Vertex) * 2;
        pDevice->CreateGraphicsPipelineState(PSOCreateInfo, &sm_pDraw_2xStride_PSO);
        ASSERT_NE(sm_pDraw_2xStride_PSO, nullptr);

        PSODesc.Name = "Instanced draw command test";
        // clang-format off
        LayoutElement InstancedElems[] =
        {
            LayoutElement{ 0, 0, 4, VT_FLOAT32},
            LayoutElement{ 1, 0, 3, VT_FLOAT32},
            LayoutElement{ 2, 1, 4, VT_FLOAT32, false, INPUT_ELEMENT_FREQUENCY_PER_INSTANCE}
        };
        // clang-format on

        GraphicsPipeline.InputLayout.LayoutElements = InstancedElems;
        GraphicsPipeline.InputLayout.NumElements    = _countof(InstancedElems);

        PSOCreateInfo.pVS = pInstancedVS;
        pDevice->CreateGraphicsPipelineState(PSOCreateInfo, &sm_pDrawInstancedPSO);
        ASSERT_NE(sm_pDrawInstancedPSO, nullptr);
    }

    static void TearDownTestSuite()
    {
        sm_pDrawProceduralPSO.Release();
        sm_pDrawPSO.Release();
        sm_pDraw_2xStride_PSO.Release();
        sm_pDrawInstancedPSO.Release();

        auto* pEnv = TestingEnvironment::GetInstance();
        pEnv->Reset();
    }

    static void SetRenderTargets(IPipelineState* pPSO)
    {
        auto* pEnv       = TestingEnvironment::GetInstance();
        auto* pContext   = pEnv->GetDeviceContext();
        auto* pSwapChain = pEnv->GetSwapChain();

        ITextureView* pRTVs[] = {pSwapChain->GetCurrentBackBufferRTV()};
        pContext->SetRenderTargets(1, pRTVs, nullptr, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

        const float ClearColor[] = {0.f, 0.f, 0.f, 0.0f};
        pContext->ClearRenderTarget(pRTVs[0], ClearColor, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

        pContext->SetPipelineState(pPSO);
    }

    static void Present()
    {
        auto* pEnv       = TestingEnvironment::GetInstance();
        auto* pSwapChain = pEnv->GetSwapChain();
        auto* pContext   = pEnv->GetDeviceContext();

        pSwapChain->Present();

        pContext->Flush();
        pContext->InvalidateState();
    }

    RefCntAutoPtr<IBuffer> CreateVertexBuffer(const void* VertexData, Uint32 DataSize)
    {
        BufferDesc BuffDesc;
        BuffDesc.Name          = "Test vertex buffer";
        BuffDesc.BindFlags     = BIND_VERTEX_BUFFER;
        BuffDesc.uiSizeInBytes = DataSize;

        BufferData InitialData;
        InitialData.pData    = VertexData;
        InitialData.DataSize = DataSize;

        auto* pDevice = TestingEnvironment::GetInstance()->GetDevice();

        RefCntAutoPtr<IBuffer> pBuffer;
        pDevice->CreateBuffer(BuffDesc, &InitialData, &pBuffer);
        VERIFY_EXPR(pBuffer);
        return pBuffer;
    }

    RefCntAutoPtr<IBuffer> CreateIndexBuffer(const Uint32* Indices, Uint32 NumIndices)
    {
        BufferDesc BuffDesc;
        BuffDesc.Name          = "Test index buffer";
        BuffDesc.BindFlags     = BIND_INDEX_BUFFER;
        BuffDesc.uiSizeInBytes = sizeof(Uint32) * NumIndices;

        BufferData InitialData;
        InitialData.pData    = Indices;
        InitialData.DataSize = BuffDesc.uiSizeInBytes;

        auto* pDevice = TestingEnvironment::GetInstance()->GetDevice();

        RefCntAutoPtr<IBuffer> pBuffer;
        pDevice->CreateBuffer(BuffDesc, &InitialData, &pBuffer);
        VERIFY_EXPR(pBuffer);
        return pBuffer;
    }

    RefCntAutoPtr<IBuffer> CreateIndirectDrawArgsBuffer(const Uint32* Data, Uint32 DataSize)
    {
        BufferDesc BuffDesc;
        BuffDesc.Name          = "Test index buffer";
        BuffDesc.BindFlags     = BIND_INDIRECT_DRAW_ARGS;
        BuffDesc.uiSizeInBytes = DataSize;

        BufferData InitialData;
        InitialData.pData    = Data;
        InitialData.DataSize = BuffDesc.uiSizeInBytes;

        auto* pDevice = TestingEnvironment::GetInstance()->GetDevice();

        RefCntAutoPtr<IBuffer> pBuffer;
        pDevice->CreateBuffer(BuffDesc, &InitialData, &pBuffer);
        VERIFY_EXPR(pBuffer);
        return pBuffer;
    }

    static void TestDynamicBufferUpdates(IShader*                      pVS,
                                         IShader*                      pPS,
                                         IBuffer*                      pDynamicCB0,
                                         IBuffer*                      pDynamicCB1,
                                         IBuffer*                      pImmutableCB,
                                         SHADER_RESOURCE_VARIABLE_TYPE DynamicCB0Type,
                                         SHADER_RESOURCE_VARIABLE_TYPE DynamicCB1Type,
                                         SHADER_RESOURCE_VARIABLE_TYPE ImmutableCBType);

    static RefCntAutoPtr<IPipelineState> sm_pDrawProceduralPSO;
    static RefCntAutoPtr<IPipelineState> sm_pDrawPSO;
    static RefCntAutoPtr<IPipelineState> sm_pDraw_2xStride_PSO;
    static RefCntAutoPtr<IPipelineState> sm_pDrawInstancedPSO;
};

RefCntAutoPtr<IPipelineState> DrawCommandTest::sm_pDrawProceduralPSO;
RefCntAutoPtr<IPipelineState> DrawCommandTest::sm_pDrawPSO;
RefCntAutoPtr<IPipelineState> DrawCommandTest::sm_pDraw_2xStride_PSO;
RefCntAutoPtr<IPipelineState> DrawCommandTest::sm_pDrawInstancedPSO;

TEST_F(DrawCommandTest, DrawProcedural)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawProceduralPSO);

    DrawAttribs drawAttrs{6, DRAW_FLAG_VERIFY_ALL};
    pContext->Draw(drawAttrs);

    Present();
}


// Non-indexed draw calls


TEST_F(DrawCommandTest, Draw)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        Vert[0], Vert[1], Vert[2],
        Vert[3], Vert[4], Vert[5]
    };
    // clang-format on

    auto     pVB       = CreateVertexBuffer(Triangles, sizeof(Triangles));
    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {0};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{6, DRAW_FLAG_VERIFY_ALL};
    pContext->Draw(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, Draw_StartVertex)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, // Skip 2 vertices using StartVertexLocation
        Vert[0], Vert[1], Vert[2],
        Vert[3], Vert[4], Vert[5]
    };
    // clang-format on

    auto     pVB       = CreateVertexBuffer(Triangles, sizeof(Triangles));
    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {0};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{6, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.StartVertexLocation = 2;
    pContext->Draw(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, Draw_VBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, // Skip 3 vertices using buffer offset
        Vert[0], Vert[1], Vert[2],
        Vert[3], Vert[4], Vert[5]
    };
    // clang-format on

    auto     pVB       = CreateVertexBuffer(Triangles, sizeof(Triangles));
    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {3 * sizeof(Vertex)};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{6, DRAW_FLAG_VERIFY_ALL};
    pContext->Draw(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, Draw_StartVertex_VBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, // Skip 3 vertices using buffer offset
        {}, {},     // Skip 2 vertices using StartVertexLocation
        Vert[0], Vert[1], Vert[2],
        Vert[3], Vert[4], Vert[5]
    };
    // clang-format on

    auto     pVB       = CreateVertexBuffer(Triangles, sizeof(Triangles));
    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {3 * sizeof(Vertex)};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{6, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.StartVertexLocation = 2;
    pContext->Draw(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, Draw_StartVertex_VBOffset_2xStride)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDraw_2xStride_PSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {},     // Skip 3 * sizeof(Vertex) using buffer offset
        {}, {}, {}, {}, // Skip 2 vertices using StartVertexLocation
        Vert[0], {}, Vert[1], {}, Vert[2], {}, 
        Vert[3], {}, Vert[4], {}, Vert[5], {}
    };
    // clang-format on

    auto     pVB       = CreateVertexBuffer(Triangles, sizeof(Triangles));
    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {3 * sizeof(Vertex)};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{6, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.StartVertexLocation = 2;
    pContext->Draw(drawAttrs);

    Present();
}



// Indexed draw calls (glDrawElements/DrawIndexed)

TEST_F(DrawCommandTest, DrawIndexed)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        Vert[0], {}, Vert[1], {}, {}, Vert[2],
        Vert[3], {}, {}, Vert[5], Vert[4]
    };
    Uint32 Indices[] = {2,4,7, 8,12,11};
    // clang-format on

    auto pVB = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pIB = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {0};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{6, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    pContext->DrawIndexed(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DrawIndexed_IBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        Vert[0], {}, Vert[1], {}, {}, Vert[2],
        Vert[3], {}, {}, Vert[5], Vert[4]
    };
    Uint32 Indices[] = {0,0,0,0, 2,4,7, 8,12,11}; // Skip 4 indices using index buffer offset
    // clang-format on

    auto pVB = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pIB = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {0};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, sizeof(Uint32) * 4, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{6, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    pContext->DrawIndexed(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DrawIndexed_IBOffset_BaseVertex)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    Uint32 bv = 2; // Base vertex
    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        Vert[0], {}, Vert[1], {}, {}, Vert[2],
        Vert[3], {}, {}, Vert[5], Vert[4]
    };
    Uint32 Indices[] = {0,0,0,0, 2-bv,4-bv,7-bv, 8-bv,12-bv,11-bv};
    // clang-format on

    auto pVB = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pIB = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {0};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, sizeof(Uint32) * 4, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{6, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.BaseVertex = bv;
    pContext->DrawIndexed(drawAttrs);

    Present();
}


// Instanced non-indexed draw calls (glDrawArraysInstanced/DrawInstanced)

TEST_F(DrawCommandTest, DrawInstanced)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        VertInst[0], VertInst[1], VertInst[2]
    };
    const float4 InstancedData[] = 
    {
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{3, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances = 2; // Draw two instances of the same triangle
    pContext->Draw(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DrawInstanced_VBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, // Skip 2 vertices with VB offset
        VertInst[0], VertInst[1], VertInst[2]
    };
    const float4 InstancedData[] = 
    {
        {}, {}, {}, // Skip 3 instances with VB offset
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {2 * sizeof(Vertex), 3 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{3, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances = 2; // Draw two instances of the same triangle
    pContext->Draw(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DrawInstanced_StartVertex)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, {}, // Skip 4 vertices with start vertex
        VertInst[0], VertInst[1], VertInst[2]
    };
    const float4 InstancedData[] = 
    {
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{3, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances        = 2; // Draw two instances of the same triangle
    drawAttrs.StartVertexLocation = 4; // Skip 4 vertices
    pContext->Draw(drawAttrs);

    Present();
}


// Instanced draw calls with first instance (glDrawArraysInstancedBaseInstance/DrawInstanced)

TEST_F(DrawCommandTest, DrawInstanced_FirstInstance)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        VertInst[0], VertInst[1], VertInst[2]
    };
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, // Skip 4 instances with FirstInstanceLocation
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{3, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4; // Skip 4 instances
    pContext->Draw(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DrawInstanced_FirstInstance_VBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, // Skip 3 vertices with buffer offset
        VertInst[0], VertInst[1], VertInst[2]
    };
    const float4 InstancedData[] = 
    {
        {}, {},         // Skip 2 instances with buffer offset
        {}, {}, {}, {}, // Skip 4 instances with FirstInstanceLocation
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {3 * sizeof(Vertex), 2 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{3, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4; // Skip 4 instances
    pContext->Draw(drawAttrs);

    Present();
}


TEST_F(DrawCommandTest, DrawInstanced_FirstInstance_BaseVertex_FirstIndex_VBOffset_IBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, {}, // Skip 4 vertices with VB offset
        {}, {}, {},     // Skip 3 vertices with StartVertexLocation
        VertInst[0], VertInst[1], VertInst[2]
    };
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, {}, // Skip 5 instances with VB offset
        {}, {}, {}, {},     // Skip 4 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {4 * sizeof(Vertex), 5 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    DrawAttribs drawAttrs{3, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4;
    drawAttrs.StartVertexLocation   = 3;
    pContext->Draw(drawAttrs);

    Present();
}

// Instanced indexed draw calls (glDrawElementsInstanced/DrawIndexedInstanced)

TEST_F(DrawCommandTest, DrawIndexedInstanced)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {4, 2, 7};
    const float4 InstancedData[] = 
    {
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances = 2;
    pContext->DrawIndexed(drawAttrs);

    Present();
}


TEST_F(DrawCommandTest, DrawIndexedInstanced_IBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {0,0,0,0,0, 4, 2, 7};
    const float4 InstancedData[] = 
    {
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 5 * sizeof(Uint32), RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances = 2;
    pContext->DrawIndexed(drawAttrs);

    Present();
}


TEST_F(DrawCommandTest, DrawIndexedInstanced_VBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, // Skip 2 vertices with VBOffset
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {4, 2, 7};
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, // Skip 4 instances with VB offset
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {2 * sizeof(Vertex), 4 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances = 2;
    pContext->DrawIndexed(drawAttrs);

    Present();
}


TEST_F(DrawCommandTest, DrawIndexedInstanced_FirstIndex)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {0,0,0,0,0, 4, 2, 7};
    const float4 InstancedData[] = 
    {
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances       = 2;
    drawAttrs.FirstIndexLocation = 5;
    pContext->DrawIndexed(drawAttrs);

    Present();
}


// Instanced indexed draw calls with first instance (glDrawElementsInstancedBaseInstance/DrawInstanced)


TEST_F(DrawCommandTest, DrawIndexedInstanced_FirstInstance)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {4, 2, 7};
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, // Skip 4 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4; // Skip 4 instances
    pContext->DrawIndexed(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DrawIndexedInstanced_FirstInstance_IBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {0,0,0,0, 4, 2, 7};
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, // Skip 4 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 4 * sizeof(Uint32), RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4; // Skip 4 instances
    pContext->DrawIndexed(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DrawIndexedInstanced_FirstInstance_VBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, {}, // Skip 4 vertices with VB offset
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {4, 2, 7};
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, {}, // Skip 5 instances with VB offset
        {}, {}, {}, {}, // Skip 4 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {4 * sizeof(Vertex), 5 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4; // Skip 4 instances
    pContext->DrawIndexed(drawAttrs);

    Present();
}


TEST_F(DrawCommandTest, DrawIndexedInstanced_FirstInstance_IBOffset_FirstIndex)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {0,0,0,0, 0,0,0, 4, 2, 7};
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, // Skip 4 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 4 * sizeof(Uint32), RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4; // Skip 4 instances
    drawAttrs.FirstIndexLocation    = 3;
    pContext->DrawIndexed(drawAttrs);

    Present();
}



// Instanced draw commands with base vertex (glDrawElementsInstancedBaseVertex/DrawInstanced)

TEST_F(DrawCommandTest, DrawIndexedInstanced_BaseVertex)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {},     // Skip 3 vertices with BaseVertex
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {4, 2, 7};
    const float4 InstancedData[] = 
    {
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {0, 0};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances = 2;
    drawAttrs.BaseVertex   = 3;
    pContext->DrawIndexed(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DrawIndexedInstanced_FirstInstance_BaseVertex_VBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, {}, // Skip 4 vertices with VB offset
        {}, {}, {},     // Skip 3 vertices with BaseVertex
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {4, 2, 7};
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, {}, // Skip 5 instances with VB offset
        {}, {}, {}, {}, // Skip 4 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {4 * sizeof(Vertex), 5 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4; // Skip 4 instances
    drawAttrs.BaseVertex            = 3;
    pContext->DrawIndexed(drawAttrs);

    Present();
}


TEST_F(DrawCommandTest, DrawIndexedInstanced_FirstInstance_BaseVertex_FirstIndex_VBOffset_IBOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, {}, // Skip 4 vertices with VB offset
        {}, {}, {},     // Skip 3 vertices with BaseVertex
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {0,0,0,0, 0,0,0, 4, 2, 7};
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, {}, // Skip 5 instances with VB offset
        {}, {}, {}, {}, // Skip 4 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {4 * sizeof(Vertex), 5 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 4 * sizeof(Uint32), RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    drawAttrs.NumInstances          = 2;
    drawAttrs.FirstInstanceLocation = 4; // Skip 4 instances
    drawAttrs.BaseVertex            = 3;
    drawAttrs.FirstIndexLocation    = 3;
    pContext->DrawIndexed(drawAttrs);

    Present();
}


//  Indirect draw calls

TEST_F(DrawCommandTest, DrawInstancedIndirect_FirstInstance_BaseVertex_FirstIndex_VBOffset_IBOffset_InstOffset)
{
    auto* pEnv    = TestingEnvironment::GetInstance();
    auto* pDevice = pEnv->GetDevice();
    if (!pDevice->GetDeviceCaps().Features.IndirectRendering)
        GTEST_SKIP() << "Indirect rendering is not supported on this device";

    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, {}, // Skip 4 vertices with VB offset
        {}, {}, {},     // Skip 3 vertices with StartVertexLocation
        VertInst[0], VertInst[1], VertInst[2]
    };
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {}, {}, // Skip 5 instances with VB offset
        {}, {}, {}, {},     // Skip 4 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {4 * sizeof(Vertex), 5 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    Uint32 IndirectDrawData[] =
        {
            0, 0, 0, 0, 0, // Offset

            6, // NumVertices
            2, // NumInstances
            3, // StartVertexLocation
            4  // FirstInstanceLocation
        };
    auto pIndirectArgsBuff = CreateIndirectDrawArgsBuffer(IndirectDrawData, sizeof(IndirectDrawData));

    DrawIndirectAttribs drawAttrs{DRAW_FLAG_VERIFY_ALL, RESOURCE_STATE_TRANSITION_MODE_TRANSITION};
    drawAttrs.IndirectDrawArgsOffset = 5 * sizeof(Uint32);
    pContext->DrawIndirect(drawAttrs, pIndirectArgsBuff);

    Present();
}

TEST_F(DrawCommandTest, DrawIndexedInstancedIndirect_FirstInstance_BaseVertex_FirstIndex_VBOffset_IBOffset_InstOffset)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawInstancedPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        {}, {}, {}, {}, // Skip 4 vertices with VB offset
        {}, {}, {},     // Skip 3 vertices with BaseVertex
        {}, {},
        VertInst[1], {}, VertInst[0], {}, {}, VertInst[2]
    };
    Uint32 Indices[] = {0,0,0, 0,0,0,0, 4, 2, 7};
    const float4 InstancedData[] = 
    {
        {}, {}, {}, {},     // Skip 4 instances with VB offset
        {}, {}, {}, {}, {}, // Skip 5 instances with FirstInstance
        float4{0.5f,  0.5f,  -0.5f, -0.5f},
        float4{0.5f,  0.5f,  +0.5f, -0.5f}
    };
    // clang-format on

    auto pVB     = CreateVertexBuffer(Triangles, sizeof(Triangles));
    auto pInstVB = CreateVertexBuffer(InstancedData, sizeof(InstancedData));
    auto pIB     = CreateIndexBuffer(Indices, _countof(Indices));

    IBuffer* pVBs[]    = {pVB, pInstVB};
    Uint32   Offsets[] = {4 * sizeof(Vertex), 4 * sizeof(float4)};
    pContext->SetVertexBuffers(0, _countof(pVBs), pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 3 * sizeof(Uint32), RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    Uint32 IndirectDrawData[] =
        {
            0, 0, 0, 0, 0, // Offset

            6, // NumIndices
            2, // NumInstances
            4, // FirstIndexLocation
            3, // BaseVertex
            5, // FirstInstanceLocation
        };
    auto pIndirectArgsBuff = CreateIndirectDrawArgsBuffer(IndirectDrawData, sizeof(IndirectDrawData));

    DrawIndexedIndirectAttribs drawAttrs{VT_UINT32, DRAW_FLAG_VERIFY_ALL, RESOURCE_STATE_TRANSITION_MODE_TRANSITION};
    drawAttrs.IndirectDrawArgsOffset = 5 * sizeof(Uint32);
    pContext->DrawIndexedIndirect(drawAttrs, pIndirectArgsBuff);

    Present();
}

void DrawCommandTest::TestDynamicBufferUpdates(IShader*                      pVS,
                                               IShader*                      pPS,
                                               IBuffer*                      pDynamicCB0,
                                               IBuffer*                      pDynamicCB1,
                                               IBuffer*                      pImmutableCB,
                                               SHADER_RESOURCE_VARIABLE_TYPE DynamicCB0Type,
                                               SHADER_RESOURCE_VARIABLE_TYPE DynamicCB1Type,
                                               SHADER_RESOURCE_VARIABLE_TYPE ImmutableCBType)
{
    auto* pEnv       = TestingEnvironment::GetInstance();
    auto* pContext   = pEnv->GetDeviceContext();
    auto* pDevice    = pEnv->GetDevice();
    auto* pSwapChain = pEnv->GetSwapChain();

    GraphicsPipelineStateCreateInfo PSOCreateInfo;

    auto& PSODesc          = PSOCreateInfo.PSODesc;
    auto& GraphicsPipeline = PSOCreateInfo.GraphicsPipeline;

    PSODesc.Name = "Draw command test - dynamic buffer update";

    PSODesc.PipelineType                          = PIPELINE_TYPE_GRAPHICS;
    GraphicsPipeline.NumRenderTargets             = 1;
    GraphicsPipeline.RTVFormats[0]                = pSwapChain->GetDesc().ColorBufferFormat;
    GraphicsPipeline.PrimitiveTopology            = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
    GraphicsPipeline.RasterizerDesc.CullMode      = CULL_MODE_NONE;
    GraphicsPipeline.DepthStencilDesc.DepthEnable = False;

    ShaderResourceVariableDesc Variables[] =
        {
            {SHADER_TYPE_VERTEX, "DynamicCB0", DynamicCB0Type},
            {SHADER_TYPE_VERTEX, "DynamicCB1", DynamicCB1Type},
            {SHADER_TYPE_VERTEX, "ImmutableCB", ImmutableCBType} //
        };
    PSODesc.ResourceLayout.NumVariables = _countof(Variables);
    PSODesc.ResourceLayout.Variables    = Variables;

    PSOCreateInfo.pVS = pVS;
    PSOCreateInfo.pPS = pPS;

    RefCntAutoPtr<IPipelineState> pPSO;
    pDevice->CreateGraphicsPipelineState(PSOCreateInfo, &pPSO);

    if (DynamicCB0Type == SHADER_RESOURCE_VARIABLE_TYPE_STATIC)
        pPSO->GetStaticVariableByName(SHADER_TYPE_VERTEX, "DynamicCB0")->Set(pDynamicCB0);
    if (DynamicCB1Type == SHADER_RESOURCE_VARIABLE_TYPE_STATIC)
        pPSO->GetStaticVariableByName(SHADER_TYPE_VERTEX, "DynamicCB1")->Set(pDynamicCB1);
    if (ImmutableCBType == SHADER_RESOURCE_VARIABLE_TYPE_STATIC)
        pPSO->GetStaticVariableByName(SHADER_TYPE_VERTEX, "ImmutableCB")->Set(pImmutableCB);

    RefCntAutoPtr<IShaderResourceBinding> pSRB;
    pPSO->CreateShaderResourceBinding(&pSRB, true);

    if (DynamicCB0Type != SHADER_RESOURCE_VARIABLE_TYPE_STATIC)
        pSRB->GetVariableByName(SHADER_TYPE_VERTEX, "DynamicCB0")->Set(pDynamicCB0);
    if (DynamicCB1Type != SHADER_RESOURCE_VARIABLE_TYPE_STATIC)
        pSRB->GetVariableByName(SHADER_TYPE_VERTEX, "DynamicCB1")->Set(pDynamicCB1);
    if (ImmutableCBType != SHADER_RESOURCE_VARIABLE_TYPE_STATIC)
        pSRB->GetVariableByName(SHADER_TYPE_VERTEX, "ImmutableCB")->Set(pImmutableCB);

    SetRenderTargets(pPSO);

    pContext->CommitShaderResources(pSRB, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    {
        MapHelper<float4> PosData{pContext, pDynamicCB0, MAP_WRITE, MAP_FLAG_DISCARD};
        for (Uint32 i = 0; i < 3; ++i)
        {
            PosData[i] = Pos[i];
        }
    }

    {
        MapHelper<float4> ColorData{pContext, pDynamicCB1, MAP_WRITE, MAP_FLAG_DISCARD};
        for (Uint32 i = 0; i < 3; ++i)
            ColorData[i] = Color[i];
    }

    DrawAttribs drawAttrs{3, DRAW_FLAG_VERIFY_ALL};
    pContext->Draw(drawAttrs);

    {
        MapHelper<float4> PosData{pContext, pDynamicCB0, MAP_WRITE, MAP_FLAG_DISCARD};
        for (Uint32 i = 0; i < 3; ++i)
            PosData[i] = Pos[3 + i];
    }

    pContext->Draw(drawAttrs);

    Present();
}

// Test dynamic buffer update between two draw calls without committing and SRB
TEST_F(DrawCommandTest, DynamicUniformBufferUpdates)
{
    auto* pEnv    = TestingEnvironment::GetInstance();
    auto* pDevice = pEnv->GetDevice();

    ShaderCreateInfo ShaderCI;
    ShaderCI.SourceLanguage             = SHADER_SOURCE_LANGUAGE_HLSL;
    ShaderCI.ShaderCompiler             = pEnv->GetDefaultCompiler(ShaderCI.SourceLanguage);
    ShaderCI.UseCombinedTextureSamplers = true;

    RefCntAutoPtr<IShader> pVS;
    {
        ShaderCI.Desc.ShaderType = SHADER_TYPE_VERTEX;
        ShaderCI.EntryPoint      = "main";
        ShaderCI.Desc.Name       = "Draw command test dynamic buffer updates - VS";
        ShaderCI.Source          = HLSL::DrawTest_DynamicBuffers.c_str();
        pDevice->CreateShader(ShaderCI, &pVS);
        ASSERT_NE(pVS, nullptr);
    }

    RefCntAutoPtr<IShader> pPS;
    {
        ShaderCI.Desc.ShaderType = SHADER_TYPE_PIXEL;
        ShaderCI.EntryPoint      = "main";
        ShaderCI.Desc.Name       = "Draw command test dynamic buffer updates - PS";
        ShaderCI.Source          = HLSL::DrawTest_PS.c_str();
        pDevice->CreateShader(ShaderCI, &pPS);
        ASSERT_NE(pPS, nullptr);
    }

    RefCntAutoPtr<IBuffer> pDynamicCB0;
    RefCntAutoPtr<IBuffer> pDynamicCB1;
    RefCntAutoPtr<IBuffer> pImmutableCB;
    {
        BufferDesc BuffDesc;
        BuffDesc.Name           = "Dynamic buffer update test - dynamic CB0";
        BuffDesc.BindFlags      = BIND_UNIFORM_BUFFER;
        BuffDesc.Usage          = USAGE_DYNAMIC;
        BuffDesc.CPUAccessFlags = CPU_ACCESS_WRITE;
        BuffDesc.uiSizeInBytes  = sizeof(float) * 16;

        pDevice->CreateBuffer(BuffDesc, nullptr, &pDynamicCB0);
        ASSERT_NE(pDynamicCB0, nullptr);

        BuffDesc.Name = "Dynamic buffer update test - dynamic CB1";
        pDevice->CreateBuffer(BuffDesc, nullptr, &pDynamicCB1);
        ASSERT_NE(pDynamicCB1, nullptr);

        {
            BuffDesc.Usage          = USAGE_IMMUTABLE;
            BuffDesc.CPUAccessFlags = CPU_ACCESS_NONE;
            BuffDesc.Name           = "Dynamic buffer update test - immutable CB";

            float      Data[16] = {0, 1};
            BufferData InitialData;
            InitialData.pData    = Data;
            InitialData.DataSize = sizeof(Data);
            pDevice->CreateBuffer(BuffDesc, &InitialData, &pImmutableCB);
            ASSERT_NE(pImmutableCB, nullptr);
        }
    }

    for (Uint32 CB0Type = 0; CB0Type < SHADER_RESOURCE_VARIABLE_TYPE_NUM_TYPES; ++CB0Type)
    {
        for (Uint32 CB1Type = 0; CB1Type < SHADER_RESOURCE_VARIABLE_TYPE_NUM_TYPES; ++CB1Type)
        {
            for (Uint32 CB2Type = 0; CB2Type < SHADER_RESOURCE_VARIABLE_TYPE_NUM_TYPES; ++CB2Type)
            {

                TestDynamicBufferUpdates(pVS, pPS, pDynamicCB0, pDynamicCB1, pImmutableCB,
                                         static_cast<SHADER_RESOURCE_VARIABLE_TYPE>(CB0Type),
                                         static_cast<SHADER_RESOURCE_VARIABLE_TYPE>(CB1Type),
                                         static_cast<SHADER_RESOURCE_VARIABLE_TYPE>(CB2Type));
            }
        }
    }
}

TEST_F(DrawCommandTest, DynamicVertexBufferUpdate)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pDevice  = TestingEnvironment::GetInstance()->GetDevice();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    RefCntAutoPtr<IBuffer> pVB;
    {
        BufferDesc BuffDesc;
        BuffDesc.Name           = "Dynamic vertex buffer";
        BuffDesc.BindFlags      = BIND_VERTEX_BUFFER;
        BuffDesc.Usage          = USAGE_DYNAMIC;
        BuffDesc.CPUAccessFlags = CPU_ACCESS_WRITE;
        BuffDesc.uiSizeInBytes  = sizeof(Vertex) * 3;

        pDevice->CreateBuffer(BuffDesc, nullptr, &pVB);
        ASSERT_NE(pVB, nullptr);
    }

    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {0};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);

    {
        MapHelper<Vertex> VertData{pContext, pVB, MAP_WRITE, MAP_FLAG_DISCARD};
        for (Uint32 i = 0; i < 3; ++i)
            VertData[i] = Vert[i];
    }

    DrawAttribs drawAttrs{3, DRAW_FLAG_VERIFY_ALL};
    pContext->Draw(drawAttrs);

    {
        MapHelper<Vertex> VertData{pContext, pVB, MAP_WRITE, MAP_FLAG_DISCARD};
        for (Uint32 i = 0; i < 3; ++i)
            VertData[i] = Vert[3 + i];
    }
    pContext->Draw(drawAttrs);

    Present();
}

TEST_F(DrawCommandTest, DynamicIndexBufferUpdate)
{
    auto* pEnv     = TestingEnvironment::GetInstance();
    auto* pDevice  = TestingEnvironment::GetInstance()->GetDevice();
    auto* pContext = pEnv->GetDeviceContext();

    SetRenderTargets(sm_pDrawPSO);

    // clang-format off
    const Vertex Triangles[] =
    {
        Vert[0], Vert[1], Vert[2],
        Vert[3], Vert[5], Vert[4]
    };
    // clang-format on

    auto pVB = CreateVertexBuffer(Triangles, sizeof(Triangles));

    RefCntAutoPtr<IBuffer> pIB;
    {
        BufferDesc BuffDesc;
        BuffDesc.Name           = "Dynamic index buffer";
        BuffDesc.BindFlags      = BIND_INDEX_BUFFER;
        BuffDesc.Usage          = USAGE_DYNAMIC;
        BuffDesc.CPUAccessFlags = CPU_ACCESS_WRITE;
        BuffDesc.uiSizeInBytes  = sizeof(Uint32) * 3;

        pDevice->CreateBuffer(BuffDesc, nullptr, &pIB);
        ASSERT_NE(pIB, nullptr);
    }


    IBuffer* pVBs[]    = {pVB};
    Uint32   Offsets[] = {0};
    pContext->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_TRANSITION, SET_VERTEX_BUFFERS_FLAG_RESET);
    pContext->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    {
        MapHelper<Uint32> IndData{pContext, pIB, MAP_WRITE, MAP_FLAG_DISCARD};
        for (Uint32 i = 0; i < 3; ++i)
            IndData[i] = i;
    }

    DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
    pContext->DrawIndexed(drawAttrs);

    {
        MapHelper<Uint32> IndData{pContext, pIB, MAP_WRITE, MAP_FLAG_DISCARD};
        for (Uint32 i = 0; i < 3; ++i)
            IndData[i] = 3 + i;
    }

    pContext->DrawIndexed(drawAttrs);

    Present();
}


TEST_F(DrawCommandTest, DeferredContexts)
{
    auto* pEnv = TestingEnvironment::GetInstance();
    if (pEnv->GetNumDeferredContexts() == 0)
    {
        GTEST_SKIP() << "Deferred contexts are not supported by this device";
    }
    VERIFY(pEnv->GetNumDeferredContexts() >= 2, "At least two deferred contexts are expected");

    auto* pSwapChain    = pEnv->GetSwapChain();
    auto* pImmediateCtx = pEnv->GetDeviceContext();

    Uint32 Indices[] = {0, 1, 2, 3, 4, 5};
    auto   pVB       = CreateVertexBuffer(Vert, sizeof(Vert));
    auto   pIB       = CreateIndexBuffer(Indices, _countof(Indices));

    StateTransitionDesc Barriers[] = //
        {
            {pVB, RESOURCE_STATE_UNKNOWN, RESOURCE_STATE_VERTEX_BUFFER, true},
            {pIB, RESOURCE_STATE_UNKNOWN, RESOURCE_STATE_INDEX_BUFFER, true} //
        };
    pImmediateCtx->TransitionResourceStates(_countof(Barriers), Barriers);

    ITextureView* pRTVs[]      = {pSwapChain->GetCurrentBackBufferRTV()};
    const float   ClearColor[] = {0.f, 0.f, 0.f, 0.0f};
    pImmediateCtx->SetRenderTargets(1, pRTVs, nullptr, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);
    pImmediateCtx->ClearRenderTarget(pRTVs[0], ClearColor, RESOURCE_STATE_TRANSITION_MODE_TRANSITION);

    constexpr Uint32                                    NumThreads = 2;
    std::array<std::thread, NumThreads>                 WorkerThreads;
    std::array<RefCntAutoPtr<ICommandList>, NumThreads> CmdLists;
    std::array<ICommandList*, NumThreads>               CmdListPtrs;
    for (Uint32 i = 0; i < NumThreads; ++i)
    {
        WorkerThreads[i] = std::thread(
            [&](Uint32 thread_id) //
            {
                auto* pCtx = pEnv->GetDeviceContext(thread_id + 1);

                pCtx->SetRenderTargets(1, pRTVs, nullptr, RESOURCE_STATE_TRANSITION_MODE_VERIFY);

                IBuffer* pVBs[]    = {pVB};
                Uint32   Offsets[] = {0};
                pCtx->SetVertexBuffers(0, 1, pVBs, Offsets, RESOURCE_STATE_TRANSITION_MODE_VERIFY, SET_VERTEX_BUFFERS_FLAG_RESET);
                pCtx->SetIndexBuffer(pIB, 0, RESOURCE_STATE_TRANSITION_MODE_VERIFY);

                pCtx->SetPipelineState(sm_pDrawPSO);

                DrawIndexedAttribs drawAttrs{3, VT_UINT32, DRAW_FLAG_VERIFY_ALL};
                drawAttrs.FirstIndexLocation = 3 * thread_id;
                pCtx->DrawIndexed(drawAttrs);

                pCtx->FinishCommandList(&CmdLists[thread_id]);
                CmdListPtrs[thread_id] = CmdLists[thread_id];
            },
            i);
    }

    for (auto& t : WorkerThreads)
        t.join();

    pImmediateCtx->ExecuteCommandLists(NumThreads, CmdListPtrs.data());

    for (size_t i = 0; i < NumThreads; ++i)
        pEnv->GetDeviceContext(i + 1)->FinishFrame();

    Present();
}

} // namespace
