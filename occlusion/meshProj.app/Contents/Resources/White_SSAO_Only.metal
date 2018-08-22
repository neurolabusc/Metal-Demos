#include <metal_stdlib>

using namespace metal;

struct VertexIn {
	float3 position;
	float4 color;
	float4 normal;
};

struct VertexOut {
	float4 position [[position]];
	float depth;
};

struct Uniforms {
	float4x4 modelViewProjectionMatrix;
	float4x4 ModelViewMatrix;
	float4x4 NormalMatrix;
	float4 lightPos;
};

vertex VertexOut vertexShader(  unsigned int vertexID               [[ vertex_id ]],
                                const device VertexIn* verticies    [[ buffer(0) ]],
								const device Uniforms* uniforms    	[[ buffer(1) ]]
                                )
{
	VertexIn VertexIn = verticies[vertexID];
	VertexOut VertexOut;
	VertexOut.position = uniforms->modelViewProjectionMatrix * float4(VertexIn.position, 1);
	VertexOut.depth = VertexOut.position.z;
	return VertexOut;
}

struct GBufferData
{
	float4 color [[color(1), raster_order_group(0)]];
	float depth [[color(2), raster_order_group(0)]];
};

fragment GBufferData fragmentShader(VertexOut  in [[stage_in]]) {
	GBufferData bufferData;
	bufferData.color = float4(1.0, 1.0, 1.0, 1.0);
	bufferData.depth = in.depth;
	return bufferData;
}
