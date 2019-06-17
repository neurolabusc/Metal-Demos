#include <metal_stdlib>

using namespace metal;

struct VertexIn {
	float3 position;
	float4 color;
	float4 normal;
};

struct VertexOut {
	float4 position [[position]];
	float4 color;
	float4 vN;
	float depth;
};

struct Uniforms {
	float4x4 modelViewProjectionMatrix;
	float4x4 ModelViewMatrix;
	float4x4 NormalMatrix;
	float4 lightPos;
};

struct GBufferData
{
	float4 color [[color(1), raster_order_group(0)]];
	float depth [[color(2), raster_order_group(0)]];
};

vertex VertexOut vertexShader(  unsigned int vertexID               [[ vertex_id ]],
                                const device VertexIn* verticies    [[ buffer(0) ]],
								const device Uniforms* uniforms    	[[ buffer(1) ]]
                                )
{
	VertexIn VertexIn = verticies[vertexID];
	VertexOut VertexOut;
	VertexOut.position = uniforms->modelViewProjectionMatrix * float4(VertexIn.position, 1);
	//VertexOut.position.z = 1.0 - VertexOut.position.z;
	VertexOut.color = VertexIn.color;
	VertexOut.vN = normalize((uniforms->NormalMatrix * VertexIn.normal));
	VertexOut.depth = VertexOut.position.z;
	return VertexOut;
}

fragment GBufferData fragmentShader(VertexOut  in [[stage_in]], texture2d<float> matCapTexture [[ texture(0) ]]) {
    constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
   	float3 n = normalize(in.vN.xyz);
	float2 uv = n.xy * 0.5 + 0.5;
	float3 color = matCapTexture.sample(textureSampler, uv).bgr;
	color *= in.color.rgb;
	GBufferData bufferData;
	bufferData.color = float4(color, 1.0);
    bufferData.depth = in.depth;
	return bufferData;
}
