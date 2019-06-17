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
	VertexOut.color = VertexIn.color;
	//VertexOut.vL = normalize(uniforms->lightPos);
	//VertexOut.vV = -(uniforms->ModelViewMatrix*float4(VertexIn.position,1.0));
	VertexOut.vN = normalize((uniforms->NormalMatrix * VertexIn.normal));
	return VertexOut;
}

fragment float4 fragmentShader(VertexOut  in [[stage_in]], texture2d<float> matCapTexture [[ texture(0) ]]) {
    constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
   	//return matCapTexture.sample(textureSampler, in.color.xy);
	float3 n = normalize(in.vN.xyz);
	float2 uv = n.xy * 0.5 + 0.5;
	float3 color = matCapTexture.sample(textureSampler, uv).bgr;
	return float4(color, 1.0);
}
