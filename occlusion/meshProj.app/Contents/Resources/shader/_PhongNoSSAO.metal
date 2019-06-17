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
	float4 vL, vV, vN;
	//float depth;
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
	//VertexOut.position.z = 1.0 - VertexOut.position.z;
	VertexOut.color = VertexIn.color;
	VertexOut.vL = normalize(uniforms->lightPos);
	VertexOut.vV = -(uniforms->ModelViewMatrix*float4(VertexIn.position,1.0));
	VertexOut.vN = normalize((uniforms->NormalMatrix * VertexIn.normal));
	return VertexOut;
}

fragment float4 fragmentShader(VertexOut  in [[stage_in]]) {
    float Ambient = 0.4;
	float Diffuse = 0.7;
	float Specular = 0.6;
	float Roughness = 0.1;
	float3 n = normalize(in.vN.xyz);
	float3 v = normalize(in.vV.xyz);
	float3 h = normalize(in.vL.xyz+v.xyz);
	float diffuse = dot(in.vL.xyz,n);
	float3 AmbientColour = in.color.rgb;
	float3 DiffuseColour = in.color.rgb;
	float3 SpecularColour = float3(1.0, 1.0, 1.0);
	float specular =  pow(max(0.0,dot(n,h)),1.0/(Roughness * Roughness));
	return float4(AmbientColour*Ambient + DiffuseColour*diffuse*Diffuse +SpecularColour*specular* Specular, 1.0);
}
