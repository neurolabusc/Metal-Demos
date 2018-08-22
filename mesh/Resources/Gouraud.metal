#include <metal_stdlib>
//Gouraud per-vertex shader: poorer quality than per-fragment
// https://www.tomdalling.com/blog/modern-opengl/06-diffuse-point-lighting/
using namespace metal;

struct VertexIn {
	float3 position;
	float4 color;
	float4 normal;
};

struct VertexOut {
	float4 position [[position]];
	float4 color;
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
	float3 l = normalize(uniforms->lightPos.xyz);
	float3 v = normalize(-(uniforms->ModelViewMatrix*float4(VertexIn.position,1.0)).xyz);
	float3 n = normalize((uniforms->NormalMatrix * VertexIn.normal).xyz);
	float4 color = VertexIn.color;
    float Ambient = 0.4;
	float Diffuse = 0.7;
	float Specular = 0.6;
	float Roughness = 0.1;
	float3 h = normalize(l+v);
	float diffuse = dot(l,n);
	float3 AmbientColour = color.rgb;
	float3 DiffuseColour = color.rgb;
	float3 SpecularColour = float3(1.0, 1.0, 1.0);
	float specular =  pow(max(0.0,dot(n,h)),1.0/(Roughness * Roughness));
	VertexOut.color = float4(AmbientColour*Ambient + DiffuseColour*diffuse*Diffuse +SpecularColour*specular* Specular, 1.0);
	return VertexOut;
}

fragment float4 fragmentShader(VertexOut  in [[stage_in]]) {
	return float4(in.color);
}
