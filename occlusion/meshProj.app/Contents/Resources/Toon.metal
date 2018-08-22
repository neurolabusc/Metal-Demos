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
	VertexOut.depth = VertexOut.position.z;
	VertexOut.color = VertexIn.color;
	VertexOut.vL = normalize(uniforms->lightPos);
	VertexOut.vV = -(uniforms->ModelViewMatrix*float4(VertexIn.position,1.0));
	VertexOut.vN = normalize((uniforms->NormalMatrix * VertexIn.normal));
	return VertexOut;
}

float stepmix(float edge0, float edge1, float E, float x) {
    float T = clamp(0.5 * (x - edge0) / E, 0.0, 1.0);
    return mix(edge0, edge1, T);
}

fragment GBufferData fragmentShader(VertexOut  in [[stage_in]]) {
	float Ambient = 0.4;
	float Diffuse = 0.7;
	float Specular = 0.6;
	float Roughness = 0.1;
	float Smooth = 1.0;
	float OutlineWidth = 0.0;
	float3 a = in.color.xyz;
	float3 d = a * Diffuse;
	a *= Ambient;
	float3 n = normalize(in.vN.xyz);
	float3 v = normalize(in.vV.xyz);
	float3 Eye = float3(0, 0, 1);
	float3 l = normalize(in.vL.xyz);
    float3 h = normalize(l + Eye);
	float df = dot(n, l);
    float specular =  pow(max(0.0,dot(n,h)),1.0/(Roughness * Roughness)) * Specular;
	float E = fwidth(specular) * Smooth;
	specular = smoothstep(0.3 - E, 0.3 + E, specular);
    const float A = 0.1;
    const float B = 0.3;
    const float C = 0.6;
    const float D = 1.0;
	E = fwidth(df) * Smooth;
	if (df < A + E) df = stepmix(A, B, E, df);
    else if (df < B + E) df = stepmix(B, C, E, df);
    else df = stepmix(C, D, E, df);
	float outline = mix(1.0, dot(float3(0, 0, 1),n), OutlineWidth);
	float3 color = a + df * d + specular;
	#define antialias
	#ifdef antialias
    E = fwidth(outline);
    if (outline > 0.5 - E && outline < 0.5 + E)
    {
        outline = smoothstep(0.5 - E, 0.5 + E, outline);
    }
    else
    {
        outline = step(0.5, outline);
    }
    #endif
	color.rgb *= outline;
	GBufferData bufferData;
	bufferData.color = float4(color, 1.0);
	bufferData.depth = in.depth;
	return bufferData;
}
