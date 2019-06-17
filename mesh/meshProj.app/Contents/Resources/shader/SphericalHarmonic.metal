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
	VertexOut.vL = normalize(uniforms->lightPos);
	VertexOut.vV = -(uniforms->ModelViewMatrix*float4(VertexIn.position,1.0));
	VertexOut.vN = normalize((uniforms->NormalMatrix * VertexIn.normal));
	return VertexOut;
}

float3 SH(float3 vNormal) {
//Spherical harmonics constants
const float C1 = 0.429043;
const float C2 = 0.511664;
const float C3 = 0.743125;
const float C4 = 0.886227;
const float C5 = 0.247708;
// Ramamoorthi, R., and P. Hanrahan. 2001b. "An Efficient Representation for Irradiance Environment Maps." In Proceedings of SIGGRAPH 2001, pp. 497–500.
// https://github.com/eskimoblood/processingSketches/blob/master/data/shader/shinyvert.glsl
// https://github.com/eskimoblood/processingSketches/blob/master/data/shader/shinyvert.glsl
// See table on page 397 of OpenGL programming Guide, 8th Edition Shreiner et al.

// Constants for Old Town Square lighting
const float3 L00 = float3( 0.871297, 0.875222, 0.864470);
const float3 L1m1 = float3( 0.175058, 0.245335, 0.312891);
const float3 L10 = float3( 0.034675, 0.036107, 0.037362);
const float3 L11 = float3(-0.004629, -0.029448, -0.048028);
const float3 L2m2 = float3(-0.120535, -0.121160, -0.117507);
const float3 L2m1 = float3( 0.003242, 0.003624, 0.007511);
const float3 L20 = float3(-0.028667, -0.024926, -0.020998);
const float3 L21 = float3(-0.077539, -0.086325, -0.091591);
const float3 L22 = float3(-0.161784, -0.191783, -0.219152);

	vNormal = float3(vNormal.x,vNormal.z,-vNormal.y);
	float3 diffuseColor =  C1 * L22 * (vNormal.x * vNormal.x - vNormal.y * vNormal.y) +
                    C3 * L20 * vNormal.z * vNormal.z +
                    C4 * L00 -
                    C5 * L20 +
                    2.0 * C1 * L2m2 * vNormal.x * vNormal.y +
                    2.0 * C1 * L21  * vNormal.x * vNormal.z +
                    2.0 * C1 * L2m1 * vNormal.y * vNormal.z +
                    2.0 * C2 * L11  * vNormal.x +
                    2.0 * C2 * L1m1 * vNormal.y +
                    2.0 * C2 * L10  * vNormal.z;
    return diffuseColor;
}

fragment float4 fragmentShader(VertexOut  in [[stage_in]]) {
    //return float4(in.color.rgb,1);//<- show front face
	float Ambient = 0.0;
	float Diffuse = 1.0;
	float Specular = 0.6;
	float Roughness = 0.1;
	float3 n = normalize(in.vN.xyz);
	float3 v = normalize(in.vV.xyz);
	float3 l = normalize(in.vL.xyz);
	float3 h = normalize(in.vL.xyz+v.xyz);
	float3 AmbientColour = in.color.rgb;
	float3 DiffuseColour = in.color.rgb  * Diffuse * SH(-reflect(n, l) );
	float3 SpecularColour = float3(1.0, 1.0, 1.0);
	float specular =  pow(max(0.0,dot(n,h)),1.0/(Roughness * Roughness));
	return float4(AmbientColour*Ambient + DiffuseColour +SpecularColour*specular* Specular, 1.0);
}
