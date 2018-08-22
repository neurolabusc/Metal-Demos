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
	float d = VertexOut.position.z;
	d = 1.0 - d; //make more distant items DARKER
	if (d < 0.0)
		VertexOut.color =  float4(1.0, 0.0, 0.0, 1.0);
	else if (d > 1.0)
		VertexOut.color =  float4(0.0, 0.0, 1.0, 1.0);
	else
		VertexOut.color =  float4(d, d, d, 1.0);
	return VertexOut;
}

fragment float4 fragmentShader(VertexOut  in [[stage_in]]) {
	return float4(in.color);
}
