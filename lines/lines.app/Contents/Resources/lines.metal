#include <metal_stdlib>

using namespace metal;

typedef struct {
    float4 position, color;
} VertexIn;

typedef struct {
    float4 position [[position]];
    float4 color;
} VertexOut;

struct Uniforms {
	float2 viewportSize, viewportSizeDivTwo, TwoDivViewportSize;
};

// Vertex shader
vertex VertexOut vertexShader(uint vertexID [[vertex_id]],
             constant VertexIn *vertices [[buffer(0)]],
             const device Uniforms* uniforms [[ buffer(1) ]]){
    //n.b. offset by 0.5 pixels, so line with width 1 and X=1 spans 0..1.0
    float2 pixelPosition = vertices[vertexID].position.xy - 0.5;
	VertexOut out;
	//division not known at compile time is "extremely slow"
	//http://devstreaming.apple.com/videos/wwdc/2016/606oluchfgwakjbymy8/606/606_advanced_metal_shader_optimization.pdf
	float2 viewportSizeDivTwo = uniforms->viewportSizeDivTwo;
	pixelPosition -= viewportSizeDivTwo;
    float2 TwoDivViewportSize = uniforms->TwoDivViewportSize;
	out.position = vector_float4((pixelPosition * TwoDivViewportSize), 0.0, 1.0);
	out.color = vertices[vertexID].color;
	return out;
}

// Fragment shader
fragment float4 fragmentShader(VertexOut in [[stage_in]]) {
	return in.color;
}

