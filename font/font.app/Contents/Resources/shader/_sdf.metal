#include <metal_stdlib>
//Signed Distance Field Font
//  xcrun -sdk macosx metal -c msdf.metal -o msdf.air
//  xcrun -sdk macosx metallib msdf.air -o msdf.metallib

using namespace metal;


typedef struct {
    float4 position; //vertex x,y position in pixel space, 
    float4 uv; //x,y in texture space, z is screenPxRange 
} VertexIn;

typedef struct {
    float4 position [[position]];
    float4 uv;
} VertexOut;

struct Uniforms {
	float2 viewportSize;
};

struct FragUniforms {
	float4 fontClr;
};

// Vertex shader
vertex VertexOut vertexShader(uint vertexID [[vertex_id]],
             const device VertexIn* vertices    [[ buffer(0) ]],
             const device Uniforms* uniforms [[ buffer(1) ]]){
    float2 pixelPosition = vertices[vertexID].position.xy;
    float2 viewportSize = uniforms->viewportSize;
    VertexOut out;
    pixelPosition -= (viewportSize/2.0);
    out.position = vector_float4((pixelPosition / (viewportSize/2)), 0.0, 1.0);
    out.uv = vertices[vertexID].uv;
    return out;
}

float median(float r, float g, float b) {
    return max(min(r, g), min(max(r, g), b));
}

// Fragment shader
fragment float4 fragmentShader(VertexOut in [[stage_in]],
texture2d<float> pngTexture [[ texture(0) ]],
const device FragUniforms* fragUniforms    	[[ buffer(1) ]]
 ) {
   constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
   float3 msd = pngTexture.sample(textureSampler, in.uv.xy).rgb;
   float sd = median(msd.r, msd.g, msd.b) - 0.5;
   //float screenPxDistance = sd/fwidth(sd);//old formula
   float screenPxDistance = in.uv.z * sd;//new formula
   float opacity = clamp(screenPxDistance + 0.5, 0.0, 1.0);
   return float4(fragUniforms->fontClr.rgb, opacity); //correct solution with renderbufferAttachment.blendingEnabled = YES
}

