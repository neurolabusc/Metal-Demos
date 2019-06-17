#include <metal_stdlib>
using namespace metal;

typedef struct {
	float4 position; //vertex x,y position in pixel space
} VertexIn;

typedef struct {
	float4 position [[position]];
	float4 textureUV;
} VertexOut;

struct FragUniforms {
	float aoRadius;
	float fracAO;
};

// Vertex shader
vertex VertexOut vertexShader(uint vertexID [[vertex_id]],
             constant VertexIn *vertices [[buffer(0)]]){
	float2 pixelPosition = vertices[vertexID].position.xy;
	VertexOut out;
	out.position = vector_float4(pixelPosition.xy, 0.0, 1.0);
	//screen space is -1..+1, color is 0..1
	out.textureUV = vector_float4((out.position.xy+1.0)*0.5, 0.0, 1.0);
	out.textureUV.y = 1.0 - out.textureUV.y;
	return out;
}

//general stuff
#define PI 3.14159265
#define samples 32 //ao sample count
#define aoclamp 0.25 //depth clamp - reduces haloing at screen edges
#define hasNoise true //use noise instead of pattern for sample dithering
#define noiseamount  0.0002 //dithering amount
#define diffarea 0.5 //self-shadowing reduction
#define gdisplace  0.4 //gauss bell center

typedef struct {
	texture2d<float> depthTexture; //depth map
	float2 texCoord; //pixel position
	float2 texture_size;
	float aoRadius;
	float fracAO;
} FragVars;

float2 rand(float2 coord, FragVars fVar) {
	float noiseX = ((fract(1.0-coord.x*(fVar.texture_size.x/2.0))*0.25)+(fract(coord.y*(fVar.texture_size.y/2.0))*0.75))*2.0-1.0;
	float noiseY = ((fract(1.0-coord.x*(fVar.texture_size.x/2.0))*0.75)+(fract(coord.y*(fVar.texture_size.y/2.0))*0.25))*2.0-1.0;
	if (hasNoise) {
		noiseX = clamp(fract(sin(dot(coord ,float2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;
		noiseY = clamp(fract(sin(dot(coord ,float2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;
	}
	return float2(noiseX,noiseY)*noiseamount;
}

//for orthographic depth is linear http://www.humus.name/temp/Linearize%20depth.txt
//for perspective to scale to 0..1
// GLlinZ = n * (z + 1.0) / (f + n - z * (f - n))
// MTLlinZ = z / (f - z * (f - n))
float readDepth(float2 coord, FragVars fVar) {
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
	float z =  ((fVar.depthTexture.sample(textureSampler, coord).x) );
	z = 1.0 - ((z + 0.87) * 0.534);
	return z;
}

float calAO(float depth, float2 coordwh, FragVars fVar) {
//n.b. Metal does not have inout variables, this explains difference wrt GLSL
	int far = 0;
	float depth2 = readDepth(fVar.texCoord+coordwh, fVar);
	float garea = 2.0; //gauss bell width
	float diff = (depth - depth2)*100.0; //depth difference (0-100)
	if (diff < gdisplace) //reduce left bell width to avoid self-shadowing
		garea = diffarea;
	else
		far = 1;
	float temp = pow(2.7182,-2.0*(diff-gdisplace)*(diff-gdisplace)/(garea*garea));
	if (far > 0) {
		depth2 = readDepth(fVar.texCoord-coordwh, fVar);
		float garea = 2.0; //gauss bell width
		float diff = (depth2- depth)*100.0; //depth difference (0-100)
		if (diff < gdisplace) //reduce left bell width to avoid self-shadowing
			garea = diffarea;
		else
			far = 1;
		float temp2 = pow(2.7182,-2.0*(diff-gdisplace)*(diff-gdisplace)/(garea*garea));
		temp += (1.0-temp)*temp2;
	}
	return temp;
}

float getAO(FragVars fVar) {
	float2 noise = rand(fVar.texCoord, fVar);
	float depth = readDepth(fVar.texCoord, fVar);
	float dd = (1.0-depth)*fVar.aoRadius;
	float w = (1.0 / fVar.texture_size.x)/clamp(depth,aoclamp,1.0)+(noise.x*(1.0-noise.x));
	float h = (1.0 / fVar.texture_size.y)/clamp(depth,aoclamp,1.0)+(noise.y*(1.0-noise.y));
	float ao = 0.0;
	float dl = PI*(3.0-sqrt(5.0));
	float dz = 1.0/float(samples);
	float l = 0.0;
	float z = 1.0 - dz/2.0;
	for (int i = 0; i <= samples; i ++) {
		float r = sqrt(1.0-z);
		ao += calAO(depth, float2(cos(l)*r*w*dd,sin(l)*r*h*dd), fVar);
		z = z - dz;
		l = l + dl;
	}
	ao /= float(samples);
	ao = clamp(ao, 0.0, 0.4) * 2.5; //threshold then LERP 0..1
	ao = smoothstep(0.0, 1.0, ao);
	ao = (1.0 - ao) * fVar.fracAO;
	//ao *= fVar.fracAO;
	return ao;
}

fragment float4 fragmentShader(VertexOut in [[stage_in]],
  texture2d<float> colorTexture [[ texture(0) ]],
  texture2d<float> depthTexture [[ texture(1) ]],
  const device FragUniforms* fragUniforms    	[[ buffer(0) ]]) {
	FragVars fVar;
	fVar.depthTexture = depthTexture;
	fVar.texCoord = in.textureUV.xy;
	fVar.texture_size = float2(depthTexture.get_width(), depthTexture.get_height());
	fVar.fracAO = fragUniforms->fracAO;
	fVar.aoRadius = fragUniforms->aoRadius;
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
	float4 clr = colorTexture.sample(textureSampler, in.textureUV.xy);
	if (clr.a == 0.0) discard_fragment();
	clr.rgb -= getAO(fVar);
	return clr;
}