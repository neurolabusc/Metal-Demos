/*
 https://developer.apple.com/documentation/metal/hello_compute
Copyright © 2018 Apple Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <metal_stdlib>
#include <simd/simd.h>

using namespace metal;


typedef enum AAPLTextureIndex
{
    AAPLTextureIndexInput  = 0,
    AAPLTextureIndexOutput = 1,
} AAPLTextureIndex;


// Blur compute kernel
kernel void
blurKernel(texture2d<half, access::sample>  inTexture  [[texture(AAPLTextureIndexInput)]],
                texture2d<half, access::write> outTexture [[texture(AAPLTextureIndexOutput)]],
                uint2                          gid         [[thread_position_in_grid]])
{
    // Check if the pixel is within the bounds of the output texture
    if((gid.x >= outTexture.get_width()) || (gid.y >= outTexture.get_height()))
    {
        return;
    }
    float dx = 1.0/outTexture.get_width(); //pixel width as fraction of texture width
    float dy = 1.0/outTexture.get_height(); //pixel height as fraction of texture height
	float2 vx = float2(gid.xy) * float2(dx,dy); //pixel to fractional coordinates
	constexpr sampler textureSampler (mag_filter::linear,min_filter::linear);
	half4 inColor = inTexture.sample(textureSampler,vx+float2(+dx,+dy));
	inColor += inTexture.sample(textureSampler,vx+float2(+dx,-dy));
	inColor += inTexture.sample(textureSampler,vx+float2(-dx,+dy));
	inColor += inTexture.sample(textureSampler,vx+float2(-dx,-dy));
	inColor *= 0.25;
    //outTexture.write(half4(inColor.rgb, inColor.a), gid);
    outTexture.write(inColor, gid);
}


// Rec. 709 luma values for grayscale image conversion
constant half3 kRec709Luma = half3(0.2126, 0.7152, 0.0722);
// Grayscale compute kernel
kernel void
grayscaleKernel(texture2d<half, access::read>  inTexture  [[texture(AAPLTextureIndexInput)]],
                texture2d<half, access::write> outTexture [[texture(AAPLTextureIndexOutput)]],
                uint2                          gid         [[thread_position_in_grid]])
{
    if((gid.x >= outTexture.get_width()) || (gid.y >= outTexture.get_height())) {
        // Return early if the pixel is out of bounds
        return;
    }
    half4 inColor  = inTexture.read(gid);
    half  gray     = dot(inColor.rgb, kRec709Luma);
    outTexture.write(half4(gray, gray, gray, inColor.a), gid);
}

