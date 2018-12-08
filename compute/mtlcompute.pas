unit mtlcompute;
//Metal Texture - draw a bitmap
{$mode objfpc}{$H+}
{$modeswitch objectivec1}
interface

uses
  MetalPipeline, MetalUtils, MetalControl, Metal,
  VectorMath, LResources, Dialogs,Classes, SysUtils, Graphics, math, strutils;

type
  TGPUTextureCompute = class
  private
    bmpHt, bmpWid: integer;
    OffsetX,OffsetY,Zoom: single;
    pngTex: MTLTextureProtocol;
    mtlControl: TMetalControl;
    shaderPipeline: TMetalPipeline;
    procedure LoadTex(fnm : string);
    procedure SetPipeline;
  public
    property BitmapHeight: integer read bmpHt;
    property BitmapWidth: integer read bmpWid;
    procedure DrawTex(); //must be called while TMetalControl is current context
    procedure SetPosition(xPixel, yPixel, zoomRatio: single);
    procedure GetPipeline(out pipeline: TMetalPipeline);
    constructor Create(fnm : string; fromView: TMetalControl); overload;
    constructor Create(fnm : string; fromView: TMetalControl; var existingPipeline: TMetalPipeline); overload;
    Destructor  Destroy; override;
  end;

implementation

procedure TGPUTextureCompute.GetPipeline(out pipeline: TMetalPipeline);
begin
     pipeline := shaderPipeline;
end;

procedure TGPUTextureCompute.SetPosition(xPixel, yPixel, zoomRatio: single);
begin
   OffsetX := xPixel;
   OffsetY := yPixel;
   Zoom := zoomRatio;
end;

{$DEFINE COMPUTE}
procedure TGPUTextureCompute.LoadTex(fnm: string);
var
 pngTexDesc: MTLTextureDescriptor;
 pngRegion: MTLRegion;
 px: TPicture;
 {$IFDEF COMPUTE}
 inTex: MTLTextureProtocol;
 options: TMetalPipelineOptions;
 computeShader : TMetalPipeline;
 threadgroupSize: MTLSize;
 threadgroupCount: MTLSize;
 {$ENDIF}
begin
 px := TPicture.Create;
 if (fnm='') or (not fileexists(fnm)) then
    fnm := ResourcePath('texture', 'png');
 try
    px.LoadFromFile(fnm);
 except
   px.Bitmap.Width:=0;
 end;
 if (px.Bitmap.PixelFormat <> pf32bit ) or (px.Bitmap.Width < 1) or (px.Bitmap.Height < 1) then begin
    writeln('Error loading 32-bit power-of-two bitmap '+fnm);
    exit;
 end;
 bmpWid := px.Bitmap.Width;
 bmpHt := px.Bitmap.Height;
 pngTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
 pngTexDesc.setTextureType(MTLTextureType2D);
 pngTexDesc.setPixelFormat(MTLPixelFormatBGRA8Unorm);
 pngTexDesc.setWidth(bmpWid);
 pngTexDesc.setHeight(bmpHt);
 pngTexDesc.setDepth(1);
 pngTexDesc.setUsage(MTLTextureUsageShaderWrite or MTLTextureUsageShaderRead);
 if (pngTex <> nil) then pngTex.release;
 pngTex := mtlControl.renderView.device.newTextureWithDescriptor(pngTexDesc);
 Fatal(pngTex = nil, 'newTextureWithDescriptor failed');
 pngRegion := MTLRegionMake2D(0, 0, bmpWid, bmpHt);
{$IFDEF COMPUTE}
    pngTexDesc.setUsage(MTLTextureUsageShaderRead);
    inTex := mtlControl.renderView.device.newTextureWithDescriptor(pngTexDesc);
    inTex.replaceRegion_mipmapLevel_withBytes_bytesPerRow(pngRegion, 0, PInteger(px.Bitmap.RawImage.Data), bmpWid*4);
    threadgroupSize := MTLSizeMake(16, 16, 1);
    threadgroupCount.width  := (bmpWid  + threadgroupSize.width -  1) div threadgroupSize.width;
    threadgroupCount.height := (bmpHt + threadgroupSize.height - 1) div threadgroupSize.height;
    threadgroupCount.depth := 1;
    options := TMetalPipelineOptions.Default;
    options.libraryName := ResourcePath('grayscale', 'metal');
    options.kernelFunction := 'grayscaleKernel';
    //options.kernelFunction := 'blurKernel';  //blur kernel
    computeShader := MTLCreatePipeline(options);
    MTLBeginCommand;
      MTLBeginEncoding(computeShader);
    	  MTLSetTexture(inTex, 0);
    	  MTLSetTexture(pngTex, 1);
    	  MTLSetDispatchThreadgroups(threadgroupCount, threadgroupSize);
      MTLEndEncoding;
    MTLEndCommand;
    computeShader.Free;
 {$ELSE}
  pngTex.replaceRegion_mipmapLevel_withBytes_bytesPerRow(pngRegion, 0, PInteger(px.Bitmap.RawImage.Data), bmpWid*4);
 {$ENDIF}
 px.Free;
end;

procedure TGPUTextureCompute.SetPipeline();
var
 options: TMetalPipelineOptions;
 shaderName: string;
begin
     if (shaderPipeline <> nil) then exit; //already set
     options := TMetalPipelineOptions.Default;
     shaderName := ResourceFolderPath + pathdelim + 'texture.metal';
     options.libraryName := shaderName;
     if not fileexists(shaderName) then begin
       writeln('Unable to find ' + shaderName);
     end;
     options.pipelineDescriptor := MTLCreatePipelineDescriptor;
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
     options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
     shaderPipeline := MTLCreatePipeline(options);
end;

constructor TGPUTextureCompute.Create(fnm : string; fromView: TMetalControl); overload;
begin
     shaderPipeline := nil;
     Create(fnm, fromView, shaderPipeline);
end;

constructor TGPUTextureCompute.Create(fnm : string; fromView: TMetalControl; var existingPipeline: TMetalPipeline); overload;
begin
 mtlControl := fromView;
 OffsetX := 0;
 OffsetY := 0;
 Zoom := 1;
 shaderPipeline := existingPipeline;
 setPipeline;
 LoadTex(fnm);
end;

type
  TVertUniforms = record //Uniforms for vertex shader
    viewportSize: TVec2;
  end;
  TAAPLVertex = record
    position: TVec2;
    // align each vertex attribute on 16 byte boundries
    padding: TVec2;
    color: TVec4;
  end;

function AAPLVertex(constref position: TVec2; constref color: TVec4): TAAPLVertex;
begin
    result.position := position;
    result.color := color;
end;

procedure TGPUTextureCompute.DrawTex();
var
 verts: array[0..3] of TAAPLVertex;
 vertUniforms: TVertUniforms;
 ZoomX,ZoomY: single;
begin
  setPipeline;
  MTLSetShader(shaderPipeline);
  if (pngTex = nil) then
    LoadTex('');
  ZoomX := bmpWid * Zoom * 0.5;
  ZoomY := bmpHt * Zoom * 0.5;
  verts[0] := AAPLVertex(V2(OffsetX - ZoomX, OffsetY + ZoomY), V4(0, 0, 0, 1));
  verts[1] := AAPLVertex(V2(OffsetX - ZoomX, OffsetY -ZoomY), V4(0, 1, 0, 1));
  verts[2] := AAPLVertex(V2(OffsetX + ZoomX, OffsetY + ZoomY), V4(1, 0, 0, 1));
  verts[3] := AAPLVertex(V2(OffsetX + ZoomX, OffsetY -ZoomY), V4(1, 1, 0, 1));
  vertUniforms.viewportSize := V2(mtlControl.Width, mtlControl.Height);
  MTLSetFragmentTexture(pngTex, 0);
  MTLSetVertexBytes(@verts, sizeof(verts), 0);
  MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
  MTLDraw(MTLPrimitiveTypeTriangleStrip, 0, 4);
end;

destructor TGPUTextureCompute.Destroy;
begin
  //call the parent destructor:
  inherited;
end;

end.
