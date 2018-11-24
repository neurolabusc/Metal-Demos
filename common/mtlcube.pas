unit mtlcube;
//Metal cube

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  MetalPipeline, MetalUtils, MetalControl, Metal,
  SimdUtils, VectorMath,
  Classes, SysUtils, Graphics, math, dialogs;

type
  TGPUCube = class
  private
    vertexBuffer: MTLBufferProtocol;
    shaderPipeline: TMetalPipeline;
    mtlControl: TMetalControl;
    fAzimuth, fElevation,SizeFrac : Single;
    scrnW, scrnH: integer;
    isRedraw, isTopLeft: boolean;
    procedure SetPipeline;
    procedure SetIsTopLeft(f: boolean);
    procedure SetSize(f: single);
    procedure SetAzimuth(f: single);
    procedure SetElevation(f: single);
    procedure  ScreenSize(Width,Height: integer);
    procedure CreateCube(sz: single);
  public
    property TopLeft : boolean read isTopLeft write SetIsTopLeft;
    property Azimuth : single read fAzimuth write SetAzimuth;
    property Elevation : single read fElevation write fElevation;
    property Size : single read SizeFrac write SetSize;
    procedure Draw(Width,Height: integer); //must be called while TOpenGLControl is current context
    constructor Create(Ctx: TMetalControl);
  end;

implementation

type
  TVertUniforms = record //Uniforms for vertex shader
    //viewportSize, viewportSizeDivTwo, TwoDivViewportSize: TVec2;
    modelViewProjectionMatrix: TMat4;
  end;
  TVtxClr = record
    vtx: TVec3;
    padding: single; // align each vertex attribute on 16 byte boundries
    clr: TVec4;
    //colorU4,pad1,pad2,pad3: TRGBA;
  end;
TVtxClrRA = array of TVtxClr;

procedure TGPUCube.SetPipeline();
var
 options: TMetalPipelineOptions;
 shaderName: string;
begin
     if (shaderPipeline <> nil) then exit; //already set
     options := TMetalPipelineOptions.Default;
     shaderName := ResourceFolderPath + pathdelim + 'cube.metal';
     if not fileexists(shaderName) then
        shaderName := ShaderDir + pathdelim +  '_Cube.metal';
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
     //options.pipelineDescriptor.setSampleCount(mtlControl.renderView.sampleCount);
     //options.pipelineDescriptor.setSampleCount(4);
     shaderPipeline := MTLCreatePipeline(options);
     MTLSetDepthStencil(shaderPipeline, MTLCompareFunctionLess, true);
end;

function setRGBAf(R,G,B,A: byte): TVec4;
begin
     result := Vec4(R/255, G/255, B/255, A/255);
end;

procedure MakeCube(sz: single; var vtxClrs: TVtxClrRA); //draw a cube of size sz
var
  nface: integer;
  clr : TVec4;
procedure vertex3f(x,y,z: single; rep: boolean = false);
begin
 vtxClrs[nface].vtx.X := x;
 vtxClrs[nface].vtx.Y := y;
 vtxClrs[nface].vtx.Z := z;
 vtxClrs[nface].clr := clr;
 nface := nface + 1;
 if not rep then exit;
 vtxClrs[nface] := vtxClrs[nface-1];
 nface := nface + 1;
end;
begin
  setlength(vtxClrs, 36);
  nface := 0;
  //bottom
  clr := setRGBAf(255, 255, 255, 255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(-sz, sz, -sz);
  vertex3f(sz, -sz, -sz);
  vertex3f(sz, sz, -sz, true);
  //top
  clr := setRGBAf(204,204,204,255);
  vertex3f(-sz, -sz, sz, true);
  vertex3f(sz, -sz, sz);
  vertex3f(-sz, sz, sz);
  vertex3f(sz, sz, sz, true);
  //front
  clr := setRGBAf(0,0,128,255);
  vertex3f(-sz, sz, -sz, true);
  vertex3f(-sz, sz, sz);
  vertex3f(sz, sz, -sz);
  vertex3f(sz, sz, sz, true);
  //back
  clr := setRGBAf(77,0,77,255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(sz, -sz, -sz);
  vertex3f(-sz, -sz, sz);
  vertex3f(sz, -sz, sz, true);
  //left
  clr := setRGBAf(153,0,0,255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(-sz, -sz, sz);
  vertex3f(-sz, sz, -sz);
  vertex3f(-sz, sz, sz, true);
  //right
  clr := setRGBAf(0,153,0,255);
  vertex3f(sz, -sz, -sz, true);
  vertex3f(sz, sz, -sz);
  vertex3f(sz, -sz, sz);
  vertex3f(sz, sz, sz, true);
end; //MakeCube()

procedure TGPUCube.SetAzimuth(f: single);
begin
  if (f <> fAzimuth) then isRedraw := true;
  fAzimuth := f;
end;

procedure TGPUCube.SetElevation(f: single);
begin
  if (f <> fElevation) then isRedraw := true;
  fElevation := f;
end;

procedure TGPUCube.SetIsTopLeft(f: boolean);
begin
     if (f <> isTopLeft) then isRedraw := true;
     isTopLeft := f;
end;

procedure  TGPUCube.SetSize(f: single);
begin
     if (f <> sizeFrac) then isRedraw := true;
     sizeFrac := f;
     if sizeFrac < 0.005 then sizeFrac := 0.005;
     if sizeFrac > 0.25 then sizeFrac := 0.25;
end;

procedure  TGPUCube.ScreenSize(Width,Height: integer);
begin
     if (Width = scrnW) and (Height = scrnH) then exit;
     scrnW := Width;
     scrnH := Height;
     isRedraw := true;
end;

constructor  TGPUCube.Create(Ctx: TMetalControl);
begin
     scrnH := 0;
     SizeFrac := 0.02;
     isRedraw := true;
     fAzimuth := 30;
     fElevation := -15;
     isTopLeft := false;
     mtlControl := Ctx;
     vertexBuffer := nil;
     shaderPipeline := nil;
end;

procedure  TGPUCube.CreateCube(sz: single);
var
  nface: integer;
  vtxClrs: TVtxClrRA;
begin
  if not isRedraw then exit;
  isRedraw := false;
  vtxClrs := nil;
  MakeCube(sz, vtxClrs);
  nface := Length(vtxClrs); //each face has 3 vertices
  if nface < 1 then exit;
  vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@vtxClrs[0], nface*sizeof(TVtxClr), MTLResourceStorageModeShared);
  setlength(vtxClrs,0);
end;

procedure  TGPUCube.Draw(Width,Height: integer);
var
  sz: single;
  vertUniforms: TVertUniforms;
  //modelViewProjectionMatrix,
    projectionMatrix, modelMatrix: TMat4;
begin
  ScreenSize(Width,Height);
  sz := min(ScrnW,ScrnH) * SizeFrac;
  if sz < 5 then exit;
  setPipeline;
  MTLSetShader(shaderPipeline);
  CreateCube(sz);
  modelMatrix := TMat4.Identity;
  projectionMatrix := TMat4.Ortho (0, ScrnW,0, ScrnH,-10*sz,10*sz);
  projectionMatrix *= TMat4.Translate(0,0,sz*8);
  projectionMatrix *= TMat4.Translate(1.8*sz,1.8*sz,0);
    modelMatrix *= TMat4.RotateX(DegToRad(90-fElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(180-fAzimuth));
  //projectionMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  //projectionMatrix *= TMat4.RotateZ(-DegToRad(fAzimuth));
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  MTLSetVertexBuffer(vertexBuffer, 0, 0);
  MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
  MTLDraw(MTLPrimitiveTypeTriangleStrip, 0, 36);
end;

end.

