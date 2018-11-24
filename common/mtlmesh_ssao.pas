unit mtlmesh_ssao;
{$DEFINE SSAO}
{$mode objfpc}{$H+}
{$modeswitch objectivec1}
interface

uses
         VectorMath, MetalPipeline, MetalUtils, MetalControl, Metal,mesh,
         SysUtils, Math, SimdUtils;
const
  kDefaultDistance = 1.0;
  kMaxDistance = 2;
type
  TGPUMesh = class
      private
        indexBuffer, vertexBuffer: MTLBufferProtocol;
        fMeshColor: TRGBA;
        fAzimuth,fElevation, numVert, numIdx: integer;
        fDistance: single;
        fLightPos: TVec4;
        meshShader: TMetalPipeline;
        mtlControl: TMetalControl;
        fPerspective: boolean;
        {$IFDEF SSAO}
        screenWidth, screenHeight: integer;
        screenVerts: array[0..3] of TVec4;
        MeshRenderPassDescriptor: MTLRenderPassDescriptor;
        outputColorTexture, outputDepthTexture: MTLTextureProtocol;
        fFracAO: single;
        SSAORenderPassDescriptor: MTLRenderPassDescriptor;
        ssaoShader: TMetalPipeline;
        {$ENDIF}
        fMeshName: string;
      private
             procedure Prepare();
      public
        property MeshName: string read fMeshName;
        property Perspective: boolean read fPerspective write fPerspective;
        {$IFDEF SSAO}property Occlusion: single read fFracAO write fFracAO;{$ENDIF}
        property MeshColor: TRGBA read fMeshColor write fMeshColor;
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        constructor Create(fromView: TMetalControl); overload;
        constructor Create(fromView: TMetalControl; InitMeshName: string); overload;
        procedure Paint();
        procedure OpenMesh(Filename: string; isSwapYZ: boolean = true);
        procedure SetShader(shaderName: string);
        procedure SaveBmp(filename: string);
  end;

implementation

Type
TVertUniforms = record //Uniforms for vertex shader
  modelViewProjectionMatrix: TMat4;
  modelViewMatrix: TMat4;
  normalMatrix: TMat4;
  lightPos: TVec4;
end;

TSSAOFragUniforms = record
  aoRadius, fracAO: TScalar;
end;

TVertVertex = record //Each vertex defines a location and color
  position: TVec3;
  padding: array[0..0] of TScalar;
  color: TVec4;
  normal: TVec4;
end;

procedure TGPUMesh.SaveBmp(filename: string);
begin
     MTLWriteTextureToFile(pChar(filename));
end;

procedure TGPUMesh.SetShader(shaderName: string);
var
 options: TMetalPipelineOptions;
begin
  options := TMetalPipelineOptions.Default;
  options.libraryName := shaderName;
  if not fileexists(shaderName) then
     writeln('Unable to find shader ' + shaderName);
  options.pipelineDescriptor := MTLCreatePipelineDescriptor;
  {$IFDEF SSAO}
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setPixelFormat(MTLPixelFormatInvalid);
	options.pipelineDescriptor.colorAttachmentAtIndex(1).setPixelFormat(MTLPixelFormatBGRA8Unorm);
	options.pipelineDescriptor.colorAttachmentAtIndex(2).setPixelFormat(MTLPixelFormatR32Float);
  {$ELSE}
	options.pipelineDescriptor := MTLCreatePipelineDescriptor;
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
	options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
  {$ENDIF}
  meshShader := MTLCreatePipeline(options);
  MTLSetDepthStencil(meshShader, MTLCompareFunctionLess, true);
end;

function VertVertex(x, y, z: TScalar; clr: TRGBA; norm: TPoint3f): TVertVertex;
begin
     result.position := V3(x,y,z);
     result.color := V4(clr.r/255, clr.g/255, clr.b/255, 1);
     result.normal := V4(norm.X, norm.y, norm.Z, 0.0);
end;

procedure TGPUMesh.Prepare();
var
    options: TMetalPipelineOptions;
    shaderName: string;
begin
  {$IFDEF SSAO}
  shaderName := ResourceFolderPath+pathdelim+'SphericalHarmonic.metal';
  SetShader(shaderName);
  shaderName := ResourceFolderPath+pathdelim+'_SSAO.metal';
  options := TMetalPipelineOptions.Default;
  options.libraryName := shaderName;
  if not fileexists(shaderName) then
     writeln('Unable to find shader ' + shaderName);
  options.pipelineDescriptor := MTLCreatePipelineDescriptor;
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
  options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
  ssaoShader := MTLCreatePipeline(options);  //screen space ambient occlusion
  screenWidth := -1;
  screenHeight := -1;
  {$ELSE}
  shaderName := ResourceFolderPath+pathdelim+'Phong.metal';
  SetShader(shaderName);
  {$ENDIF}
end;

constructor TGPUMesh.Create(fromView: TMetalControl; InitMeshName: string);  overload;
begin
  mtlControl := fromView;
  fDistance := kDefaultDistance;
  fMeshName := InitMeshName;
  fAzimuth := 110;
  fElevation := 30;
  fLightPos := Vec4(0.0 ,0.707, 0.707, 0.0);
  fMeshColor.r := 210;
  fMeshColor.g := 148;
  fMeshColor.b := 148;
  fPerspective := false;
  vertexBuffer := nil;
  meshShader := nil;
  {$IFDEF SSAO}
  fFracAO := 0.25;
  screenVerts[0] := V4(-1, -1, 0, 1);
  screenVerts[1] := V4(-1, +1, 0, 1);
  screenVerts[2] := V4(+1, -1, 0, 1);
  screenVerts[3] := V4(+1, +1, 0, 1);
  meshRenderPassDescriptor := nil;
  SSAORenderPassDescriptor := nil;
  {$ENDIF}
end;

constructor TGPUMesh.Create(fromView: TMetalControl);  overload;
begin
  {$IFDEF SSAO}
  fMeshName := ResourceFolderPath+pathdelim+'brain.mz3';
  {$ELSE}
  //fMeshName := ResourceFolderPath+pathdelim+'teapot.ply';
  fMeshName := '';
  {$ENDIF}
  Create(fromView, fMeshName);
end;

procedure TGPUMesh.OpenMesh(Filename: string; isSwapYZ: boolean = true);
var
  faces: TFaces;
  verts, vNorm: TVertices;
  colors: TVertexRGBA;
  i: integer;
  mtlVertices: array of TVertVertex;
begin
  //de-allocate textures https://stackoverflow.com/questions/39158302/how-to-deallocate-a-mtlbuffer-and-mtltexture
  indexBuffer := nil;
  vertexBuffer := nil;
  LoadMesh(Filename, faces, verts, vNorm, colors, fMeshColor, isSwapYZ);
  numIdx := length(faces) * 3;//each face is a triangle with 3 vertices
  indexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@faces[0], sizeof(uint32) * numIdx, MTLResourceStorageModeShared);
  setlength(mtlVertices, length(verts));
  for i := 0 to (length(verts)-1) do
      mtlVertices[i] := VertVertex(verts[i].X, verts[i].Y, verts[i].Z, colors[i], vNorm[i]);
  vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@mtlVertices[0], sizeof(TVertVertex) * Length(mtlVertices), MTLResourceStorageModeShared);
  numVert := length(mtlVertices);
end; //OpenMesh()

procedure TGPUMesh.Paint();
var
  colorAttachment: MTLRenderPassColorAttachmentDescriptor;
  vertUniforms: TVertUniforms;
  ssaofragUniforms: TSSAOFragUniforms;
  projectionMatrix, modelMatrix: TMat4;
  w,h: integer;
  whratio, scale: single;
begin
  if meshShader = nil then
     Prepare();
  if vertexBuffer = nil then
     OpenMesh(fMeshName, false);
  w := trunc(mtlControl.renderView.drawableSize.width);
  h := trunc(mtlControl.renderView.drawableSize.height);
  if (w = 0) or (h = 0) then exit; //avoid divide by zero
  scale := 0.6*fDistance;
  whratio := w/h;
  if fPerspective then
     projectionMatrix := TMat4.Perspective(fDistance/kMaxDistance * 120.0, whratio, 0.01, kMaxDistance)
  else if (whratio > 1) then //Wide window
     projectionMatrix := TMat4.Ortho(-scale * whratio, scale * whratio, -scale, scale, 0.01, kMaxDistance)//5.0)
  else
      projectionMatrix := TMat4.Ortho(-scale, scale, -scale/whratio, scale/whratio, 0.01, kMaxDistance);//, 5.0);
  modelMatrix := TMat4.Identity;
  scale := 1.0;
  modelMatrix *= TMat4.Scale(0.5/Scale, 0.5/Scale, 0.5/Scale);
  modelMatrix *= TMat4.Translate(0, 0, -Scale*2);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  vertUniforms.modelViewMatrix := modelMatrix;
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  vertUniforms.normalMatrix := modelMatrix.Inverse.Transpose;
  vertUniforms.lightPos := fLightPos;
  {$IFDEF SSAO}
  if (w <> screenWidth) or (h <> screenHeight) then begin
     //outputColorTexture := MTLNewTexture(w, h, MTLTextureType2D, MTLPixelFormatBGRA8Unorm, MTLTextureUsageShaderRead or MTLTextureUsageShaderWrite);
     //outputDepthTexture := MTLNewTexture(w, h, MTLTextureType2D, MTLPixelFormatR32Float, MTLTextureUsageShaderRead or MTLTextureUsageShaderWrite);
     outputColorTexture := MTLNewTexture(w, h, MTLTextureType2D, MTLPixelFormatBGRA8Unorm, MTLTextureUsageShaderRead or MTLTextureUsageRenderTarget);
     outputDepthTexture := MTLNewTexture(w, h, MTLTextureType2D, MTLPixelFormatR32Float, MTLTextureUsageShaderRead or MTLTextureUsageRenderTarget);

     //outputColorTexture := MTLNewTexture(trunc(view.drawableSize.width), trunc(view.drawableSize.height), MTLTextureType2D, options.pipelineDescriptor.colorAttachmentAtIndex(1).pixelFormat, MTLTextureUsageShaderRead or MTLTextureUsageRenderTarget);
     //outputDepthTexture := MTLNewTexture(trunc(view.drawableSize.width), trunc(view.drawableSize.height), MTLTextureType2D, options.pipelineDescriptor.colorAttachmentAtIndex(2).pixelFormat, MTLTextureUsageShaderRead or MTLTextureUsageRenderTarget);

     screenWidth := w;
     screenHeight := h;
     meshRenderPassDescriptor := nil
  end;
  if meshRenderPassDescriptor = nil then begin
     meshRenderPassDescriptor := MTLRenderPassDescriptor.alloc.init;
     meshRenderPassDescriptor.depthAttachment.setTexture(mtlControl.renderView.depthStencilTexture);
     colorAttachment := meshRenderPassDescriptor.colorAttachmentAtIndex(1);
     colorAttachment.setTexture(outputColorTexture);
     colorAttachment.setClearColor(mtlControl.renderView.clearColor);
     colorAttachment.setLoadAction(MTLLoadActionClear);
     colorAttachment.setStoreAction(MTLStoreActionStore);
     colorAttachment := meshRenderPassDescriptor.colorAttachmentAtIndex(2);
     colorAttachment.setTexture(outputDepthTexture);
     colorAttachment.setClearColor(mtlControl.renderView.clearColor);
     colorAttachment.setLoadAction(MTLLoadActionClear);
     colorAttachment.setStoreAction(MTLStoreActionStore);
  end;
    MTLBeginCommand;
       MTLBeginEncoding(meshShader, meshRenderPassDescriptor);
         MTLSetCullMode(MTLCullModeNone);
         MTLSetVertexBuffer(vertexBuffer, 0, 0);
         MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
         MTLDrawIndexed (MTLPrimitiveTypeTriangle, numIdx, MTLIndexTypeUInt32, indexBuffer, 0);
       MTLEndEncoding;
       MTLBeginEncoding(ssaoShader); //composite g-buffer to screen: ambient occlusion
         ssaofragUniforms.fracAO := ffracAO;
         ssaofragUniforms.aoRadius := max( w, h)/(96*fDistance) ;
         MTLSetFragmentBytes(@ssaofragUniforms, sizeof(ssaofragUniforms), 0);
         MTLSetVertexBytes(@screenVerts, sizeof(screenVerts), 0);
         MTLSetFragmentTexture(outputColorTexture, 0);
         MTLSetFragmentTexture(outputDepthTexture, 1);
         MTLDraw(MTLPrimitiveTypeTriangleStrip, 0, 4);
       MTLEndEncoding;
     MTLEndCommand;
    {$ELSE}
  MTLBeginFrame(meshShader);
    MTLSetCullMode(MTLCullModeNone);
    MTLSetVertexBuffer(vertexBuffer, 0, 0);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
    MTLDrawIndexed (MTLPrimitiveTypeTriangle, numIdx, MTLIndexTypeUInt32, indexBuffer, 0);
  MTLEndFrame;
    {$ENDIF}
end;

end.

