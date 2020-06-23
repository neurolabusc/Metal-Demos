unit mtlvolume;
{$mode objfpc}
{$modeswitch objectivec1}
{$H+}
interface

{$DEFINE GPUGRADIENTS} //Computing volume gradients on the GPU is much faster than using the CPU
{$DEFINE MATCAP}
uses
    {$IFDEF MATCAP} intfgraphics, graphtype, Graphics,  {$ENDIF}
    VectorMath, MetalPipeline, MetalUtils, MetalControl, Metal,
    SysUtils, Math, loadNifti, SimdUtils;
const
 kDefaultDistance = 2.25;
 kMaxDistance = 40;
type
  TGPUVolume = class
      private
        RayCastQuality1to10, slices,fAzimuth,fElevation: integer;
        fDistance: single;
        fLightPos: TVec4;
        indexBuffer, vertexBuffer: MTLBufferProtocol;
        pipeline: TMetalPipeline;
        mtlControl: TMetalControl;
        volTex, gradTex: MTLTextureProtocol;
        {$IFDEF MATCAP} matCapTex: MTLTextureProtocol;{$ENDIF}
        procedure Prepare();
        procedure LoadCube(var fromView: TMetalControl);
        procedure LoadTexture(var vol: TNIfTI; fromView: TMetalControl);
      public
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        constructor Create(fromView: TMetalControl);
        procedure Paint(var vol: TNIfTI);
        procedure SetShader(shaderName: string);
        procedure SaveBmp(filename: string);
        {$IFDEF MATCAP} function SetMatCap(fnm: string): boolean; {$ENDIF}
  end;

implementation

type
  TVertVertex = record //Each vertex defines a location and color
    position: TVec3;
    padding: array[0..0] of TScalar;
    color: TVec4;
  end;
  TVertVertexArray =  array of TVertVertex;
  TVertUniforms = record //Uniforms for vertex shader
    modelViewProjectionMatrix: TMat4;
  end;
  TFragUniforms = record //Uniforms for fragment shader
    stepSize, sliceSize, x0, x1: TScalar;
    rayDir: TVec4;
    lightPos: TVec4;
    normalMatrix: TMat4;
  end;

{$IFDEF MATCAP}

procedure FlipVertical (var px: TPicture);
var
  p: array of byte;
  i, half, b: integer;
  LoPtr, HiPtr: PInteger;
begin
    if px.Height < 3 then exit;
    half := (px.Height div 2);
    b := px.Bitmap.RawImage.Description.BytesPerLine;
    LoPtr := PInteger(px.Bitmap.RawImage.Data);
    HiPtr := PInteger(px.Bitmap.RawImage.Data+ ((px.Height -1) * b));
    setlength(p, b);
    for i := 1 to half do begin
          System.Move(LoPtr^,p[0],b); //(src, dst,sz)
          System.Move(HiPtr^,LoPtr^,b); //(src, dst,sz)
          System.Move(p[0],HiPtr^,b); //(src, dst,sz)
          Inc(PByte(LoPtr), b );
          Dec(PByte(HiPtr), b);
    end;
end; //FlipVertical()

procedure CreateBmp(var px: TPicture);
Type
  TRGBquad = PACKED RECORD
     rgbBlue,rgbGreen,rgbRed,rgbAlpha: byte;
    end;
  TQuadRA = array [1..1] of TRGBQuad;
  RGBQuadp = ^TQuadRA;
function rgb2quad(R,G,B: Byte): TRGBquad;
begin
  result.rgbRed:= (R);
  result.rgbGreen:= (G);
  result.rgbBlue:= (B);
  result.rgbAlpha:= 0;
end;
const
  x = 256;
  y = 256;
var
  i, j, k: integer;
  lBuff: RGBQuadp;
  Ptr: pointer;
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
begin
  GetMem(lBuff, x*y* sizeof(TRGBQuad));
  k := 1;
  for j := 1 to y do
      for i :=  1 to x do begin
          //lBuff^[k] := rgb2quad(256-i,i-1,256-j);
          lBuff^[k] := rgb2quad(256-j,256-j,256-j);
          k := k + 1;
      end;
  lRawImage.Init;
  //lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(0,0);
  lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(x,y);
  lRawImage.CreateData(true);
  AImage := TLazIntfImage.Create(x,y);
  AImage.SetRawImage(lRawImage);
  AImage.BeginUpdate;
  i := 1;
  for j := 0 to (y-1) do begin
    ptr := AImage.GetDataLineStart(j);
    Move(lBuff^[i], Ptr^, x * sizeof(TRGBQuad));
    inc(i, x);
  end;
  AImage.EndUpdate;
  px.Bitmap.LoadFromIntfImage(AImage);
  FreeMem(lBuff);
end;


function TGPUVolume.SetMatCap(fnm: string): boolean;
var
  px: TPicture;
  bmpHt, bmpWid: integer;
  isPng : boolean;
  pngTexDesc: MTLTextureDescriptor;
  pngRegion: MTLRegion;
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
  //ifnm, MatCapDir: string;
begin
  result := false;
  if (not fileexists(fnm)) and (fnm <> '') then begin
       //MatCapDir := ExtractFilePath(ShaderDir)+ 'matcap';
       fnm := ExtractFilePath(ShaderDir)+ 'matcap'+pathdelim+fnm+'.jpg';
  end;
  //if (fnm <> '') and (not fileexists(fnm)) then begin
  px := TPicture.Create;
  if not fileexists(fnm) then begin
     if fnm <> '' then
        writeln('Unable to find MatCap "'+fnm+'"');
     CreateBmp(px)
  end else begin
    isPng := upcase(ExtractFileExt(fnm)) = '.PNG';
    try
      if isPng then
          px.LoadFromFile(fnm)
      else begin
        lRawImage.Init;
        //lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(0,0);
        lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(0,0);
        lRawImage.CreateData(false);
        AImage := TLazIntfImage.Create(0,0);
        try
          AImage.SetRawImage(lRawImage);
          AImage.LoadFromFile(fnm);
          px.Bitmap.LoadFromIntfImage(AImage);
        finally
          AImage.Free;
        end;
      end;
    except
      px.Bitmap.Width:=0;
    end;
  end;
  if (px.Bitmap.PixelFormat <> pf32bit ) or (px.Bitmap.Width < 1) or (px.Bitmap.Height < 1) then begin
     writeln('Error loading 32-bit MatCap '+fnm);
     exit;
  end;
  FlipVertical(px);
  bmpHt := px.Bitmap.Height;
  bmpWid := px.Bitmap.Width;
  if px.Bitmap.PixelFormat <> pf32bit then
     exit; //distance stored in ALPHA field
  pngTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
  pngTexDesc.setTextureType(MTLTextureType2D);
  if isPng then
     pngTexDesc.setPixelFormat(MTLPixelFormatBGRA8Unorm)
  else
      pngTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
  //pngTexDesc.setPixelFormat(MTLPixelFormatBGRA8Unorm);
  pngTexDesc.setWidth(bmpWid);
  pngTexDesc.setHeight(bmpHt);
  pngTexDesc.setDepth(1);
  if (matCapTex <> nil) then matCapTex.release;
  matCapTex := mtlControl.renderView.device.newTextureWithDescriptor(pngTexDesc);
  Fatal(matCapTex = nil, format('mtlfont: newTextureWithDescriptor failed %dx%d', [bmpHt, bmpWid]));
  pngRegion := MTLRegionMake3D(0, 0, 0, bmpWid, bmpHt, 1);
  matCapTex.replaceRegion_mipmapLevel_withBytes_bytesPerRow(pngRegion, 0, PInteger(px.Bitmap.RawImage.Data), bmpWid*4);
  px.Free;
  result := true;
end;
{$ENDIF}

procedure TGPUVolume.SaveBmp(filename: string);
begin
     MTLWriteTextureToFile(pChar(filename));
end;

procedure TGPUVolume.SetShader(shaderName: string);
var
 options: TMetalPipelineOptions;
begin
 options := TMetalPipelineOptions.Default;
 options.libraryName := shaderName;
 if not fileexists(shaderName) then
    writeln('Unable to find shader ' + shaderName)
 else
     writeln('Using shader ' + shaderName);

 options.pipelineDescriptor := MTLCreatePipelineDescriptor;
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
 options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
 pipeline := MTLCreatePipeline(options);
 MTLSetDepthStencil(pipeline, MTLCompareFunctionLess, true);
end;

procedure TGPUVolume.Prepare();
var
 shaderName: string;
begin
 shaderName := ResourceFolderPath+pathdelim+'Render.metal';
 SetShader(shaderName);
end;

constructor TGPUVolume.Create(fromView: TMetalControl);
{$IFDEF MATCAP}
var
   fnm: string;
{$ENDIF}
begin
  mtlControl := fromView;
  fDistance := kDefaultDistance;
  fAzimuth := 70;
  fElevation := 30;
  RaycastQuality1to10 := 6;
  fLightPos := Vec4(0,0.707,0.707, 0.0);
  //fClearColor.r := 200;
  //fClearColor.g := 200;
  //fClearColor.b := 255;
  vertexBuffer := nil;
  pipeline := nil;
  {$IFDEF MATCAP}
  matCapTex  := nil;
  fnm := ResourceFolderPath+pathdelim+'matcap'+pathdelim+'RedPlastic.jpg'; //.png or .jpg
  if not SetMatCap(fnm) then
     writeln('Unable to load MatCap '+fnm);
  {$ENDIF}
end;

function VertVertex(x, y, z: TScalar): TVertVertex;
begin
     result.position := V3(x,y,z);
     result.color := V4(x,y,z, 1);
end;

procedure TGPUVolume.LoadCube(var fromView: TMetalControl);
const
 mtlFaces: array[0..13] of uint16 = (1,0,4,3,7,6,4,5,1,6,2,3,1,0);
 //mtlFaces: array[0..13] of uint16 = (0,1,3,2,6,1,5,4, 6,7,3, 4, 0, 1);
var
  v0,v1,v2, v3,v4,v5, v6, v7:TVertVertex;
  vertices: array of TVertVertex;
begin
 v0 := VertVertex(0,0,0);
 v1 := VertVertex(0,1,0);
 v2 := VertVertex(1,1,0);
 v3 := VertVertex(1,0,0);
 v4 := VertVertex(0,0,1);
 v5 := VertVertex(0,1,1);
 v6 := VertVertex(1,1,1);
 v7 := VertVertex(1,0,1);
 vertices := TVertVertexArray.Create(v0,v1,v2,v3,v4,v5,v6,v7);
 indexBuffer := fromView.renderView.device.newBufferWithBytes_length_options(@mtlFaces[0], sizeof(uint16) * Length(mtlFaces), MTLResourceStorageModeShared);
 vertexBuffer := fromView.renderView.device.newBufferWithBytes_length_options(@vertices[0], sizeof(TVertVertex) * Length(vertices), MTLResourceStorageModeShared);
end;

procedure TGPUVolume.LoadTexture(var vol: TNIfTI; fromView: TMetalControl);
var
 volTexDesc, gradTexDesc: MTLTextureDescriptor;
 volRegion: MTLRegion;
 {$IFDEF GPUGRADIENTS}
 options: TMetalPipelineOptions;
 blurShader, sobelShader : TMetalPipeline;
 threadgroupSize: MTLSize;
 threadgroupCount: MTLSize;
 tempTex: MTLTextureProtocol;
 {$ELSE}
 gradData: TRGBAs;
 gradRegion: MTLRegion;
 {$ENDIF}
begin
 if (Vol.VolRGBA = nil) then exit;
 slices := max(Vol.Dim.X,max(Vol.Dim.Y,Vol.Dim.Z));
 volTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
 volTexDesc.setTextureType(MTLTextureType3D);
 volTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
 volTexDesc.setWidth(Vol.Dim.X);
 volTexDesc.setHeight(Vol.Dim.Y);
 volTexDesc.setDepth(Vol.Dim.Z);
 if (volTex <> nil) then volTex.release;
 volTex := fromView.renderView.device.newTextureWithDescriptor(volTexDesc);
 Fatal(volTex = nil, 'newTextureWithDescriptor failed');
 volRegion := MTLRegionMake3D(0, 0, 0, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z);
 volTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(volRegion, 0,0, @Vol.VolRGBA[0], Vol.Dim.X*4, Vol.Dim.X*Vol.Dim.Y*4);
 //compute and load gradients
 //startTime := Now;
 //Caption := format('Gradient Computation Required: %d Stretch %g %g %g', [MilliSecondsBetween(Now,startTime), gPrefs.ScaleDim[1],gPrefs.ScaleDim[2],gPrefs.ScaleDim[3]]);
 gradTexDesc := MTLTextureDescriptor.alloc.init.autorelease;
 gradTexDesc.setTextureType(MTLTextureType3D);
 gradTexDesc.setUsage(MTLTextureUsageShaderWrite or MTLTextureUsageShaderRead);
 gradTexDesc.setPixelFormat(MTLPixelFormatRGBA8Unorm);
 gradTexDesc.setWidth(Vol.Dim.X);
 gradTexDesc.setHeight(Vol.Dim.Y);
 gradTexDesc.setDepth(Vol.Dim.Z);
 if (gradTex <> nil) then gradTex.release;
 gradTex := fromView.renderView.device.newTextureWithDescriptor(gradTexDesc);
 Fatal(gradTex = nil, 'newTextureWithDescriptor failed');
 {$IFDEF GPUGRADIENTS}
  tempTex := fromView.renderView.device.newTextureWithDescriptor(gradTexDesc);
  threadgroupSize := MTLSizeMake(8, 8, 8);
  threadgroupCount.width  := (Vol.Dim.X  + threadgroupSize.width -  1) div threadgroupSize.width;
  threadgroupCount.height := (Vol.Dim.Y + threadgroupSize.height - 1) div threadgroupSize.height;
  threadgroupCount.depth := (Vol.Dim.Z + threadgroupSize.depth - 1) div threadgroupSize.depth;
  options := TMetalPipelineOptions.Default;
  options.libraryName := ResourcePath('_Blur3d', 'metal');
  options.kernelFunction := 'sobelKernel';  //blur kernel
  sobelShader := MTLCreatePipeline(options);
  options.kernelFunction := 'blurKernel';  //blur kernel
  blurShader := MTLCreatePipeline(options);
  MTLBeginCommand;
   MTLBeginEncoding(blurShader);
      MTLSetTexture(volTex, 0); //in
      MTLSetTexture(tempTex, 1);//out
      MTLSetDispatchThreadgroups(threadgroupCount, threadgroupSize);
   MTLEndEncoding;
   MTLBeginEncoding(sobelShader);
        MTLSetTexture(tempTex, 0); //in
        MTLSetTexture(gradTex, 1);//out
        MTLSetDispatchThreadgroups(threadgroupCount, threadgroupSize);
    MTLEndEncoding;
  //MTLEndCommand; //does not wait for completion
  MTLEndCommand(true); //<- syncrhonous: waitUntilCompleted, reduce flicker
  sobelShader.Free;
  blurShader.Free;
  tempTex.release;
  tempTex := nil;
 {$ELSE}
 gradRegion := MTLRegionMake3D(0, 0, 0, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z);
 gradData := Vol.GenerateGradientVolume;
 //CreateGradientVolume (Vol.VolRGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, gradData);
 gradTex.replaceRegion_mipmapLevel_slice_withBytes_bytesPerRow_bytesPerImage(gradRegion, 0,0, @gradData[0], Vol.Dim.X*4, Vol.Dim.X*Vol.Dim.Y*4);
 gradData := nil;
 {$ENDIF}
 Vol.GPULoadDone;
end;

procedure addFuzz (var v: TVec4); //avoid shader compile by zero error
const
     kEPS = 0.0001;
begin
   if (abs(v.x) < kEPS) then v.x := kEPS;
   if (abs(v.y) < kEPS) then v.y := kEPS;
   if (abs(v.z) < kEPS) then v.z := kEPS;
   if (abs(v.w) < kEPS) then v.w := kEPS;
end;

procedure TGPUVolume.Paint(var vol: TNIfTI);
var
  vertUniforms: TVertUniforms;
  fragUniforms: TFragUniforms;
  projectionMatrix, modelMatrix: TMat4;
  modelLightPos, v, rayDir: TVec4;
  whratio, scale: single;
begin
 //if MetalControl1.Tag = 0 then exit;
 //MetalControl1.Tag := 0;
 if pipeline = nil then
    Prepare;
  if vertexBuffer = nil then // only once
    LoadCube(mtlControl);
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol, mtlControl);
  if (volTex = nil) then
    exit;
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(0, 0, -fDistance);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  modelMatrix *= TMat4.Translate(-vol.Scale.X/2, -vol.Scale.Y/2, -vol.Scale.Z/2);
  modelLightPos := (modelMatrix.Transpose * fLightPos);
  modelMatrix *= TMat4.Scale(vol.Scale.X, vol.Scale.Y, vol.Scale.Z); //for volumes that are rectangular not square
  fragUniforms.normalMatrix := modelMatrix.Inverse.Transpose;
  if fDistance = 0 then
          scale := 1
  else
      scale := 0.5 * 1/abs(kDefaultDistance/(fDistance+1.0));
  whratio := mtlControl.clientwidth/mtlControl.clientheight;
  if (whratio > 1) or (whratio = 0) then //Wide window
     projectionMatrix := TMat4.Ortho(-scale * whratio, scale * whratio, -scale, scale, 0.01, 5.0)
  else
      projectionMatrix := TMat4.Ortho(-scale, scale, -scale/whratio, scale/whratio, 0.01, 5.0);
  vertUniforms.modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  rayDir.x := 0; RayDir.y := 0; rayDir.z := 1; RayDir.w := 0;
  v := rayDir;
  rayDir := (vertUniforms.modelViewProjectionMatrix.Inverse * v);
  rayDir.w := 0;
  rayDir := rayDir.Normalize;
  addFuzz(rayDir);
  fragUniforms.rayDir.X := rayDir.x; fragUniforms.rayDir.Y := rayDir.y; fragUniforms.rayDir.Z := rayDir.z;
  fragUniforms.lightPos := modelLightPos;
  fragUniforms.sliceSize := 1/slices;
  fragUniforms.stepSize := 1.0/ ((slices*0.25)+ (slices*1.75)* (RayCastQuality1to10/10));
  MTLBeginFrame(pipeline);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
    MTLSetFragmentTexture(volTex, 0);
    MTLSetFragmentTexture(gradTex, 1);
    {$IFDEF MATCAP}
    MTLSetFragmentTexture(matcapTex, 2);
    {$ENDIF}
    MTLSetVertexBuffer(vertexBuffer, 0, 0);
    MTLSetFragmentBytes(@fragUniforms, sizeof(fragUniforms), 1);
    MTLSetCullMode(MTLCullModeFront);
    MTLDrawIndexed (MTLPrimitiveTypeTriangleStrip, 14, MTLIndexTypeUInt16, indexBuffer, 0);
  MTLEndFrame;
end;

end.

