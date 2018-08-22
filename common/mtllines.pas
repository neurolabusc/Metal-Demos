unit mtllines;
//openGL lines

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
interface


uses
  MetalPipeline, MetalUtils, MetalControl, Metal,
  VectorMath,  SimdUtils,
  Classes, SysUtils, Graphics,  dialogs;


type
  TGPULines = class
  private
         LineWid: single;
         LineClr: TRGBA;
         vertexBuffer: MTLBufferProtocol;
         numVertices: integer;
         isRedraw: boolean;
    	 shaderPipeline: TMetalPipeline;
         mtlControl: TMetalControl;
         procedure CreateStrips;
         procedure SetPipeline;
  public
    property NumberOfVertices : integer read numVertices;
    property LineWidth : single read LineWid write LineWid;
    property LineColor : TRGBA read LineClr write LineClr;
    procedure AddLine(startX,startY,endX,endY: single); overload;
    procedure AddLine(startXY, endXY: TVec2); overload;
    procedure ClearLines();
    procedure Draw(); //must be called while TMetalControl is current context
    constructor Create(fromView: TMetalControl);
  end;

implementation

type
  TPoint3f = Packed Record
    x,y,z: single;
  end;

type
  TVertUniforms = record //Uniforms for vertex shader
    viewportSize, viewportSizeDivTwo, TwoDivViewportSize: TVec2;
  end;
  TVtxClr = record
    position: TVec2;
    padding: TVec2; // align each vertex attribute on 16 byte boundries
    color: TVec4;
    //colorU4,pad1,pad2,pad3: TRGBA;
  end;

const
  kBlockSz = 2048;
var
    g2Dvnc: array of TVtxClr;

procedure TGPULines.CreateStrips;
begin
  setlength(g2Dvnc,0);
  isRedraw := false;
end;

procedure TGPULines.SetPipeline();
var
 options: TMetalPipelineOptions;
 shaderName: string;
begin
     if (shaderPipeline <> nil) then exit; //already set
     options := TMetalPipelineOptions.Default;
     shaderName := ResourceFolderPath + pathdelim + 'lines.metal';
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

constructor TGPULines.Create(fromView: TMetalControl);
begin
     mtlControl := fromView;
     LineClr := setRGBA(255, 255, 255, 255);
     LineWid := 10;
     isRedraw := true;
     numVertices := 0;
     vertexBuffer := nil;
     shaderPipeline := nil;
end;

procedure TGPULines.ClearLines();
begin
     numVertices := 0;
end;

procedure TGPULines.AddLine(startX,startY,endX,endY: single); overload;
var
  i: integer;
  nx,ny, len: single;
  clr: TVec4;
begin
  ny :=  (startX-endX);
  nx :=  (startY-endY);
  len := sqrt(sqr(nx)+sqr(ny));
  if (len = 0) then exit;
  nx := 0.5*lineWid*nx/len;
  ny := 0.5*lineWid*ny/len;
  if (numVertices+6) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
  clr := Vec4(LineClr.R/255, LineClr.G/255, LineClr.B/255, LineClr.A/255);
  //for i := 0 to 5 do
  //    g2Dvnc[numVertices +i].colorU4 := LineClr;
  for i := 0 to 5 do
      g2Dvnc[numVertices +i].color := clr;
  g2Dvnc[numVertices+0].position := Vec2(startX+nx,startY-ny);
  g2Dvnc[numVertices+1].position := Vec2(startX-nx,startY+ny);
  g2Dvnc[numVertices+2].position := Vec2(endX+nx,endY-ny);
  g2Dvnc[numVertices+3].position := g2Dvnc[numVertices+1].position;
  g2Dvnc[numVertices+4].position := g2Dvnc[numVertices+2].position;
  g2Dvnc[numVertices+5].position := Vec2(endX-nx,endY+ny);
  numVertices := numVertices + 6;
  isRedraw := true;
end;

procedure TGPULines.AddLine(startXY, endXY: TVec2); overload;
begin
     AddLine(startXY.X, startXY.Y, endXY.X, endXY.Y);
end;

procedure TGPULines.Draw();
var
vertUniforms: TVertUniforms;
begin
  if numVertices < 1 then exit;
  setPipeline;
  MTLSetShader(shaderPipeline);
  if isRedraw then begin //only update buffer if something has changed
     vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@g2Dvnc[0], numVertices*sizeof(TVtxClr), MTLResourceStorageModeShared);
     isRedraw := false;
  end;
  vertUniforms.viewportSize := V2(mtlControl.Width, mtlControl.Height);
  vertUniforms.viewportSizeDivTwo := V2(mtlControl.Width/2.0, mtlControl.Height/2.0);
  vertUniforms.TwoDivViewportSize := V2(2.0/mtlControl.Width, 2.0/mtlControl.Height);
  MTLSetVertexBuffer(vertexBuffer, 0, 0);
  MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
  MTLDraw(MTLPrimitiveTypeTriangle, 0, numVertices);
end;

end.

