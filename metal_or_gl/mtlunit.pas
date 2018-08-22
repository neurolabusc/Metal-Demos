unit mtlunit;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}


interface

uses
  Dialogs, Classes, SysUtils, MetalPipeline, MetalUtils, MetalControl, Metal, VectorMath;

type
  TViewGPU = class(TMetalControl)
    procedure MyPrepare(Sender: TObject);
    procedure MyPaint(Sender: TObject);
  private
    pipeline: TMetalPipeline;

  end;

var
  gOffsetXY : TPoint =  (x:0; y:0);
  gSize : single = 100;


implementation

procedure TViewGPU.MyPrepare(Sender: TObject);
var
 options: TMetalPipelineOptions;
begin
 options := TMetalPipelineOptions.Default;
 options.libraryName := ResourcePath('basic', 'metal');
 if not fileexists(options.libraryName) then
    showmessage('Unable to find ' + options.libraryName);
 pipeline := MTLCreatePipeline(options);
end;

const
  AAPLVertexVerticesIndex     = 0;
  AAPLVertexUniformsIndex = 1;
type
  TAAPLVertex = record
    position: TVec2;
    // NOTE: why doesn't {$align 16} on
    // align each vertex attribute on 16 byte boundries
    //padding: array[0..1] of simd_float;
    padding: TVec2;
    color: TVec4;//vector_float4;
  end;

function AAPLVertex(constref position: TVec2; constref color: TVec4): TAAPLVertex;
begin
    result.position := position;
    result.color := color;
end;

Type
TVertUniforms = record //Uniforms for vertex shader
  viewportSize: TVec2;
end;

procedure TViewGPU.MyPaint(Sender: TObject);
var
 verts: array[0..2] of TAAPLVertex;
 vertUniforms: TVertUniforms;
begin
  verts[0] := AAPLVertex(V2(gOffsetXY.x + gSize, gOffsetXY.y -gSize), V4(1, 0, 0, 1));
  verts[1] := AAPLVertex(V2(gOffsetXY.x - gSize, gOffsetXY.y -gSize), V4(0, 1, 0, 1));
  verts[2] := AAPLVertex(V2(gOffsetXY.x + 0,     gOffsetXY.y + gSize), V4(0, 0, 1, 1));
  vertUniforms.viewportSize := V2(Self.Width, Self.Height);
  //MTLSetClearColor(pipeline, MTLClearColorMake(0.0, 0.0, 0.7, 1));
  MTLSetClearColor(MTLClearColorMake(0.0, 0.0, 0.7, 1));
  MTLBeginFrame(pipeline);
    MTLSetVertexBytes(@verts, sizeof(verts), AAPLVertexVerticesIndex);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), AAPLVertexUniformsIndex);
    MTLDraw(MTLPrimitiveTypeTriangle, 0, 3);
  MTLEndFrame;
end;

end.

