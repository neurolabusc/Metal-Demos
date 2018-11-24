unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
    Dialogs, ExtCtrls, MetalPipeline, MetalUtils, MetalControl, Metal, VectorMath;
type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure MetalControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MetalControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MetalControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MetalControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MetalControl1Prepare(Sender: TObject);
    procedure MetalControl1Paint(Sender: TObject);
  private
    pipeline: TMetalPipeline;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
var
  gMouseXY : TPoint =  (x:0; y:-1);
  gOffsetXY : TPoint =  (x:0; y:0);
  gSize : single = 100;
  gClearColor: TVec4 = (r: 0.7; g: 0.3; b: 0.9; a: 1.0);
  MetalControl1: TMetalControl;

procedure TForm1.MetalControl1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Wheeldelta < 0 then
     gSize := gSize - 2
  else
      gSize := gSize + 2;
  if gSize > 1024 then
     gSize := 1024;
  if gSize < 5 then
     gSize := 5;
end;

procedure TForm1.MetalControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 gMouseXY.x := X;
 gMouseXY.y := Y;
end;

procedure TForm1.MetalControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
begin
 if gMouseXY.y < 0 then exit; //mouse is not down
 gOffsetXY.x := gOffsetXY.x + (X - gMouseXY.x);
 gOffsetXY.y := gOffsetXY.y - (Y - gMouseXY.y);
 gMouseXY.x := X;
 gMouseXY.y := Y;
end;

procedure TForm1.MetalControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
  gMouseXY.y := -1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 MetalControl1 := TMetalControl.Create(Form1);
 MetalControl1.Parent := Form1;
 MetalControl1.Align:= alClient;
 MetalControl1.OnPrepare := @MetalControl1Prepare;
 MetalControl1.OnMouseDown := @MetalControl1MouseDown;
 MetalControl1.OnMouseMove := @MetalControl1MouseMove;
 MetalControl1.OnMouseUp := @MetalControl1MouseUp;
 MetalControl1.OnMouseWheel := @MetalControl1MouseWheel;
 MetalControl1.OnPaint := @MetalControl1Paint;
end;

procedure TForm1.MetalControl1Prepare(Sender: TObject);
// load any pipelines you need here using the procedural API or Objective Pascal
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
    // NOTE Metal requires vertices to be aligned, hence padding required
    padding: TVec2;
    color: TVec4;
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

procedure TForm1.MetalControl1Paint(Sender: TObject);
var
 verts: array[0..2] of TAAPLVertex;
 vertUniforms: TVertUniforms;
begin
  //describe position and color for each vertex of the triangle
  verts[0] := AAPLVertex(V2(gOffsetXY.x + gSize, gOffsetXY.y -gSize), V4(1, 0, 0, 1));
  verts[1] := AAPLVertex(V2(gOffsetXY.x - gSize, gOffsetXY.y -gSize), V4(0, 1, 0, 1));
  verts[2] := AAPLVertex(V2(gOffsetXY.x + 0,     gOffsetXY.y + gSize), V4(0, 0, 1, 1));
  //the vertex uniform transforms the vertices to screen space - it changes when the window size changes
  vertUniforms.viewportSize := V2(MetalControl1.Width,MetalControl1.Height);
  MTLSetClearColor(MTLClearColorMake(gClearColor.r, gClearColor.g, gClearColor.b, gClearColor.a));
  //gClearColor.r := gClearColor.r + 0.05; if (gClearColor.r > 1.0) then gClearColor.r := 0.0;
  MTLBeginFrame(pipeline);
    MTLSetVertexBytes(@verts, sizeof(verts), AAPLVertexVerticesIndex);
    MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), AAPLVertexUniformsIndex);
    MTLDraw(MTLPrimitiveTypeTriangle, 0, 3);
  MTLEndFrame;
end;

end.
