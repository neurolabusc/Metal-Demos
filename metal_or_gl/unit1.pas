unit Unit1;

{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
 {$DEFINE METALAPI}
{$ENDIF}
{$IFDEF LCLCarbon}
MacOS must use Cocoa, regardless of whether OpenGL Core or Metal is used.
{$ENDIF}
interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
    Dialogs, ExtCtrls, {$IFDEF METALAPI}mtlunit {$ELSE} glunit{$ENDIF};

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
var
  gMouseXY : TPoint =  (x:0; y:-1);
  ViewGPU1: TViewGPU;

procedure TForm1.ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState;
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
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPUMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 gMouseXY.x := X;
 gMouseXY.y := Y;
end;

procedure TForm1.ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
begin
 ViewGPU1.Invalidate;
 if gMouseXY.y < 0 then exit; //mouse is not down
 gOffsetXY.x := gOffsetXY.x + (X - gMouseXY.x);
 gOffsetXY.y := gOffsetXY.y - (Y - gMouseXY.y);
 gMouseXY.x := X;
 gMouseXY.y := Y;
 ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
  gMouseXY.y := -1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 ViewGPU1 :=  TViewGPU.Create(Form1);
 ViewGPU1.Parent := Form1;
 ViewGPU1.Align:= alClient;
 ViewGPU1.OnMouseDown := @ViewGPUMouseDown;
 ViewGPU1.OnMouseMove := @ViewGPUMouseMove;
 ViewGPU1.OnMouseUp := @ViewGPUMouseUp;
 ViewGPU1.OnMouseWheel := @ViewGPUMouseWheel;
 {$IFDEF METALAPI}
 ViewGPU1.OnPrepare := @ViewGPU1.MyPrepare;
 {$ELSE}
 ViewGPU1.OpenGLMajorVersion:= 3;
 ViewGPU1.OpenGLMinorVersion:= 3;
 ViewGPU1.InitGL(ViewGPU1);
 {$ENDIF}
 ViewGPU1.OnPaint := @ViewGPU1.MyPaint;
 ViewGPU1.Invalidate;
end;


end.
