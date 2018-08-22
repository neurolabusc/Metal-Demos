unit Unit1;

{$mode objfpc}{$H+}
{$IFNDEF LCLCocoa}
  MetalAPI only supported on modern MacOS
{$ENDIF}

interface

uses
    Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls;
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
    procedure ViewGPUPrepare(Sender: TObject);
    procedure ViewGPUPaint(Sender: TObject);
  private
    //
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses Metal,MetalPipeline, MetalControl, mtlcompute, VectorMath, MetalUtils;
var
  gMouseY : integer = -1;
  gMouseX : integer = -1;
  gPositionX: single = 0;
  gPositionY: single = 0;
  gZoom : single = 1;
  gClearColor: TVec4 =  (r: 0.4; g: 0.6;  b: 0.95; a: 1);
  ViewGPU1: TMetalControl;
  gTex: TGPUTextureCompute;

procedure TForm1.ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 if WheelDelta = 0 then exit;
 if WheelDelta > 0 then
   gZoom := gZoom + 0.1
 else
    gZoom := gZoom - 0.1;
 if gZoom < 0.1 then gZoom := 0.1;
 if gZoom > 10 then gZoom := 10;
 gTex.SetPosition(gPositionX,gPositionY,gZoom);
end;

procedure TForm1.ViewGPUMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 gMouseY := Y;
 gMouseX := X;
end;

procedure TForm1.ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
 if gMouseY < 0 then exit; //mouse is not down
 if (X <> gMouseX) or (Y <> gMouseY) then begin
       gPositionX := gPositionX + (X - gMouseX);
       gPositionY := gPositionY + (gMouseY - Y);
       gTex.SetPosition(gPositionX,gPositionY,gZoom);
 end;
 gMouseY := Y;
 gMouseX := X;
end;

procedure TForm1.ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
 gMouseY := -1; //released
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 ViewGPU1 :=  TMetalControl.Create(Form1);
 ViewGPU1.OnPrepare := @ViewGPUPrepare;
 ViewGPU1.Parent := Form1;
 ViewGPU1.Align:= alClient;
 ViewGPU1.OnMouseDown := @ViewGPUMouseDown;
 ViewGPU1.OnMouseMove := @ViewGPUMouseMove;
 ViewGPU1.OnMouseUp := @ViewGPUMouseUp;
 ViewGPU1.OnMouseWheel := @ViewGPUMouseWheel;
 ViewGPU1.OnPaint := @ViewGPUPaint;
end;

procedure TForm1.ViewGPUPrepare(Sender: TObject);
begin
 gTex := TGPUTextureCompute.Create(ResourcePath('compute', 'png'), ViewGPU1);
 gTex.SetPosition(0,0,1);
end;

procedure TForm1.ViewGPUPaint(Sender: TObject);
begin
  MTLSetClearColor(MTLClearColorMake(gClearColor.r, gClearColor.g, gClearColor.b, 1));
  //MTLBeginCommand;
  MTLBeginFrame();
    gTex.DrawTex();
  MTLEndFrame;
  //MTLEndCommand;
  //ViewGPU1.renderView.currentDrawable;
end;

end.
