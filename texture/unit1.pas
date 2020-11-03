unit Unit1;

{$mode objfpc}{$H+}
{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
 //{$DEFINE METALAPI} //set in ProjectOptions/CompilerOptions/CustomOptions
  {$modeswitch objectivec1}
{$ENDIF}
{$IFNDEF METALAPI}
 {$include ../common/glopts.inc}
{$ENDIF}
interface

uses
    Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls;
type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
{$IFDEF METALAPI}
uses Metal,MetalPipeline, MetalControl, mtltexture, VectorMath,SimdUtils;
{$ELSE}
uses {$IFDEF LCLCocoa}retinahelper,{$ENDIF} {$IFDEF COREGL}glcorearb,{$ELSE}gl, glext, {$ENDIF} OpenGLContext, gl_core_utils, VectorMath, gltexture, SimdUtils;
{$ENDIF}
var
  gMouseY : integer = -1;
  gMouseX : integer = -1;
  gPositionX: single = 0;
  gPositionY: single = 0;
  gZoom : single = 1;
  gClearColor: TVec4 =  (r: 0.4; g: 0.6;  b: 0.95; a: 1);
  {$IFDEF METALAPI}
  ViewGPU1: TMetalControl;
  {$ELSE}
  ViewGPU1: TOpenGLControl;
  {$ENDIF}
  gTexBG, gTex: TGPUTexture;

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
 ViewGPU1.Invalidate;
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
 ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
 gMouseY := -1; //released
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 {$IFDEF METALAPI}
 ViewGPU1 := TMetalControl.Create(Form1);
 ViewGPU1.OnPrepare := @ViewGPUPrepare;
 {$ELSE}
 ViewGPU1 := TOpenGLControl.Create(Form1);
 {$ENDIF}
 ViewGPU1.Parent := Form1;
 ViewGPU1.Align:= alClient;
 ViewGPU1.OnMouseDown := @ViewGPUMouseDown;
 ViewGPU1.OnMouseMove := @ViewGPUMouseMove;
 ViewGPU1.OnMouseUp := @ViewGPUMouseUp;
 ViewGPU1.OnMouseWheel := @ViewGPUMouseWheel;
 ViewGPU1.OnPaint := @ViewGPUPaint;
 {$IFNDEF METALAPI}
 {$IFDEF COREGL}
 ViewGPU1.OpenGLMajorVersion:= 3;
 ViewGPU1.OpenGLMinorVersion:= 3;
 {$ELSE}
 ViewGPU1.OpenGLMajorVersion:= 2;
 ViewGPU1.OpenGLMinorVersion:= 1;
 {$ENDIF}
 ViewGPU1.MultiSampling:=1;
 {$IFDEF LCLCocoa}ViewGPU1.setRetina(false);{$ENDIF}
 ViewGPU1.MakeCurrent(false);
 {$IFDEF COREGL}
 if (not  Load_GL_version_3_3_CORE) then begin
 {$ELSE}
 if (not  Load_GL_version_2_1) then begin
 {$ENDIF}
    showmessage('Unable to load OpenGL');
    halt;
 end;
 Form1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
 ViewGPU1.ReleaseContext;
 ViewGPUPrepare(Sender);
 {$ENDIF}
end;

procedure TForm1.FormResize(Sender: TObject);
begin
 {$IFDEF METALAPI}
    ViewGPU1.invalidate();
 {$ENDIF}
end;

procedure TForm1.ViewGPUPrepare(Sender: TObject);
// load any pipelines you need here using the procedural API or Objective Pascal
var
  fnm: string;
begin
 fnm := ResourceFile('texture', 'png');
 if not fileexists(fnm) then
   showmessage('Unable to find image "'+fnm+'"');
 gTex := TGPUTexture.Create(fnm, ViewGPU1);
 gTexBG := TGPUTexture.Create(ResourceFile('coral', 'png'), ViewGPU1);
 gTex.SetPosition(0,0,1);
 gTexBG.SetPosition(0,0,1);
 {$IFDEF METALAPI}
    ViewGPU1.setPreferredFrameRate(0);
    Form1.OnResize := @FormResize;
 {$ENDIF}
end;

procedure TForm1.ViewGPUPaint(Sender: TObject);
begin
   {$IFDEF METALAPI}
  MTLSetClearColor(MTLClearColorMake(gClearColor.r, gClearColor.g, gClearColor.b, 1));
  MTLBeginFrame();
    gTexBG.DrawTex();
    gTex.DrawTex();
  MTLEndFrame;
  {$ELSE}
  glViewPort(0,0,ViewGPU1.ClientWidth, ViewGPU1.ClientHeight);
  glClearColor(gClearColor.r, gClearColor.g, gClearColor.b, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  glDisable(GL_DEPTH_TEST);
  gTexBG.DrawTex();
  gTex.DrawTex();
  ViewGPU1.SwapBuffers;
  {$ENDIF}
end;

end.
