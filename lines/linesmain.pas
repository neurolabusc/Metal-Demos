unit linesmain;

{$mode objfpc}{$H+}
{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
 //{$DEFINE METALAPI} //set in ProjectOptions/CompilerOptions/CustomOptions
  {$modeswitch objectivec1}
{$ENDIF}
{$IFDEF LCLCarbon}
  MacOS must use Cocoa, regardless of whether OpenGL Core or Metal is used.
{$ENDIF}

interface

{$IFNDEF METALAPI}
 {$include ../common/glopts.inc}
{$ENDIF}


uses
  Classes, SysUtils, Forms,  SimdUtils,
  Controls, Dialogs, ExtCtrls, Menus;
type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    RandomMenu: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure ViewGPU1Prepare(Sender: TObject);
    procedure ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClrMenu(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ViewGPU1Paint(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;


implementation
{$R *.lfm}

{$IFDEF METALAPI}
uses Metal, MetalPipeline, MetalControl, mtllines, VectorMath;
{$ELSE}
uses {$IFDEF COREGL}{$IFDEF LCLCocoa}retinahelper,{$ENDIF} glcorearb,{$ELSE}gl, glext, {$ENDIF} OpenGLContext, gl_core_utils, VectorMath, gllines;
{$ENDIF}
var
  gLines: TGPULines;
  {$IFDEF METALAPI}
  ViewGPU1: TMetalControl;
  {$ELSE}
  ViewGPU1: TOpenGLControl;
  {$ENDIF}

procedure TForm1.ViewGPU1Prepare(Sender: TObject);
begin
{$IFDEF METALAPI}
    ViewGPU1.SetPreferredFrameRate(0);
    Form1.OnResize := @FormResize;
  {$ENDIF}
end;

procedure TForm1.ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
 ClrMenu(Sender);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  {$IFDEF METALAPI}
  ViewGPU1.Invalidate;
  {$ENDIF}
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  {$IFDEF METALAPI}
  ViewGPU1 :=  TMetalControl.Create(Form1);
  ViewGPU1.OnPrepare := @ViewGPU1Prepare;
  {$ELSE}
  ViewGPU1 :=  TOpenGLControl.Create(Form1);
  {$IFDEF COREGL}
  ViewGPU1.OpenGLMajorVersion:= 3;
  ViewGPU1.OpenGLMinorVersion:= 3;
  {$ELSE}
  ViewGPU1.OpenGLMajorVersion:= 2;
  ViewGPU1.OpenGLMinorVersion:= 1;
  {$ENDIF}
  ViewGPU1.MultiSampling:=4;
  {$ENDIF}
  ViewGPU1.Parent := Form1;
  ViewGPU1.Align:= alClient;
  ViewGPU1.OnPaint := @ViewGPU1Paint;
  ViewGPU1.OnMouseDown := @ViewGPU1MouseDown;
  {$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
  {$IFNDEF METALAPI}
  ViewGPU1.MakeCurrent(false);
  {$IFDEF LCLCocoa} {$IFDEF COREGL}
  ViewGPU1.setRetina(true);
  //LSetWantsBestResolutionOpenGLSurface(true, ViewGPU1.Handle);
  {$ENDIF} {$ENDIF}
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
  //ViewGPU1Prepare(Sender);
  if GLErrorStr <> '' then begin
     showmessage(GLErrorStr);
     GLErrorStr := '';
  end;
  {$ENDIF}
  gLines := TGPULines.Create(ViewGPU1);
  ClrMenu(Sender);
  ViewGPU1.Invalidate;
end;


function RandClr(): TRGBA;
begin
  result.R := random(255);
  result.G := random(255);
  result.B := random(255);
  result.A := 128+random(127);
end;

function RandVec(w,h: integer): TVec2;
begin
     result.x := random(w);
     result.y := random(h);
end;

procedure TForm1.ClrMenu(Sender: TObject);
const
  kMinLines = 512;
  kRanLines = 512;
var
   w, h, i: integer;
begin
  {$IFDEF METALAPI} caption := 'multisample = '+inttostr(ViewGPU1.renderView.sampleCount);{$ENDIF}
  w := ViewGPU1.ClientWidth;
  h := ViewGPU1.ClientHeight;
  gLines.ClearLines();
  for i := 1 to kMinLines+random(kRanLines) do begin
      gLines.LineWidth:= 1 + random(6);
      gLines.LineColor := RandClr();
      gLines.AddLine(RandVec(w,h),RandVec(w,h));
  end;
  //draw fine lines to show pixel position: e.g. 1 pixel line at position 2 should span 1..2 not darken two pixels (1.5..2.5)
  gLines.LineWidth:= 1;
  gLines.LineColor := setRGBA(0,0,0,255);
  gLines.AddLine(0,2, 100, 2);
  gLines.AddLine(2,0, 2, 100);
  gLines.AddLine(0,20, 100, 20);
  gLines.AddLine(20,0, 20, 100);
  gLines.AddLine(0,22, 100, 22);
  gLines.AddLine(22,0, 22, 100);
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPU1Paint(Sender: TObject);
begin
  {$IFDEF METALAPI}
  MTLSetClearColor(MTLClearColorMake(0.3, 0.5, 0.8, 1));
  MTLBeginFrame();
    gLines.Draw();
  MTLEndFrame;
  {$ELSE}
  glDisable(GL_DEPTH_TEST);
  glViewPort(0,0,ViewGPU1.ClientWidth, ViewGPU1.ClientHeight);
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  gLines.Draw();
  ViewGPU1.SwapBuffers;
  {$ENDIF}
end;

end.

