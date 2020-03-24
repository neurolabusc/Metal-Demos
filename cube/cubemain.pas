unit cubemain;

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
    procedure ViewGPUMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);

  private

  public

  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

{$IFDEF METALAPI}
uses Metal, MetalPipeline, MetalControl, mtlcube, VectorMath;
{$ELSE}
uses {$IFDEF LCLCocoa}retinahelper,{$ENDIF} glcorearb, OpenGLContext, gl_core_utils, VectorMath,  glcube;
{$ENDIF}
var
  gCube : TGPUCube;
  gMouse : TPoint;
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

procedure TForm1.ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  gMouse.Y := Y;
  gMouse.X := X;
  {$IFDEF METALAPI} caption := 'multisample = '+inttostr(ViewGPU1.renderView.sampleCount);{$ENDIF}
end;

procedure TForm1.ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if gMouse.Y < 0 then exit; //mouse is not down
  gCube.Azimuth := gCube.Azimuth + (X - gMouse.X);
  gCube.Elevation := gCube.Elevation - (Y - gMouse.Y);
  while gCube.Azimuth > 360 do gCube.Azimuth := gCube.Azimuth - 360;
  while gCube.Azimuth < 0 do gCube.Azimuth := gCube.Azimuth + 360;
  if gCube.Elevation > 90 then gCube.Elevation := 90;
  if gCube.Azimuth < -90 then gCube.Elevation := -90;
  gMouse.X := X;
  gMouse.Y := Y;
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    gMouse.Y := -1; //released
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  {$IFDEF METALAPI}
  ViewGPU1.Invalidate;
  {$ENDIF}
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  gMouse.Y := -1;
  {$IFDEF METALAPI}
  ViewGPU1 :=  TMetalControl.Create(Form1);
  ViewGPU1.OnPrepare := @ViewGPU1Prepare;
  {$ELSE}
  ViewGPU1 :=  TOpenGLControl.Create(Form1);
  ViewGPU1.OpenGLMajorVersion:= 3;
  ViewGPU1.OpenGLMinorVersion:= 3;
  ViewGPU1.MultiSampling:=4;
  {$ENDIF}
  ViewGPU1.Parent := Form1;
  {$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
  ViewGPU1.Align:= alClient;
  ViewGPU1.OnPaint := @ViewGPU1Paint;
  ViewGPU1.OnMouseDown := @ViewGPU1MouseDown;
  ViewGPU1.OnMouseUp := @ViewGPUMouseUp;
  ViewGPU1.OnMouseMove := @ViewGPUMouseMove;
  {$IFNDEF METALAPI}
  ViewGPU1.MakeCurrent(false);
  {$IFDEF LCLCocoa}
  ViewGPU1.setRetina(true);
  //LSetWantsBestResolutionOpenGLSurface(true, ViewGPU1.Handle);
  {$ENDIF}

  if (not  Load_GL_version_3_3_CORE) then begin
     showmessage('Unable to load OpenGL 3.3 Core');
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
  gCube := TGPUCube.Create(ViewGPU1);
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
begin
  gCube.Size := 0.01 * (1+random(18));
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPU1Paint(Sender: TObject);
begin
  {$IFDEF METALAPI}
  MTLSetClearColor(MTLClearColorMake(0.3, 0.5, 0.8, 1));
  //ctx :=  ViewGPU1.renderView.;  //SharedContext  ViewGPU1
  //ViewGPU1.renderView.sampleCount;
  MTLBeginFrame();
    gCube.Draw(ViewGPU1.ClientWidth, ViewGPU1.ClientHeight);
  MTLEndFrame;
  {$ELSE}
  //glViewPort(0,0,ViewGPU1.ClientWidth, ViewGPU1.ClientHeight);
  glViewPort(0,0,ViewGPU1.ClientWidth, ViewGPU1.ClientHeight);
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  glDisable(GL_DEPTH_TEST);
  gCube.Draw(ViewGPU1.ClientWidth, ViewGPU1.ClientHeight);
  ViewGPU1.SwapBuffers;
  if GLErrorStr <> '' then begin
     caption := GLErrorStr;
     GLErrorStr := '';
  end;
  {$ENDIF}
end;

end.

