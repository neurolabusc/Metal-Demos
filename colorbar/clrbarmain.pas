unit clrbarmain;

{$mode objfpc}{$H+}
{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
  {$DEFINE METALAPI}
  {$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, SimdUtils,
  Controls, Graphics, Dialogs, ExtCtrls, Menus;
type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ClrbarOffMenu: TMenuItem;
    TranslucentWhiteMenu: TMenuItem;
    BlackMenu: TMenuItem;
    TranslucentBlackMenu: TMenuItem;
    WhiteMenu: TMenuItem;
    procedure ClrMenu(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ViewGPU1Prepare(Sender: TObject);
    procedure ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewGPU1Paint(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

{$IFDEF METALAPI}
uses Metal, MetalPipeline, MetalControl, mtlclrbar;
{$ELSE}
uses glcorearb, OpenGLContext, gl_core_utils, glclrbar ;
{$ENDIF}
var
  gClrbar: TGPUClrbar;
  {$IFDEF METALAPI}
  ViewGPU1: TMetalControl;
  {$ELSE}
  ViewGPU1: TOpenGLControl;
  {$ENDIF}

procedure TForm1.ViewGPU1Prepare(Sender: TObject);
var
   i: integer;
   LUT: TLUT;
begin
  //set up color bar
  gClrbar:= TGPUClrbar.Create(ViewGPU1);
  gClrbar.isTopOrRight:=true;
  for i := 0 to 255 do begin
      LUT[i].R := i;
      LUT[i].G := 0;
      LUT[i].B := 0;
  end;
  gClrbar.SetLUT(1, LUT, -1,1);
  for i := 0 to 255 do begin
      LUT[i].R := 0;
      LUT[i].G := i;
      LUT[i].B := 0;
  end;
  gClrbar.SetLUT(2, LUT, 1,7);
  {$IFNDEF METALAPI}
  if GLErrorStr <> '' then begin
     showmessage(GLErrorStr);
     GLErrorStr := '';
  end;
  {$ELSE}
  ViewGPU1.SetPreferredFrameRate(0);
  ViewGPU1.InvalidateOnResize := true;
  //ViewGPU1.OnResize := @FormResize;
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
  ViewGPU1.OpenGLMajorVersion:= 3;
  ViewGPU1.OpenGLMinorVersion:= 3;
  {$ENDIF}
  ViewGPU1.Parent := Form1;
  ViewGPU1.Align:= alClient;
  ViewGPU1.OnMouseDown := @ViewGPU1MouseDown;
  ViewGPU1.OnPaint := @ViewGPU1Paint;
  {$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
  {$IFNDEF METALAPI}
  ViewGPU1.MakeCurrent(false);
  if (not  Load_GL_version_3_3_CORE) then begin
     showmessage('Unable to load OpenGL 3.3 Core');
     halt;
  end;
  Form1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  ViewGPU1.ReleaseContext;
  ViewGPU1Prepare(Sender);
  {$ENDIF}
end;

procedure TForm1.ClrMenu(Sender: TObject);
begin
  Case (sender as TMenuItem).Tag of
       1: begin
         gClrbar.BackColor := (setRGBA(255,255,255,255));
         gClrbar.FontColor := (setRGBA(0,0,0,255));
       end;
       2: begin
         gClrbar.BackColor := (setRGBA(255,255,255,128));
         gClrbar.FontColor := (setRGBA(0,0,0,255));
       end;
       3: begin
         gClrbar.BackColor := (setRGBA(0,0,0,255));
         gClrbar.FontColor := (setRGBA(255,255,255,255));
       end;
       4: begin
         gClrbar.BackColor := (setRGBA(0,0,0,128));
         gClrbar.FontColor := (setRGBA(255,255,255,255));
       end;
  end;
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF METALAPI} caption := 'multisample = '+inttostr(ViewGPU1.renderView.sampleCount);{$ENDIF}
   if (gClrbar.isVertical) then
     gClrbar.isTopOrRight := not gClrbar.isTopOrRight;
  gClrbar.isVertical := not gClrbar.isVertical;
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPU1Paint(Sender: TObject);
begin
  {$IFDEF METALAPI}
  MTLSetClearColor(MTLClearColorMake(0.3, 0.5, 0.8, 1));
  MTLBeginFrame();
    gClrbar.Draw(2);
  MTLEndFrame;
  {$ELSE}
  glClearColor(0.3, 0.5, 0.8, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  //draw colorbar
  if not ClrbarOffMenu.checked then
     gClrbar.Draw(2);
  //show result
  ViewGPU1.SwapBuffers;
  {$ENDIF}
end;

end.

