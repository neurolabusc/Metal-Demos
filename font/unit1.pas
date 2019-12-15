unit Unit1;

{$mode objfpc}{$H+}
{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
 {$DEFINE METALAPI}
{$modeswitch objectivec1}
{$ENDIF}
{$IFDEF LCLCarbon}
  MacOS must use Cocoa, regardless of whether you use OpenGL Core or Metal
{$ENDIF}
{$DEFINE myTextures} // <- show background texture
interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
    Dialogs, ExtCtrls;
type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ViewGPU1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure UpdateText;
    procedure ViewGPU1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewGPU1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ViewGPU1Prepare(Sender: TObject);
    procedure ViewGPU1Paint(Sender: TObject);
  private
    //
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{$IFDEF METALAPI}
uses MetalPipeline, MetalUtils, MetalControl, Metal, VectorMath, mtlfont, {$IFDEF myTextures}mtltexture,{$ENDIF} SimdUtils;
{$ELSE}
uses glcorearb, OpenGLContext, gl_core_utils, VectorMath, glfont,{$IFDEF myTextures}gltexture,{$ENDIF} SimdUtils;
{$ENDIF}

var
  gMouseXY : TPoint =  (x:0; y:-1);
  gOffsetXY : TPoint =  (x:0; y:0);
  gClearColor: TVec4 =  (r: 0.4; g: 0.4;  b: 0.8; a: 1);
  gFontColor: TVec4 =  (r: 0.8; g: 0.8;  b: 0.5; a: 1);
  gZoom : single = 1;
  gStr : string = 'The quick brown fox jumped over the lazy dog';
  {$IFDEF METALAPI}
  ViewGPU1: TMetalControl;
  {$ELSE}
  ViewGPU1: TOpenGLControl;
  {$ENDIF}
  {$IFDEF myTextures}gTex: TGPUTexture;{$ENDIF}
  gText, gText2: TGPUFont;

procedure TForm1.ViewGPU1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Wheeldelta < 0 then
     gZoom := gZoom - 0.1
  else
      gZoom := gZoom + 0.1;
  if gZoom > 3 then
     gZoom := 3;
  if gZoom < 0.1 then
     gZoom := 0.1;
  UpdateText;
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 gMouseXY.x := X;
 gMouseXY.y := Y;
  {$IFDEF METALAPI} caption := 'multisample = '+inttostr(ViewGPU1.renderView.sampleCount);{$ENDIF}

end;

procedure TForm1.ViewGPU1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
begin
 if gMouseXY.y < 0 then exit; //mouse is not down
 gOffsetXY.x := gOffsetXY.x + (X - gMouseXY.x);
 gOffsetXY.y := gOffsetXY.y - (Y - gMouseXY.y);
 gMouseXY.x := X;
 gMouseXY.y := Y;
 UpdateText;
end;

procedure TForm1.UpdateText;
begin
     gText.ClearText;
     gText.TextOut(gOffsetXY.X,gOffsetXY.Y,gZoom, gStr);
     ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPU1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
  gMouseXY.y := -1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     FreeAndNil(gText);
     FreeAndNil(gText2);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  {$IFDEF METALAPI}
    ViewGPU1.invalidate;
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
    {$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
    ViewGPU1.Align:= alClient;
    ViewGPU1.OnMouseDown := @ViewGPU1MouseDown;
    ViewGPU1.OnMouseMove := @ViewGPU1MouseMove;
    ViewGPU1.OnMouseUp := @ViewGPU1MouseUp;
    ViewGPU1.OnMouseWheel := @ViewGPU1MouseWheel;
    ViewGPU1.OnPaint := @ViewGPU1Paint;
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

procedure TForm1.ViewGPU1Prepare(Sender: TObject);
// load any pipelines you need here using the procedural API or Objective Pascal
var
 fnm: string;
 success: boolean;
begin
  //create 1st font
  fnm := ResourceFile('Roboto', 'png');
  gText := TGPUFont.Create(fnm, success, ViewGPU1);
  if not success then
     showmessage('Error: unable to load .png and .fnt '+fnm);
  gText.FontColor := gFontColor;
  //gText.TextColor(gFontColor);//default
  //gText.ClearColor(gClearColor);//default
  //2nd font
  gText2 := TGPUFont.Create(fnm, success, ViewGPU1);
  {$IFDEF myTextures}gTex := TGPUTexture.Create(ResourceFile('texture', 'png'), ViewGPU1);{$ENDIF}
  if not success then
     showmessage('Error: unable to load default font ');
  gText.FontColor := vec4(0,0,0,1);

  //gText2.TextColor(vec4(0,0,0,1));//
  //gText2.ClearColor(gClearColor);//default
  gText2.TextOut(6,16,1,10, 'Sphinx of black quartz, judge my vow.');
  {$IFDEF METALAPI}
    ViewGPU1.SetPreferredFrameRate(0);
    Form1.OnResize := @FormResize;
  {$ENDIF}
  UpdateText;
end;

procedure TForm1.ViewGPU1Paint(Sender: TObject);
begin
  {$IFDEF METALAPI}
  MTLSetClearColor(MTLClearColorMake(gClearColor.r, gClearColor.g, gClearColor.b, 1));
  MTLBeginFrame;
   {$IFDEF myTextures}gTex.DrawTex();{$ENDIF}
   gText2.DrawText();
   gText.DrawText();
 MTLEndFrame;
  {$ELSE}
  glDisable(GL_DEPTH_TEST);
  glViewPort(0,0,ViewGPU1.ClientWidth, ViewGPU1.ClientHeight);
  glClearColor(gClearColor.r, gClearColor.g, gClearColor.b, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  {$IFDEF myTextures}gTex.DrawTex();{$ENDIF}
  gText2.DrawText();
  gText.DrawText();
  ViewGPU1.SwapBuffers;
  {$ENDIF}
end;

end.
