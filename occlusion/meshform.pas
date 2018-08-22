unit meshForm;

{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
  {$DEFINE METALAPI}
  {$modeswitch objectivec1}
{$ENDIF}
{$IFNDEF METALAPI}
 This project does not yet support OpenGL
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Types, fileutil;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FlipMenu: TMenuItem;
    OcclusionMediumMenu: TMenuItem;
    OcclusionHeavyMenu: TMenuItem;
    OcclusionNoneMenu: TMenuItem;
    OcclusionLightMenu: TMenuItem;
    OcclusionMenu: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveMenu: TMenuItem;
    PerspectiveMenu: TMenuItem;
    ShaderMenu: TMenuItem;
    MeshColorMenu: TMenuItem;
    AppleMenu: TMenuItem;
    AboutMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    ViewMenu: TMenuItem;
    BackColorMenu: TMenuItem;
    OpenMenu: TMenuItem;
    procedure AboutMenuClick(Sender: TObject);
    procedure BackColorMenuClick(Sender: TObject);
    procedure FlipMenuClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MeshColorMenuClick(Sender: TObject);
    procedure OcclusionMenuClick(Sender: TObject);
    procedure PerspectiveMenuClick(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
    procedure LoadMesh;
    procedure ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenMenuClick(Sender: TObject);
    procedure ShaderMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ViewGPUMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUPrepare(Sender: TObject);
    procedure ViewGPUPaint(Sender: TObject);
  private
    //
  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}
{$IFDEF METALAPI}
uses
  SimdUtils, MetalPipeline, MetalUtils, MetalControl, Metal, mtlmesh_ssao;
const  kExt = '.metal';
{$ELSE}
uses OpenGLContext,  SimdUtils, glmesh, mesh, glcorearb, gl_core_utils;
const kExt = '.glsl';
{$ENDIF}
var
 gMouse : TPoint;
 gClearColor: TRGBA = (r:255; g:255; b:255; a:255);//(r:200; g:200; b:255; a:255);
 Mesh1: TGPUMesh;
 {$IFDEF METALAPI}
 ViewGPU1: TMetalControl;
 {$ELSE}
 ViewGPU1: TOpenGLControl;
 {$ENDIF}

procedure TForm1.LoadMesh;
begin
     Mesh1.OpenMesh(OpenDialog1.filename, FlipMenu.checked);
     ViewGPU1.Invalidate;
end;

procedure TForm1.OpenMenuClick(Sender: TObject);
begin
   if not OpenDialog1.execute then
      exit;
    LoadMesh;
end;

procedure TForm1.MeshColorMenuClick(Sender: TObject);
begin
 ColorDialog1.Color:= RGBToColor(Mesh1.MeshColor.r, Mesh1.MeshColor.g, Mesh1.MeshColor.b);
 if not ColorDialog1.Execute then exit;
 Mesh1.MeshColor := SetRGBA(Red(ColorDialog1.Color), Green(ColorDialog1.Color), Blue(ColorDialog1.Color),255);
 LoadMesh;
end;

procedure TForm1.OcclusionMenuClick(Sender: TObject);
begin
  Mesh1.Occlusion := (Sender as TMenuItem).tag / 100.0;
  ViewGPU1.Invalidate;
end;

procedure TForm1.PerspectiveMenuClick(Sender: TObject);
begin
  Mesh1.Perspective := PerspectiveMenu.checked;
  ViewGPU1.Invalidate;
end;

procedure TForm1.SaveMenuClick(Sender: TObject);
begin
 {$IFDEF METALAPI}
 if not SaveDialog1.execute then exit;
 Mesh1.SaveBmp(SaveDialog1.Filename);
 {$ELSE}
 SaveBmp(SaveDialog1.Filename, ViewGPU1);
 {$ENDIF}
end;

procedure TForm1.BackColorMenuClick(Sender: TObject);
begin
     ColorDialog1.Color := RGBToColor(gClearColor.r, gClearColor.g, gClearColor.b);
     if not ColorDialog1.Execute then exit;
     gClearColor := setRGBA(Red(ColorDialog1.Color), Green(ColorDialog1.Color), Blue(ColorDialog1.Color), 255);
     ViewGPU1.Invalidate;
end;

procedure TForm1.FlipMenuClick(Sender: TObject);
begin
  LoadMesh();
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  {$IFDEF METALAPI}
  ViewGPU1.Invalidate();
  {$ENDIF}
end;

procedure TForm1.AboutMenuClick(Sender: TObject);
begin
  MessageDlg('Mesh demo for reading PLY/OBJ files',mtInformation,[mbAbort, mbOK],0);
end;

procedure TForm1.ViewGPUMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 gMouse.Y := Y;
 gMouse.X := X;
end;

procedure TForm1.ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if gMouse.Y < 0 then exit; //mouse is not down
  Mesh1.Azimuth := Mesh1.Azimuth + (X - gMouse.X);
  Mesh1.Elevation := Mesh1.Elevation + (Y - gMouse.Y);
  while Mesh1.Azimuth > 360 do Mesh1.Azimuth := Mesh1.Azimuth - 360;
  while Mesh1.Azimuth < 0 do Mesh1.Azimuth := Mesh1.Azimuth + 360;
  if Mesh1.Elevation > 90 then Mesh1.Elevation := 90;
  if Mesh1.Azimuth < -90 then Mesh1.Elevation := -90;
  gMouse.X := X;
  gMouse.Y := Y;
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    gMouse.Y := -1; //released
    //caption := inttostr(trunc(ViewGPU1.renderView.drawableSize.width));
end;

procedure TForm1.ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Wheeldelta < 0 then
     Mesh1.Distance := Mesh1.Distance - 0.1
  else
      Mesh1.Distance := Mesh1.Distance + 0.1;
  if Mesh1.Distance > kMaxDistance then
     Mesh1.Distance := kMaxDistance;
  if Mesh1.Distance < 1 then
     Mesh1.Distance := 1.0;
  ViewGPU1.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
 i: integer;
 shaderName, shaderPath: string;
 shaderNames : TStringList;
 newMenu: TMenuItem;
begin
  gMouse.y := -1;
  OpenDialog1.Filename := ResourceFolderPath+pathdelim+'brain.mz3';
  {$IFDEF METALAPI}
  ViewGPU1 :=  TMetalControl.Create(Form1);
  ViewGPU1.OnPrepare := @ViewGPUPrepare;
  {$ELSE}
  ViewGPU1 := TOpenGLControl.Create(Form1);
  ViewGPU1.OpenGLMajorVersion:= 3;
  ViewGPU1.OpenGLMinorVersion:= 3;
  //Prepare(ViewGPU1);
  {$ENDIF}
  ViewGPU1.Parent := Form1;
  ViewGPU1.Align:= alClient;
  ViewGPU1.OnMouseDown := @ViewGPUMouseDown;
  ViewGPU1.OnMouseMove := @ViewGPUMouseMove;
  ViewGPU1.OnMouseUp := @ViewGPUMouseUp;
  ViewGPU1.OnMouseWheel := @ViewGPUMouseWheel;
  ViewGPU1.OnPaint := @ViewGPUPaint;
  Mesh1 := TGPUMesh.Create(ViewGPU1, OpenDialog1.Filename);
  {$IFNDEF METALAPI}
  ViewGPU1.MakeCurrent(false);
  if (not  Load_GL_version_3_3_CORE) then begin
     showmessage('Unable to load OpenGL 3.3 Core');
     halt;
  end;
  Form1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  ViewGPU1.ReleaseContext;
  Mesh1.Prepare(ViewGPU1);
  {$ENDIF}
  //auto generate shaders
  shaderPath := ResourceFolderPath;
  if not fileexists(shaderPath) then exit;
  shaderNames := FindAllFiles(shaderPath, '*'+kExt, true);
  if shaderNames.Count > 0 then begin
     shaderNames.Sort;
     for i := 0 to (shaderNames.Count-1) do begin
         shaderName := ChangeFileExt(ExtractFileName(shaderNames[i]),'');
         if (length(shaderName) < 1) or (shaderName[1]='_') or (shaderName[1] = '.')  then continue;
         newMenu := TMenuItem.Create(MainMenu);
         newMenu.Caption := shaderName;
         newMenu.OnClick:= @ShaderMenuClick;
         newMenu.GroupIndex:=132;
         newMenu.AutoCheck:=true;
         newMenu.RadioItem:=true;
         ShaderMenu.Add(newMenu);
     end;
  end;
  shaderNames.Free;
end;

procedure TForm1.ViewGPUPrepare(Sender: TObject);
begin
  Mesh1.Prepare();
  {$IFDEF METALAPI}
  ViewGPU1.setPreferredFrameRate(0);
  Form1.OnResize := @FormResize;
  {$ENDIF}
end;

procedure TForm1.ShaderMenuClick(Sender: TObject);
var
 shaderName: string;
begin
 shaderName := ResourceFolderPath + pathdelim + (Sender as TMenuItem).caption+kExt;
 Mesh1.SetShader(shaderName);
 ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPUPaint(Sender: TObject);
begin
 {$IFDEF METALAPI}
 MTLSetClearColor(MTLClearColorMake(gClearColor.r/255, gClearColor.g/255, gClearColor.b/255, 0.0));
 {$ELSE}
 glClearColor(gClearColor.R/255, gClearColor.G/255, gClearColor.B/255, 1.0);
 {$ENDIF}
 Mesh1.Paint();
end;

end.

