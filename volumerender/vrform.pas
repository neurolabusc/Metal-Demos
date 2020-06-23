unit vrForm;

{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
   //{$DEFINE METALAPI} //set in ProjectOptions/CompilerOptions/CustomOptions
   {$modeswitch objectivec1}
{$ENDIF}
{$IFDEF LCLCarbon}
  error: you must compile for the Cocoa widgetset (ProjectOptions/Additions&Overrides)
  MacOS must use Cocoa, regardless of whether OpenGL Core or Metal is used.
{$ENDIF}

interface

uses
  StdCtrls, SimdUtils, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Types, fileutil;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ContrastMenu: TMenuItem;
    EditMenu: TMenuItem;
    CopyMenu: TMenuItem;
    NewInstanceMenu: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveMenu: TMenuItem;
    ShaderMenu: TMenuItem;
    AppleMenu: TMenuItem;
    AboutMenu: TMenuItem;
    OpenDialog1: TOpenDialog;
    ViewMenu: TMenuItem;
    BackColorMenu: TMenuItem;
    OpenMenu: TMenuItem;
    procedure AboutMenuClick(Sender: TObject);
    procedure BackColorMenuClick(Sender: TObject);
    procedure ContrastMenuClick(Sender: TObject);
    procedure CopyMenuClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewInstanceMenuClick(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
    procedure ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenMenuClick(Sender: TObject);
    procedure ShaderMenuClick(Sender: TObject);
    //procedure FormCreate(Sender: TObject);
    procedure ViewGPUMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    //procedure ViewGPUPrepare(Sender: TObject);
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
  MetalPipeline,  Metal,
  MetalControl, mtlvolume, loadNifti;
const  kExt = '.metal';
{$ELSE}
uses OpenGLContext,  glvolume, glcorearb, gl_core_utils, loadNifti;
const kExt = '.glsl';
{$ENDIF}
var
 gClearColor: TRGBA = (r: 200; g:200; b:255; a: 255);
 gMouse : TPoint;
 Vol1: TGPUVolume;
 niftiVol: TNIfTI;
 {$IFDEF METALAPI}
 ViewGPU1: TMetalControl;
 {$ELSE}
 ViewGPU1: TOpenGLControl;
 {$ENDIF}

procedure TForm1.ContrastMenuClick(Sender: TObject);
// function  SimplifyPref(out Tol, minLength: single): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    minLabel, maxLabel: TLabel;
    minEdit, maxEdit: TEdit;
    mn, mx: single;
    isOK: boolean;
begin
  PrefForm:=TForm.Create(nil);
  PrefForm.SetBounds(100, 100, 520, 112);
  PrefForm.Caption:= Format('Contrast: brightness range %g..%g',[niftiVol.VolumeMin, niftiVol.VolumeMax]);
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //min
  minLabel:=TLabel.create(PrefForm);
  minLabel.Caption:= 'Minimum intensity (darker voxels transparent)';
  minLabel.Left := 8;
  minLabel.Top := 12;
  minLabel.Parent:=PrefForm;
  minEdit:=TEdit.create(PrefForm);
  minEdit.Caption := FloatToStrF(niftiVol.DisplayMin, ffGeneral, 8, 4);
  minEdit.Top := 12;
  minEdit.Width := 92;
  minEdit.Left := PrefForm.Width - minEdit.Width - 8;
  minEdit.Parent:=PrefForm;
  //max
  maxLabel:=TLabel.create(PrefForm);
  maxLabel.Caption:= 'Maximum intensity';
  maxLabel.Left := 8;
  maxLabel.Top := 42;
  maxLabel.Parent:=PrefForm;
  maxEdit:=TEdit.create(PrefForm);
  maxEdit.Caption := FloatToStrF(niftiVol.DisplayMax, ffGeneral, 8, 4);
  maxEdit.Top := 42;
  maxEdit.Width := 92;
  maxEdit.Left := PrefForm.Width - maxEdit.Width - 8;
  maxEdit.Parent:=PrefForm;
  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  OkBtn.Top := 72;
  OkBtn.Width := 128;
  OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  PrefForm.ShowModal;
  mn := StrToFloatDef(minEdit.Caption, niftiVol.DisplayMin);
  mx := StrToFloatDef(maxEdit.Caption, niftiVol.DisplayMax);
  isOK :=  PrefForm.ModalResult = mrOK;
  FreeAndNil(PrefForm);
  if not isOK then exit;
  niftiVol.SetDisplayMinMax(mn, mx);
  ViewGPU1.Invalidate;
end;

procedure TForm1.CopyMenuClick(Sender: TObject);
begin
 {$IFDEF METALAPI}
 Vol1.SaveBmp('');
 {$ELSE}
 Showmessage('Not implemented');
 {$ENDIF}
end;

procedure TForm1.NewInstanceMenuClick(Sender: TObject);
begin
  //Application.
  Application.CreateForm(TForm1, Form1);
end;


procedure TForm1.OpenMenuClick(Sender: TObject);
begin
   if not OpenDialog1.execute then
      OpenDialog1.filename := '';
   niftiVol.Load(OpenDialog1.filename);
   ViewGPU1.Invalidate;
end;

procedure TForm1.BackColorMenuClick(Sender: TObject);
begin
     ColorDialog1.Color:= RGBToColor(gClearColor.r, gClearColor.g, gClearColor.b);
     if not ColorDialog1.Execute then exit;
     gClearColor := SetRGBA(Red(ColorDialog1.Color), Green(ColorDialog1.Color), Blue(ColorDialog1.Color), 255);
     ViewGPU1.Invalidate;
end;


procedure TForm1.SaveMenuClick(Sender: TObject);
begin
     if not SaveDialog1.execute then exit;
     {$IFDEF METALAPI}
     Vol1.SaveBmp(SaveDialog1.Filename);
     {$ELSE}
     SaveBmp(SaveDialog1.Filename, ViewGPU1);
     {$ENDIF}
end;

procedure TForm1.AboutMenuClick(Sender: TObject);
begin
  MessageDlg('Volume rendering for NIfTI images',mtInformation,[mbAbort, mbOK],0);
end;

procedure TForm1.ViewGPUMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 gMouse.Y := Y;
 gMouse.X := X;
 {$IFDEF METALAPI} caption := 'multisample = '+inttostr(ViewGPU1.renderView.sampleCount);{$ENDIF}
end;

procedure TForm1.ViewGPUMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if gMouse.Y < 0 then exit; //mouse is not down
  Vol1.Azimuth := Vol1.Azimuth + (X - gMouse.X);
  Vol1.Elevation := Vol1.Elevation + (Y - gMouse.Y);
  while Vol1.Azimuth > 360 do Vol1.Azimuth := Vol1.Azimuth - 360;
  while Vol1.Azimuth < 0 do Vol1.Azimuth := Vol1.Azimuth + 360;
  if Vol1.Elevation > 90 then Vol1.Elevation := 90;
  if Vol1.Azimuth < -90 then Vol1.Elevation := -90;
  gMouse.X := X;
  gMouse.Y := Y;
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPUMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    gMouse.Y := -1; //released
end;

procedure TForm1.ViewGPUMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Wheeldelta < 0 then
     Vol1.Distance := Vol1.Distance - 0.1
  else
      Vol1.Distance := Vol1.Distance + 0.1;
  if Vol1.Distance > kMaxDistance then
     Vol1.Distance := kMaxDistance;
  if Vol1.Distance < 1 then
     Vol1.Distance := 1.0;
  ViewGPU1.Invalidate;
end;

procedure TForm1.FormShow(Sender: TObject);
var
 i: integer;
 shaderPath, shaderName: string;
 shaderNames : TStringList;
 newMenu: TMenuItem;
begin
  {$IFNDEF Darwin}
  AppleMenu.visible := false;
  {$ENDIF}
  gMouse.y := -1;
  niftiVol := TNIfTI.Create();
  {$IFDEF METALAPI}
  ViewGPU1 :=  TMetalControl.Create(Form1);
  //{$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
  //ViewGPU1.renderView.setPreferredFramesPerSecond(0);
  //ViewGPU1.OnPrepare := @ViewGPUPrepare;
  {$ELSE}
  ViewGPU1 :=  TOpenGLControl.Create(Form1);
  ViewGPU1.OpenGLMajorVersion:= 3;
  ViewGPU1.OpenGLMinorVersion:= 3;
  {$ENDIF}
  ViewGPU1.Parent := Form1;
  ViewGPU1.Align:= alClient;
  ViewGPU1.OnMouseDown := @ViewGPUMouseDown;
  ViewGPU1.OnMouseMove := @ViewGPUMouseMove;
  ViewGPU1.OnMouseUp := @ViewGPUMouseUp;
  ViewGPU1.OnMouseWheel := @ViewGPUMouseWheel;
  ViewGPU1.OnPaint := @ViewGPUPaint;
  {$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
  Vol1 := TGPUVolume.Create(ViewGPU1);
  {$IFNDEF METALAPI}
  ViewGPU1.MakeCurrent(false);
  if (not  Load_GL_version_3_3_CORE) then begin
     showmessage('Unable to load OpenGL 3.3 Core');
     halt;
  end;
  Form1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  ViewGPU1.ReleaseContext;
  //Vol1.Prepare();
  {$ENDIF}
  //auto generate shaders
  shaderPath := ResourceDir;
  if not DirectoryExists(shaderPath) then exit;
  shaderNames := FindAllFiles(shaderPath, '*'+kExt, true);
  if shaderNames.Count > 0 then begin
     shaderNames.Sort;
     for i := 0 to (shaderNames.Count-1) do begin
         shaderName := ChangeFileExt(ExtractFileName(shaderNames[i]),'');
         if (length(shaderName) < 1) or (shaderName[1] = '_') or (shaderName[1] = '.')  then
            continue;
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

procedure TForm1.ShaderMenuClick(Sender: TObject);
var
 shaderName: string;
begin
 shaderName := ResourceDir + pathdelim + (Sender as TMenuItem).caption+kExt;
 Vol1.SetShader(shaderName);
 ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPUPaint(Sender: TObject);
begin
 {$IFDEF METALAPI}
 MTLSetClearColor(MTLClearColorMake(gClearColor.r/255, gClearColor.g/255, gClearColor.b/255, 1));
 {$ELSE}
 glClearColor(gClearColor.R/255, gClearColor.G/255, gClearColor.B/255, 1.0);
 {$ENDIF}
 Vol1.Paint(niftiVol);
end;

end.

