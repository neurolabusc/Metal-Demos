unit graphmain;

{$mode objfpc}{$H+}
{$IFDEF LCLCocoa}
 //MetalAPI supported on modern MacOS: disable for Linux, Windows and old MacOS
  //{$DEFINE METALAPI}
  {$modeswitch objectivec1}
{$ENDIF}
{$IFDEF LCLCarbon}
  MacOS must use Cocoa, regardless of whether OpenGL Core or Metal is used.
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Clipbrd,
  Controls, Dialogs, ExtCtrls, Menus, SimdUtils;
type
  { TForm1 }
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    CopyMenu: TMenuItem;
    ColorSchemeG: TMenuItem;
    ColorSchemeDarkB: TMenuItem;
    NodeMarkerMenu: TMenuItem;
    SaveMenu: TMenuItem;
    OpenMenu: TMenuItem;
    MenuItem6: TMenuItem;
    ColorSchemeW: TMenuItem;
    ColorSchemeB: TMenuItem;
    ColorSchemeBk: TMenuItem;
    DemeanMenu: TMenuItem;
    PercentMenu: TMenuItem;
    Normalize01Menu: TMenuItem;
    NormalizeMenu: TMenuItem;
    RawMenu: TMenuItem;
    ModeMenu: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure ColorSchemeWClick(Sender: TObject);
    procedure CopyMenuClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NodeMarkerMenuClick(Sender: TObject);
    procedure OpenMenuClick(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
    procedure ViewGPU1Prepare(Sender: TObject);
    procedure ScaleMenu(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ViewGPU1Paint(Sender: TObject);
    procedure ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);

  private

  public

  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

{$IFDEF METALAPI}
uses Metal, MetalPipeline, MetalControl, mtlgraph;
{$ELSE}
uses {$IFDEF LCLCocoa}retinahelper,{$ENDIF} glcorearb, OpenGLContext, gl_core_utils, gllines, glgraph;
{$ENDIF}
var
   gGraph: TGPUGraph;
  {$IFDEF METALAPI}
  ViewGPU1: TMetalControl;
  {$ELSE}
  ViewGPU1: TOpenGLControl;
  {$ENDIF}
procedure TForm1.ViewGPU1MouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
 caption := floattostr(gGraph.HorizontalClickFrac(X));
end;


procedure TForm1.ViewGPU1Prepare(Sender: TObject);
begin
  {$IFDEF METALAPI}
  ViewGPU1.SetPreferredFrameRate(0);
  Form1.OnResize := @FormResize;
  {$ENDIF}
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  {$IFDEF METALAPI}
  ViewGPU1.Invalidate;
  {$ENDIF}
end;

procedure TForm1.NodeMarkerMenuClick(Sender: TObject);
begin
  gGraph.isMarker := NodeMarkerMenu.Checked;
  gGraph.isRedraw:= true;
  ViewGPU1.Invalidate;
end;

procedure TForm1.OpenMenuClick(Sender: TObject);
var
  openDlg: TOpenDialog;
begin
  openDlg := TOpenDialog.Create(application);
  openDlg.Title := 'Select text file with graph data';
  openDlg.InitialDir := GetCurrentDir;
  openDlg.Filter := 'Text file|*.txt|Any file|*.*';
  openDlg.DefaultExt := 'txt';
  openDlg.FilterIndex := 1;
  if not openDlg.Execute then begin
     openDlg.Free;
     exit;
  end;
  if not gGraph.LoadText(openDlg.Filename) then
     showmessage('Unable to load text');
  openDlg.Free;
  ViewGPU1.Invalidate;
end;

procedure TForm1.SaveMenuClick(Sender: TObject);
var
  saveDlg: TSaveDialog;
  strs: TStringList;
  resp : integer;
begin
  resp := MessageDlg('Export X axis data as the first column?', mtConfirmation,[mbCancel, mbYes, mbNo], 0);
  if resp = mrCancel then exit;
  strs := gGraph.AsText(resp = mrYes);
  if (strs.Count < 1) then begin
     showmessage('Unable to convert graph to text');
     strs.Free;
     exit;
  end;
  saveDlg := TSaveDialog.Create(application);
  saveDlg.Title := 'Export graph data as text';
  saveDlg.InitialDir := GetCurrentDir;
  saveDlg.Filter := 'Text file|*.txt';
  saveDlg.DefaultExt := 'txt';
  saveDlg.FilterIndex := 1;
  if not saveDlg.Execute then begin
     saveDlg.Free;
     strs.Free;
     exit;
  end;
  strs.SaveToFile(saveDlg.Filename);
  strs.Free;
  saveDlg.Free;

end;

procedure TForm1.ColorSchemeWClick(Sender: TObject);
begin
  case (Sender as TMenuItem).tag of
      1: gGraph.GrayColorScheme();
      2: gGraph.DarkColorScheme();
      3: gGraph.BlueColorScheme();
      4: gGraph.DarkBlueColorScheme();
      else gGraph.LightColorScheme();
  end;
  gGraph.isRedraw := true;
  ViewGPU1.Invalidate;
end;

procedure TForm1.CopyMenuClick(Sender: TObject);
var
      strs: TStringList;
begin
  strs := gGraph.AsText();
  Clipboard.AsText:= strs.Text;
  strs.free;
end;

procedure TForm1.FormShow(Sender: TObject);
const
  k = 38;
var
   newVals: TFloat32s;
   i: integer;
begin
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
  ViewGPU1.Align:= alClient;
  ViewGPU1.OnPaint := @ViewGPU1Paint;
  ViewGPU1.OnMouseDown := @ViewGPU1MouseDown;
  {$IFDEF METALAPI}ViewGPU1.renderView.setSampleCount(4);{$ENDIF}
  {$IFNDEF METALAPI}
  ViewGPU1.MakeCurrent(false);
  {$IFDEF LCLCocoa}
  ViewGPU1.setRetina(true);
  {$ENDIF}
  if (not  Load_GL_version_3_3_CORE) then begin
     showmessage('Unable to load OpenGL 3.3 Core');
     halt;
  end;
  Form1.caption := glGetString(GL_VENDOR)+'; OpenGL= '+glGetString(GL_VERSION)+'; Shader='+glGetString(GL_SHADING_LANGUAGE_VERSION);
  ViewGPU1.ReleaseContext;
  if GLErrorStr <> '' then begin
     showmessage(GLErrorStr);
     GLErrorStr := '';
  end;
  {$ENDIF}
  gGraph := TGPUGraph.Create(ViewGPU1);
  setlength(newVals, k);
  for i := 0 to k-1 do
      newVals[i] := i* 0.5;
  gGraph.AddLine(newVals, 'linear');
  for i := 0 to k-1 do
      newVals[i] := (3 * sin(i/(k/12))) + 6;
  gGraph.AddLine(newVals, 'Sin');
  for i := 0 to k-1 do
      newVals[i] := (3 * cos(i/(k/12))) + 4;
  gGraph.AddLine(newVals, 'Cos');
  for i := 0 to k-1 do
      newVals[i] := 0.5;
  gGraph.AddLine(newVals, '0.5');
  newVals := nil;
  ViewGPU1.Invalidate;
  gGraph.LoadText('/Users/chris/brik/afni/Right.1D');
end;

procedure TForm1.ScaleMenu(Sender: TObject);
begin
  gGraph.Style := (sender as TMenuItem).tag;
  gGraph.isRedraw := true;
  ViewGPU1.Invalidate;
end;

procedure TForm1.ViewGPU1Paint(Sender: TObject);
begin
  gGraph.Paint(ViewGPU1);
end;

end.

