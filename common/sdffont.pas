unit sdffont; //generic signed distance field typeface
{$mode objfpc}{$H+}
interface

uses
  VectorMath, Classes, SysUtils, math, strutils, Dialogs;

const
    kMaxChar = 2048; //maximum number of characters on screen, if >21845 change TPoint3i to uint32 and set glDrawElements to GL_UNSIGNED_INT
type
    TMetric = Packed Record //each vertex has position and texture coordinates
      x,y,xEnd,yEnd,w,h,xo,yo,xadv   : single; //position coordinates
    end;
    TMetrics = record
      M : array [0..255] of TMetric;
      lineHeight, base, scaleW, scaleH: single;
    end;
    Txyuv = packed Record //each vertex has position and texture coordinates
      x,y : TScalar; //position coordinates
      u,v : TScalar; //texture coordinates
    end;
    TRotMat = Packed Record //https://en.wikipedia.org/wiki/Rotation_matrix
      Xx,Xy, Yx,Yy: single;
    end;
    TQuad = array [0..3] of Txyuv; //each character rectangle has 4 vertices
TVertUniforms = record //Uniforms for vertex shader
  viewportSize: TVec2;
end;
TQuads = array[0..(kMaxChar-1)] of TQuad;

  TSDFFont = class
  private
    nChar: integer;
    isRedraw: boolean;
    quads: TQuads;
    metrics: TMetrics;
    fontClr: TVec4;
    procedure CharOut(x,y,scale: single; rx: TRotMat; asci: byte);
  public
    property NumChar: integer read nChar write nChar;
    property Redraw: boolean read isRedraw write isRedraw;
    property FontColor: TVec4 read fontClr write fontClr;
    property QuadVerts: TQuads read quads;
    procedure ClearText; //remove all previous drawn text
    procedure TextOut(x,y,scale: single; s: string); overload; //add line of text
    procedure TextOut(x,y,scale, angle: single; s: string); overload; //add line of text
    procedure TextColor(red,green,blue: byte); overload;
    function BaseHeight: single;
    function LineHeight: single;
    function TextWidth(scale: single; s: string): single;
    constructor Create(fnm : string; out success: boolean);
  end;

implementation


procedure TSDFFont.TextColor(red,green,blue: byte); overload;
begin
  FontClr.r := red/255;
  FontClr.g := green/255;
  FontClr.b := blue/255;
end;

function LoadMetricsJson(fnm: string; out fnt: TMetrics): boolean;
//load JSON format created by
// https://github.com/Jam3/msdf-bmfont
//Identical attributes to Hiero ASCII FNT format, just saved in JSON
const
  idKey = '"id"';
var
   pages, id, strBlockStart, strBlockEnd: integer;
   str: string;
   f: textfile;
function GetFntVal(key: string): single;
var
   p, pComma: integer;
begin
  result := 0;
  p := PosEx(key,str,strBlockStart);
  if (p < 1) or (p > strBlockEnd) then exit;
  p :=  p + length(key)+1;
  pComma := PosEx(',',str,p);
  if (pComma <= p) or (pComma > strBlockEnd) then exit;
  result := strtofloatdef(copy(str,p, pComma-p), 0);
end; //nested GetFntVal()
begin
  if (fnm <> '') then
     fnm := changefileext(fnm,'.json');
  result := false;
  for id := 0 to 255 do begin
      fnt.M[id].x := 0;
      fnt.M[id].y := 0;
      fnt.M[id].xEnd := 0;
      fnt.M[id].yEnd := 0;
      fnt.M[id].w := 0;
      fnt.M[id].h := 0;
      fnt.M[id].xo := 0;
      fnt.M[id].yo := 0;
      fnt.M[id].xadv := 0; //critical to set: fnt format omits non-graphical characters (e.g. DEL): we skip characters whete X-advance = 0
  end;
    if not fileexists(fnm) then begin
       showmessage('Unable to find '+fnm);
       exit;
    end;
    AssignFile(f, fnm);
    Reset(f);
    ReadLn(f, str);
    CloseFile(f);
  strBlockStart := PosEx('"common"',str,1);
  strBlockEnd := PosEx('}',str, strBlockStart);
  if (strBlockStart < 1) or (strBlockEnd < 1) then begin
     showmessage('Error: no "common" section');
     exit;
  end;
  fnt.lineHeight := GetFntVal('"lineHeight"');
  fnt.base := GetFntVal('"base"');
  fnt.scaleW := GetFntVal('"scaleW"');
  fnt.scaleH := GetFntVal('"scaleH"');
  pages := round(GetFntVal('"pages"'));
  if (pages <> 1) then begin
     showmessage('Only able to read single page fonts');
     exit;
  end;
  strBlockStart := 1;
  repeat
        strBlockStart := PosEx(idKey,str,strBlockStart);
        if strBlockStart < 1 then continue;
        strBlockEnd := PosEx('}',str, strBlockStart);
        if strBlockEnd < strBlockStart then
           break;
        id := round(GetFntVal(idKey));
        if id = 0 then begin
           strBlockStart := strBlockEnd;
           continue;
        end;
        fnt.M[id].x := GetFntVal('"x"');
        fnt.M[id].y := GetFntVal('"y"');
        fnt.M[id].w := GetFntVal('"width"');
        fnt.M[id].h := GetFntVal('"height"');
        fnt.M[id].xo := GetFntVal('"xoffset"');
        fnt.M[id].yo := GetFntVal('"yoffset"');
        fnt.M[id].xadv := GetFntVal('"xadvance"');
        strBlockStart := strBlockEnd;
  until strBlockStart < 1;
  if (fnt.scaleW < 1) or (fnt.scaleH < 1) then exit;
  for id := 0 to 255 do begin //normalize from pixels to 0..1
      fnt.M[id].yo := fnt.base - (fnt.M[id].h + fnt.M[id].yo);
      fnt.M[id].x:=fnt.M[id].x/fnt.scaleW;
      fnt.M[id].y:=fnt.M[id].y/fnt.scaleH;
      fnt.M[id].xEnd := fnt.M[id].x + (fnt.M[id].w/fnt.scaleW);
      fnt.M[id].yEnd := fnt.M[id].y + (fnt.M[id].h/fnt.scaleH);
  end;
  result := true;
end; //LoadMetricsJson()

procedure Rot(xK,yK, x,y: single; r: TRotMat; out Xout, Yout: single);
// rotate points x,y and add to constant offset xK,yK
begin
     Xout := xK + (x * r.Xx) + (y * r.Xy);
     Yout := yK + (x * r.Yx) + (y * r.Yy);
end; //Rot()

procedure TSDFFont.CharOut(x,y,scale: single; rx: TRotMat; asci: byte);
var
  //i: integer;
  q: TQuad;
  x0,x1,y0,y1: single;
begin
  if metrics.M[asci].w = 0 then exit; //nothing to draw, e.g. SPACE character
  if nChar > kMaxChar then nChar := 0; //overflow!
  x0 := (scale * metrics.M[asci].xo);
  x1 := x0 + (scale * metrics.M[asci].w);
  y0 := (scale * metrics.M[asci].yo);
  y1 := y0 + (scale * metrics.M[asci].h);
  Rot(x,y, x0, y0, rx, q[0].x, q[0].y);
  Rot(x,y, x0, y1, rx, q[1].x, q[1].y);
  Rot(x,y, x1, y0, rx, q[2].x, q[2].y);
  Rot(x,y, x1, y1, rx, q[3].x, q[3].y);
  q[0].u := metrics.M[asci].x;
  q[1].u := q[0].u;
  q[2].u := metrics.M[asci].xEnd;
  q[3].u := q[2].u;
  q[0].v := metrics.M[asci].yEnd;
  q[1].v := metrics.M[asci].y;
  q[2].v := q[0].v;
  q[3].v := q[1].v;
  quads[nChar] := q;
  isRedraw := true;
  nChar := nChar+ 1;
end; //CharOut()

procedure TSDFFont.TextOut(x,y,scale, angle: single; s: string); overload;
var
  i: integer;
  asci: byte;
  rx: TRotMat;
begin
  angle := DegToRad(angle);
  rx.Xx := cos(angle);
  rx.Xy := -sin(angle);
  rx.Yx := sin(angle);
  rx.Yy := cos(angle);
  if length(s) < 1 then exit;
  for i := 1 to length(s) do begin
      asci := ord(s[i]);
      if metrics.M[asci].xadv = 0 then continue; //not in dataset
      CharOut(x,y,scale,rx,asci);
      Rot(x,y, (scale * metrics.M[asci].xadv),0, rx, x, y);
  end;
end; //TextOut()

procedure TSDFFont.TextOut(x,y,scale: single; s: string); overload;
begin
     TextOut(x,y,scale,0,s);
end; //TextOut()

function TSDFFont.BaseHeight: single;
begin
  result := metrics.base;
end; //BaseHeight()

function TSDFFont.LineHeight: single;
begin
     result := metrics.lineHeight;
end; //LineHeight()

function TSDFFont.TextWidth(scale: single; s: string): single;
var
  i: integer;
  asci: byte;
begin
  result := 0;
  if length(s) < 1 then exit;
  for i := 1 to length(s) do begin
      asci := ord(s[i]);
      if metrics.M[asci].xadv = 0 then continue; //not in dataset
      result := result + (scale * metrics.M[asci].xadv);
  end;
end; //TextWidth()

procedure TSDFFont.ClearText;
begin
  nChar := 0;
end; //ClearText()

constructor TSDFFont.Create(fnm : string; out success: boolean);
begin
  fontClr := vec4(1,1,1,1);
  nChar := 0;
  isRedraw := false;
  success := LoadMetricsJson(fnm, metrics);
end; //Create()

end.
