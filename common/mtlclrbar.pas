unit mtlclrbar;
//Metal color bars

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  MetalPipeline, MetalUtils, MetalControl, Metal, VectorMath, mtlfont,
  Classes, SysUtils, Graphics,  math, dialogs, SimdUtils;

type
 TLUTminmax = packed record
   LUT : TLUT;
   mn,mx: single;
 end;
const
  kMaxClrBar = 32;
type
  TGPUClrbar = class
  private
         LUTs: array [1..kMaxClrBar] of TLUTminmax;
         nLUTs, scrnW, scrnH: integer;
         SizeFrac : Single;
         FontClr,BackClr: TRGBA;
         pipeline: TMetalPipeline;
         fisVisible, fisVertical, fisTopOrRight, isRedraw, isText: boolean;
         indexBuffer, vertexBuffer: MTLBufferProtocol;
         mtlControl: TMetalControl;
         txt: TGPUFont;
         procedure CreateStrips();
         procedure CreateClrbar();
         procedure ScreenSize(nLUT,Width,Height: integer);
         procedure CreateTicksText(mn,mx: single; BarLength, BarTop, BarThick, fntScale: single);
         procedure SetVertical(isV: boolean);
         procedure SetTopOrRight(isTR: boolean);
         procedure SetBackColor(c: TRGBA);
         procedure SetFontColor(c: TRGBA);
         procedure SetSizeFrac(f: single);
         procedure InitShader;
  public
    property isVisible : boolean read fisVisible write fisVisible;
    property isVertical : boolean read fisVertical write SetVertical;
    property isTopOrRight : boolean read fisTopOrRight write SetTopOrRight;
    property Number: integer read nLUTs write nLUTs;
    property BackColor : TRGBA read BackClr write SetBackColor;
    property FontColor : TRGBA read FontClr write SetFontColor;
    property SizeFraction : single read SizeFrac write SetSizeFrac;
    function PanelFraction: single; //size of all color tables and surrounding border
    procedure Draw(nLUT: integer); overload; //must be called while TOpenGLControl is current context
    procedure Draw(); overload;  //must be called while TOpenGLControl is current context
    procedure SetLUT(index: integer; LUT: TLUT; mn,mx: single; isFromZero: boolean = false);
    constructor Create(fromView: TMetalControl);
    destructor Destroy; overload;
  end;

implementation

uses
  graphTicks;

const
    kBlockSz = 8192;
type
  TVtxClr = packed Record //each vertex has position and texture coordinates
    vtx   : TVec4; //position coordinates
    clr : TVec4; //texture coordinates
  end;

var
    g2Dvnc: array of TVtxClr;
    g2Drgba : TVec4;
    g2DNew: boolean;
    gnface: integer;

destructor TGPUClrbar.Destroy;
begin
  g2Dvnc := nil;
  txt.Free;
  inherited;
end;

function TGPUClrbar.PanelFraction (): single;
begin
  result := 0.0;
  if (not isVisible) or (nLUTs < 1) then exit; //nothing to do
  result := sizeFrac*((nLUTs * 2)+0.5);
end;

procedure TGPUClrbar.CreateStrips();
type
  TInts = array of uint16; //if uint32 then MTLIndexTypeUInt32
var
  i: integer;
  faces: TInts;
begin
  if gnface < 1 then exit;
  setlength(faces,gnface);
  for i := 0 to (gnface-1) do
      faces[i] := i;
  indexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@faces[0], sizeof(uint16) * Length(faces), MTLResourceStorageModeShared);
  setlength(faces, 0 );
  setlength(g2Dvnc,0);
end;

procedure nglBegin();
begin
     g2DNew := true;
end;

procedure nglColor4ub (r,g,b,a: byte);
begin
  g2Drgba.r := (r/255 );
  g2Drgba.g := (g/255 );
  g2Drgba.b := (b/255 );
  g2Drgba.a := (a/255 );
end;

procedure nglVertex3f(x,y,z: single);
var
  i: integer;
begin
  i := gnface; //array indexed from 0 not 1
  gnface := gnface + 1;
  if (gnface+1) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
   g2Dvnc[i].vtx.X := x;
   g2Dvnc[i].vtx.Y := y;
   g2Dvnc[i].vtx.Z := z;
   g2Dvnc[i].clr := g2Drgba;
   if not g2DNew then exit;
   g2DNew := false;
   g2Dvnc[gnface] := g2Dvnc[i];
   gnface := gnface + 1;
end;

procedure nglVertex2fr(x,y: single);
begin
     nglVertex3f(round(x),round(y), -1);
end;

procedure nglEnd;
var
  i: integer;
begin
     //add tail
     if gnface < 1 then exit;
     i := gnface; //array indexed from 0 not 1
     gnface := gnface + 1;
     if gnface > length(g2Dvnc) then
        setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
     //https://developer.apple.com/documentation/metal/mtlrendercommandencoder/1515520-drawindexedprimitives
     //primitive restart 0xFFFF for MTLIndexTypeUInt16 or 0xFFFFFFFF for MTLIndexTypeUInt32
     // however, degenerate triangles appear to work as well
     g2Dvnc[i] := g2Dvnc[i-1];
end;

function isSame(x,y: TRGBA): boolean;
begin
     result := (x.r = y.r) and (x.g = y.g) and (x.b = y.b) and (x.a = y.a);
end;

procedure TGPUClrbar.SetBackColor(c: TRGBA);
begin
     if not isSame(c, BackClr) then isRedraw := true;
     BackClr := c;
end;

procedure TGPUClrbar.SetFontColor(c: TRGBA);
begin
     if not isSame(c, FontClr) then isRedraw := true;
     FontClr := c;
end;

procedure TGPUClrbar.SetSizeFrac(f: single);
begin
     if (f <> sizeFrac) then isRedraw := true;
     sizeFrac := f;
     if sizeFrac < 0.005 then sizeFrac := 0.005;
     if sizeFrac > 0.25 then sizeFrac := 0.25;
end;

procedure TGPUClrbar.SetTopOrRight(isTR: boolean);
begin
     if (isTR <> fisTopOrRight) then isRedraw := true;
     fisTopOrRight := isTR;
end;

procedure TGPUClrbar.SetVertical(isV: boolean);
begin
     if (isV <> fisVertical) then isRedraw := true;
     fisVertical := isV;
end;

procedure TGPUClrbar.SetLUT(index: integer; LUT: TLUT; mn,mx: single; isFromZero: boolean = false);
var
  j,k: integer;
  frac: single;
begin
     if (index > kMaxClrBar) or (index < 1) then exit;
     LUTs[index].LUT := LUT;
     if (mn > mx) then begin
       frac := mx;
       mx := mn;
       mn := frac;
     end;
     LUTs[index].mn := mn;
     LUTs[index].mx := mx;
     isRedraw := true;
     if not isFromZero then exit;
     if (mn = mx) then exit;
     if ((mn > 0) or (mx < 0)) then begin //range does not cross zero
       if (mn > 0) then
         frac := mn/mx
       else
          frac := abs(mx)/abs(mn);
       for j := 1 to 255 do begin
          k := round (255 * (frac + ((1-frac) * j/255)));
          LUTs[index].LUT[j] := lut[k];
       end;
     end;
end;

procedure TGPUClrbar.ScreenSize(nLUT,Width,Height: integer);
begin
     if (nLUTs = nLUT) and (Width = scrnW) and (Height = scrnH) then exit;
     scrnW := Width;
     scrnH := Height;
     nLUTs := nLUT;
     isRedraw := true;
end;

function setRGBA(r,g,b,a: byte): TRGBA;
begin
     result.r := r;
     result.g := g;
     result.b := b;
     result.a := a;
end;

constructor TGPUClrbar.Create(fromView: TMetalControl);
begin
     mtlControl := fromView;
     scrnH := 0;
     fisVisible := true;
     SizeFrac := 0.05;
     pipeline := nil;
     FontClr := setRGBA(255, 255, 255, 255);
     BackClr:= setRGBA(0,0,0,156);
     fisVertical := false;
     fisTopOrRight := false;
     isRedraw := true;
     Txt := TGPUFont.Create(ResourcePath('Roboto', 'png'),  isText, fromView); //<-multi-channel channel fonts glmtext
end;

FUNCTION specialsingle (var s:single): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//4byte IEEE: msb[31] = signbit, bits[23-30] exponent, bits[0..22] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 255 shl 23;
VAR Overlay: LongInt ABSOLUTE s;
BEGIN
  IF ((Overlay AND kSpecialExponent) = kSpecialExponent) THEN
     RESULT := true
  ELSE
      RESULT := false;
END;

procedure TGPUClrbar.CreateTicksText(mn,mx: single; BarLength, BarTop, BarThick, fntScale: single);
var
  lRange, lStep, t, MarkerSzX,MarkerSzY, lPosX, lPosY, StWid: double;
  isInvert: boolean;
  ticMin, ticStep: double;
  ticDecimals: integer;
  St: string;
begin
  if (mx = mn) or (BarThick = 0) or (BarLength = 0) then exit;
  if specialsingle(mn) or specialsingle(mx) then exit;
  if (mx < mn) then begin
    t := mx;
    mx := mn;
    mn := t;
  end;
  isInvert :=  (mn < 0) and (mx < 0);
  MarkerSzX := BarThick * 0.2;
  if (MarkerSzX < 1) then MarkerSzX := 1;
  if not fisVertical then begin
     MarkerSzY := MarkerSzX;
     MarkerSzX := 1;
  end else
      MarkerSzY := 1;
  //next: compute increment
  SelectTicks(mn, mx, lStep, ticStep, ticDecimals);
  lRange := abs(mx - mn); //full range, in case mn < 0 and mx > 0
  nglColor4ub (FontClr.r,FontClr.g,FontClr.b,255);//outline
  repeat
        if not fisVertical then begin
           lPosX :=   (lStep-mn)/lRange*BarLength;
           if isInvert   then
              lPosX :=   BarLength - lPosX;
           lPosX := lPosX + BarThick;
           lPosY := BarTop;
        end else begin
           lPosX := BarTop + BarThick;
           lPosY :=  (lStep-mn)/lRange*BarLength;
           if isInvert   then
              lPosY :=   BarLength - lPosY;
           lPosY := lPosY + BarThick;
        end;
        nglColor4ub (FontClr.r,FontClr.b,FontClr.b,255);//outline
        nglBegin();
          nglVertex2fr(lPosX-MarkerSzX,lPosY-MarkerSzY);
          nglVertex2fr(lPosX-MarkerSzX,lPosY+MarkerSzY);
          nglVertex2fr(lPosX+MarkerSzX,lPosY-MarkerSzY);
          nglVertex2fr(lPosX+MarkerSzX,lPosY+MarkerSzY);
        nglEnd;
        if fntScale > 0 then begin
           St := FloatToStrF(lStep, ffFixed,7,ticDecimals);
           StWid := Txt.TextWidth(fntScale, St);
           if not fisVertical then
              Txt.TextOut(lPosX-(StWid*0.5),BarTop-(BarThick*0.82),fntScale, St)
           else
               Txt.TextOut(lPosX+(BarThick*0.82),lPosY-(StWid*0.5),fntScale,90, St)
        end;
        lStep := lStep + ticStep;
  until lStep > (mx+(ticStep*0.01));
end; //CreateTicksText()

procedure TGPUClrbar.CreateClrbar();
var
  BGThick, BarLength,BarThick, i,b,  t,tn: integer;
  frac, pos, fntScale: single;
begin
   if nLUTs < 1 then exit; //nothing to do
   if scrnW < scrnH then
      BarThick := round(scrnW * sizeFrac)
   else
       BarThick := round(scrnH * sizeFrac);
   if BarThick < 1 then exit;
   if not fisVertical then
      BarLength := ScrnW - BarThick - BarThick
   else
       BarLength := ScrnH - BarThick - BarThick;
   if BarLength < 1 then exit;
   BGThick := round(BarThick*((nLUTs * 2)+0.5));
   if isText then begin
         txt.ClearText;
   end;
   if fisTopOrRight then begin
      if not fisVertical then
            t := scrnH-BGThick
      else
          t := scrnW - BGThick;
   end else
       t := 0;
   fntScale := 0;
   if (BarThick > 9) and (isText) then begin
      fntScale := (BarThick*0.6)/txt.BaseHeight;
      Txt.TextColor(FontClr.R,FontClr.G,FontClr.B);//black
   end;
   gnface := 0;
   setlength(g2Dvnc, 0);
   nglColor4ub (BackClr.r, BackClr.g, BackClr.b,BackClr.a);
   nglBegin();
   //background
   if not fisVertical then begin
     nglVertex2fr(0,T+BGThick );
     nglVertex2fr(0,T);
     nglVertex2fr(scrnW,T+BGThick);
     nglVertex2fr(scrnW,T);
   end else begin //else vertical
       nglVertex2fr(T+BGThick,0 );
       nglVertex2fr(T,0);
       nglVertex2fr(T+BGThick,scrnH);
       nglVertex2fr(T+0, scrnH);
   end;
   nglEnd;
   frac := BarLength/255;
   for b := 1 to nLUTs do begin
       nglColor4ub (FontClr.R,FontClr.G,FontClr.B,255);//outline
       nglBegin();
       if not fisVertical then begin
           tn := T+BarThick*(((nLUTs - b) * 2)+1);
           nglVertex2fr(BarThick-1,tn+BarThick+1);
           nglVertex2fr(BarThick-1,tn-1);
           nglVertex2fr(BarLength+BarThick+1,tn+BarThick+1);
           nglVertex2fr(BarLength+BarThick+1,tn-1);
       end else begin
           tn := round(T+BarThick*(((b) * 2)-1.5));
           nglVertex2fr(tn+BarThick+1, BarThick-1);
           nglVertex2fr(tn-1, BarThick-1);
           nglVertex2fr(tn+BarThick+1, BarLength+BarThick+1);
           nglVertex2fr(tn-1, BarLength+BarThick+1);
       end;
       nglEnd;
       pos := BarThick;
       nglBegin();
       if LUTs[b].lut[0].a = 0 then
          nglColor4ub (LUTs[b].lut[1].r, LUTs[b].lut[1].g, LUTs[b].lut[1].b,255)
       else
           nglColor4ub (LUTs[b].lut[0].r, LUTs[b].lut[0].g, LUTs[b].lut[0].b,255);
       if not fisVertical then begin
          nglVertex2fr(pos,tn+BarThick );
          nglVertex2fr(pos,tn);
       end else begin
           nglVertex2fr(tn+BarThick,pos );
           nglVertex2fr(tn,pos);
       end;
       for i := 1 to 255 do begin
         pos := pos + frac;
         nglColor4ub (LUTs[b].lut[i].r, LUTs[b].lut[i].g, LUTs[b].lut[i].b,255);
         if not fisVertical then begin
            nglVertex2fr(pos,tn+BarThick);
            nglVertex2fr(pos,tn);
         end else begin
           nglVertex2fr(tn+BarThick,pos);
           nglVertex2fr(tn,pos);
         end;
       end;
       nglEnd;
       CreateTicksText(LUTs[b].mn,LUTs[b].mx, BarLength, tn, BarThick, fntScale);
   end;
   vertexBuffer := mtlControl.renderView.device.newBufferWithBytes_length_options(@g2Dvnc[0], gnface*SizeOf(TVtxClr), MTLResourceStorageModeShared);
   CreateStrips();
   isRedraw := false;
end;

procedure TGPUClrbar.InitShader;
var
 options: TMetalPipelineOptions;
 fnm: string;
begin
	if pipeline <> nil then exit;
	options := TMetalPipelineOptions.Default;
        fnm := ResourceDir + pathdelim + 'colorbar.metal';
        if not fileexists(fnm) then
           fnm := ShaderDir + pathdelim +  '_Colorbar.metal';
        options.libraryName := fnm;
        if not fileexists(options.libraryName) then begin
		writeln('Unable to find ' + fnm);
	end;
        options.pipelineDescriptor := MTLCreatePipelineDescriptor;
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setBlendingEnabled(true);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setRgbBlendOperation(MTLBlendOperationAdd);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setAlphaBlendOperation(MTLBlendOperationAdd);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceRGBBlendFactor(MTLBlendFactorSourceAlpha);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setSourceAlphaBlendFactor(MTLBlendFactorSourceAlpha);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationRGBBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
        options.pipelineDescriptor.colorAttachmentAtIndex(0).setDestinationAlphaBlendFactor(MTLBlendFactorOneMinusSourceAlpha);
	pipeline := MTLCreatePipeline(options);
end;

procedure TGPUClrbar.Draw(nLUT: integer); overload;
type
  TVertUniforms = record //Uniforms for vertex shader
  viewportSize: TVec2;
end;
var
   Width,Height: integer;
   vertUniforms: TVertUniforms;
begin
  if nLUT < 1 then exit;
  if not fisVisible then exit;
  InitShader;
  MTLSetCullMode(MTLCullModeNone);
  MTLSetShader(pipeline);
  Width := mtlControl.ClientWidth;
  Height := mtlControl.ClientHeight;
  ScreenSize(nLUT, Width,Height);
  if isRedraw then
   CreateClrbar();
  if gnface < 1 then exit;
  vertUniforms.viewportSize := V2(Width, Height);
  MTLSetVertexBytes(@vertUniforms, sizeof(vertUniforms), 1);
  MTLSetVertexBuffer(vertexBuffer, 0, 0);
  MTLDrawIndexed (MTLPrimitiveTypeTriangleStrip, gnface, MTLIndexTypeUInt16, indexBuffer, 0); //MTLIndexTypeUInt32
  if isText then
   Txt.DrawText();
end;

procedure TGPUClrbar.Draw(); overload;
begin
     Draw(nLUTs);
end;

end.

