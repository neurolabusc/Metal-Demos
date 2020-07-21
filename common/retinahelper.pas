unit retinahelper;

interface

 {$include glopts.inc}

uses {$IFDEF COREGL}glcorearb, {$ELSE}gl, {$ENDIF} OpenGLContext;

type
  TCustomOpenGLControl = class helper for TOpenGLControl
    function clientWidth: Integer;
   function clientHeight: Integer;
   procedure setRetina(wantRetina: boolean);
   procedure enableTiledScreenShot(tileLeft, tileBottom,totalWidth, totalHeight: integer );
   procedure disableTiledScreenShot();
   procedure SetViewPort(); //same as glViewport(0,0,w,h) but handles tiled screenshots
   function retinaScale: Single;
end;

implementation
{$IFDEF LCLCocoa}
uses  glcocoanscontext;
{$ENDIF}

var
    fTileLeft, fTileBottom,fTotalWidth, fTotalHeight: integer;

procedure TCustomOpenGLControl.setRetina(wantRetina: boolean);
begin
  {$IFDEF LCLCocoa}
  LSetWantsBestResolutionOpenGLSurface(wantRetina, self.Handle);
  {$ENDIF}
end;

function TCustomOpenGLControl.ClientWidth: integer;
begin
  if (fTotalWidth > 0) then begin
    result := fTotalWidth;
    exit;
  end;
  {$IFDEF LCLCocoa}
  result := Round(width * LBackingScaleFactor(self.Handle));
  {$ELSE}
  result := width;
  {$ENDIF}
end;

function TCustomOpenGLControl.ClientHeight: integer;
begin
  if (fTotalHeight > 0) then begin
    result := fTotalHeight;
    exit;
  end;
  {$IFDEF LCLCocoa}
  result := Round(height * LBackingScaleFactor(self.Handle));
  {$ELSE}
   result := height;
  {$ENDIF}
end;

procedure TCustomOpenGLControl.SetViewPort();
var
    f: single;
    w,h: integer;
begin
  {$IFDEF LCLCocoa}
  f := LBackingScaleFactor(self.Handle);
  w := round(f*width);
  h := round(f*height);
  {$ELSE}
  w := width;
  h := height;
  {$ENDIF}
  if (fTotalHeight < 1) or (fTotalWidth < 1) then begin
       glViewport(0, 0, w, h);
      exit;
  end;
  //glViewport(fTileLeft, fTileBottom, w, h);
  glViewport(fTileLeft,fTileBottom, fTotalWidth, fTotalHeight);

end;

procedure TCustomOpenGLControl.enableTiledScreenShot(tileLeft, tileBottom, totalWidth, totalHeight: integer );
begin
  fTileLeft := tileLeft;
  fTileBottom := tileBottom;
  fTotalWidth := totalWidth;
  fTotalHeight := totalHeight;
end;



procedure TCustomOpenGLControl.disableTiledScreenShot();
begin
     fTileLeft := 0;
     fTileBottom := 0;
     fTotalWidth := 0;
     fTotalHeight := 0;
end;

function TCustomOpenGLControl.retinaScale: Single;
begin
  {$IFDEF LCLCocoa}
  result := LBackingScaleFactor(self.Handle);
  {$ELSE}
   result := 1;
  {$ENDIF}
end;


initialization
 fTotalWidth := 0;
 fTotalHeight := 0;

end.
