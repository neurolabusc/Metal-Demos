unit gllines;
//openGL lines

{$mode objfpc}{$H+}
interface


uses
  glcorearb, gl_core_utils, VectorMath, SimdUtils, Classes, SysUtils, Graphics, OpenGLContext, dialogs;

type
  TGPULines = class
  private
         uniform_viewportSize: GLint;
         vaoLine2D, vboLine2D, shaderProgram: GLuint;
         numVertices: integer;
         LineWid: single;
         LineClr: TRGBA;
         isRedraw: boolean;
         glControl: TOpenGLControl;
  public
    property NumberOfVertices : integer read numVertices;
    property LineWidth : single read LineWid write LineWid;
    property LineColor : TRGBA read LineClr write LineClr;
    procedure AddLine(startX,startY,endX,endY: single); overload;
    procedure AddLine(startXY, endXY: TVec2); overload;
    procedure ClearLines();
    procedure Draw(); //must be called while TOpenGLControl is current context
    constructor Create(fromView: TOpenGLControl);
  end;

implementation

type
  TPoint3f = Packed Record
    x,y,z: single;
  end;

TVtxClr = Packed Record
  vtx   : TPoint3f; //vertex coordinates
  clr : TRGBA;
end;

var
    g2Dvnc: array of TVtxClr;
    const
        kBlockSz = 8192;
        kVert2D ='#version 330'
    +#10'layout(location = 0) in vec3 Vert;'
    +#10'layout(location = 3) in vec4 Clr;'
    +#10'out vec4 vClr;'
    +#10'uniform vec2 ViewportSize;'
    +#10'void main() {'
    +#10'    vec2 ptx = Vert.xy - 0.5;'
    +#10'    ptx -= (ViewportSize/2.0);'
    +#10'    gl_Position = vec4((ptx / (ViewportSize/2)), 0.0, 1.0);'
    +#10'    vClr = Clr;'
    +#10'}';
        kFrag2D = '#version 330'
    +#10'in vec4 vClr;'
    +#10'out vec4 color;'
    +#10'void main() {'
    +#10'    color = vClr;'
    +#10'}';


constructor TGPULines.Create(fromView: TOpenGLControl);
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_CLR = 3;   //color RGBA are positions 3,4,5,6
begin
  glControl := fromView;
  LineClr := setRGBA(255, 255, 255, 255);
  LineWid := 10;
  isRedraw := true;
  glControl.MakeCurrent();
  shaderProgram :=  initVertFrag(kVert2D, kFrag2D);
  uniform_viewportSize := glGetUniformLocation(shaderProgram, pAnsiChar('ViewportSize'));
  //setup VAO for lines
  vboLine2D := 0;
  vaoLine2D := 0;
  glGenVertexArrays(1, @vaoLine2D);
  glGenBuffers(1, @vboLine2D);
  glBindVertexArray(vaoLine2d);
  glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
  // Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  // Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxClr), PChar( sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);  //required, even if we will bind it next
  //done
  glFinish;
  glControl.ReleaseContext;
end;

function pt(x,y: single): TPoint3f;
begin
  result.x := x;
  result.y := y;
end;

procedure TGPULines.ClearLines();
begin
     numVertices := 0;
end;
//{$DEFINE STRIP}
{$IFDEF STRIP}
procedure TGPULines.AddLine(startX,startY,endX,endY: single); overload;
var
  i: integer;
  nx,ny, len: single;
begin
  ny :=  (startX-endX);
  nx :=  (startY-endY);
  len := sqrt(sqr(nx)+sqr(ny));
  if (len = 0) then exit;
  nx := 0.5*lineWid*nx/len;
  ny := 0.5*lineWid*ny/len;
  if (numVertices+6) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
  for i := 0 to 5 do
      g2Dvnc[numVertices +i].clr := LineClr;
  g2Dvnc[numVertices+0].vtx := pt(startX+nx,startY-ny);
  g2Dvnc[numVertices+1].vtx := g2Dvnc[numVertices+0].vtx;
  g2Dvnc[numVertices+2].vtx := pt(startX-nx,startY+ny);
  g2Dvnc[numVertices+3].vtx := pt(endX+nx,endY-ny);
  g2Dvnc[numVertices+4].vtx := pt(endX-nx,endY+ny);
  g2Dvnc[numVertices+5] := g2Dvnc[numVertices+4];
  numVertices := numVertices + 6;
  isRedraw := true;
end;
{$ELSE}
procedure TGPULines.AddLine(startX,startY,endX,endY: single); overload;
var
  i: integer;
  nx,ny, len: single;
begin
  ny :=  (startX-endX);
  nx :=  (startY-endY);
  len := sqrt(sqr(nx)+sqr(ny));
  if (len = 0) then exit;
  nx := 0.5*lineWid*nx/len;
  ny := 0.5*lineWid*ny/len;
  if (numVertices+6) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
  for i := 0 to 5 do
      g2Dvnc[numVertices +i].clr := LineClr;
  g2Dvnc[numVertices+0].vtx := pt(startX+nx,startY-ny);
  g2Dvnc[numVertices+1].vtx := pt(startX-nx,startY+ny);
  g2Dvnc[numVertices+2].vtx := pt(endX+nx,endY-ny);
  g2Dvnc[numVertices+3].vtx := g2Dvnc[numVertices+1].vtx;
  g2Dvnc[numVertices+4].vtx := g2Dvnc[numVertices+2].vtx;
  g2Dvnc[numVertices+5].vtx := pt(endX-nx,endY+ny);
  numVertices := numVertices + 6;
  isRedraw := true;
end;
{$ENDIF}

procedure TGPULines.AddLine(startXY, endXY: TVec2); overload;
begin
     AddLine(startXY.X, startXY.Y, endXY.X, endXY.Y);
end;

procedure TGPULines.Draw();
begin
  if isRedraw then begin
    glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
    glBufferData(GL_ARRAY_BUFFER, numVertices*SizeOf(TVtxClr), @g2Dvnc[0], GL_STATIC_DRAW);
    isRedraw := false;
  end;
  if numVertices < 1 then exit;
  glDisable(GL_CULL_FACE);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram(shaderProgram);
  glUniform2f(uniform_viewportSize, glControl.ClientWidth, glControl.ClientHeight);
  glBindBuffer(GL_ARRAY_BUFFER, vboLine2D);
  glBindVertexArray(vaoLine2d);
  {$IFDEF STRIP}
  glDrawArrays(GL_TRIANGLE_STRIP, 0, numVertices);
  {$ELSE}
  glDrawArrays(GL_TRIANGLES, 0, numVertices);
  {$ENDIF}
  glBindVertexArray(0);
  glUseProgram(0);
end;

end.

