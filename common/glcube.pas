unit glcube;
//openGL cube

{$mode objfpc}{$H+}

interface

uses
  SimdUtils, VectorMath, glcorearb, gl_core_utils,
  Classes, SysUtils, Graphics, OpenGLContext, math, dialogs;

type
  TGPUCube = class
  private
    uniform_mtx: GLint;
    vbo_point,vao_point2d, shaderProgram: GLuint;
    fAzimuth, fElevation,SizeFrac : Single;
    scrnW, scrnH: integer;
    isRedraw, isTopLeft: boolean;
    procedure SetIsTopLeft(f: boolean);
    procedure SetSize(f: single);
    procedure SetAzimuth(f: single);
    procedure SetElevation(f: single);
    procedure  ScreenSize(Width,Height: integer);
    procedure CreateCube(sz: single);
  public
    property TopLeft : boolean read isTopLeft write SetIsTopLeft;
    property Azimuth : single read fAzimuth write SetAzimuth;
    property Elevation : single read fElevation write fElevation;
    property Size : single read SizeFrac write SetSize;
    procedure Draw(Width,Height: integer); //must be called while TOpenGLControl is current context
    constructor Create(Ctx: TOpenGLControl);
  end;

implementation

type


TVtxClr = Packed Record
  vtx   : TVec3; //vertex coordinates
  clr : TRGBA;
end;
TVtxClrRA = array of TVtxClr;

const
kVert2D ='#version 330'
  +#10'layout(location = 0) in vec3 Vert;'
  +#10'layout(location = 3) in vec4 Clr;'
  +#10'out vec4 vClr;'
  +#10'uniform mat4 ModelViewProjectionMatrix;'
  +#10'void main() {'
  +#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
  +#10'    vClr = Clr;'
  +#10'}';
kFrag2D = '#version 330'
  +#10'in vec4 vClr;'
  +#10'out vec4 color;'
  +#10'void main() {'
  +#10'    color = vClr;'
  +#10'}';

procedure MakeCube(sz: single; var vtxClrs: TVtxClrRA); //draw a cube of size sz
var
  nface: integer;
  clr : TRGBA;
procedure vertex3f(x,y,z: single; rep: boolean = false);
begin
 vtxClrs[nface].vtx.X := x;
 vtxClrs[nface].vtx.Y := y;
 vtxClrs[nface].vtx.Z := z;
 vtxClrs[nface].clr := clr;
 nface := nface + 1;
 if not rep then exit;
 vtxClrs[nface] := vtxClrs[nface-1];
 nface := nface + 1;
end;
begin
  setlength(vtxClrs, 36);
  nface := 0;
  //bottom
  clr := setRGBA(52, 52, 52, 255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(-sz, sz, -sz);
  vertex3f(sz, -sz, -sz);
  vertex3f(sz, sz, -sz, true);
  //top
  clr := setRGBA(204,204,204,255);
  vertex3f(-sz, -sz, sz, true);
  vertex3f(sz, -sz, sz);
  vertex3f(-sz, sz, sz);
  vertex3f(sz, sz, sz, true);
  //front
  clr := setRGBA(0,0,128,255);
  vertex3f(-sz, sz, -sz, true);
  vertex3f(-sz, sz, sz);
  vertex3f(sz, sz, -sz);
  vertex3f(sz, sz, sz, true);
  //back
  clr := setRGBA(77,0,77,255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(sz, -sz, -sz);
  vertex3f(-sz, -sz, sz);
  vertex3f(sz, -sz, sz, true);
  //left
  clr := setRGBA(153,0,0,255);
  vertex3f(-sz, -sz, -sz, true);
  vertex3f(-sz, -sz, sz);
  vertex3f(-sz, sz, -sz);
  vertex3f(-sz, sz, sz, true);
  //right
  clr := setRGBA(0,153,0,255);
  vertex3f(sz, -sz, -sz, true);
  vertex3f(sz, sz, -sz);
  vertex3f(sz, -sz, sz);
  vertex3f(sz, sz, sz, true);
end; //MakeCube()

procedure  TGPUCube.CreateCube(sz: single);
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_CLR = 3;   //color RGBA are positions 3,4,5,6
var
  nface: integer;
  vtxClrs: TVtxClrRA;
begin
  if not isRedraw then exit;
  isRedraw := false;
  vtxClrs := nil;
  MakeCube(sz, vtxClrs);
  nface := Length(vtxClrs); //each face has 3 vertices
  if nface < 1 then exit;
  if vao_point2d <> 0 then
     glDeleteVertexArrays(1,@vao_point2d);
  glGenVertexArrays(1, @vao_point2d);
  vbo_point := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, nface*SizeOf(TVtxClr), @vtxClrs[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  glBindVertexArray(vao_point2d);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  //Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  //Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxClr), PChar( sizeof(TVec3)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  setlength(vtxClrs,0);
end;

procedure TGPUCube.SetAzimuth(f: single);
begin
  if (f <> fAzimuth) then isRedraw := true;
  fAzimuth := f;
end;

procedure TGPUCube.SetElevation(f: single);
begin
  if (f <> fElevation) then isRedraw := true;
  fElevation := f;
end;

procedure TGPUCube.SetIsTopLeft(f: boolean);
begin
     if (f <> isTopLeft) then isRedraw := true;
     isTopLeft := f;
end;

procedure  TGPUCube.SetSize(f: single);
begin
     if (f <> sizeFrac) then isRedraw := true;
     sizeFrac := f;
     if sizeFrac < 0.005 then sizeFrac := 0.005;
     if sizeFrac > 0.25 then sizeFrac := 0.25;
end;

procedure  TGPUCube.ScreenSize(Width,Height: integer);
begin
     if (Width = scrnW) and (Height = scrnH) then exit;
     scrnW := Width;
     scrnH := Height;
     isRedraw := true;
end;

constructor  TGPUCube.Create(Ctx: TOpenGLControl);
begin
     scrnH := 0;
     SizeFrac := 0.02;
     isRedraw := true;
     fAzimuth := 30;
     fElevation := -15;
     isTopLeft := false;
     vao_point2d := 0;
     vbo_point := 0;
     Ctx.MakeCurrent();
     shaderProgram :=  initVertFrag(kVert2D, kFrag2D);
     uniform_mtx := glGetUniformLocation(shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
     glFinish;
     Ctx.ReleaseContext;
end;

procedure  TGPUCube.Draw(Width,Height: integer);
var
  sz: single;
  modelViewProjectionMatrix, projectionMatrix, modelMatrix: TMat4;
begin
  ScreenSize(Width,Height);
  sz := min(ScrnW,ScrnH) * SizeFrac;
  if sz < 5 then exit;
  CreateCube(sz);
  modelMatrix := TMat4.Identity;
  projectionMatrix := TMat4.OrthoGL (0, ScrnW,0, ScrnH,-10*sz,10*sz);
  projectionMatrix *= TMat4.Translate(0,0,sz*8);
  projectionMatrix *= TMat4.Translate(1.8*sz,1.8*sz,0);
  projectionMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  projectionMatrix *= TMat4.RotateZ(-DegToRad(fAzimuth));
  modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  glEnable(GL_CULL_FACE);
  glUseProgram(shaderProgram);
  glUniformMatrix4fv(uniform_mtx, 1, GL_FALSE, @modelViewProjectionMatrix);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBindVertexArray(vao_point2d);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 36);
  glBindVertexArray(0);
  glDisable(GL_CULL_FACE);
  glUseProgram(0);
end;

end.

