unit glunit;

{$mode objfpc}{$H+}
{$include ../common/glopts.inc}
interface

uses
   {$IFDEF COREGL}
     glcorearb,
   {$ELSE}
   gl, glext,
   {$ENDIF}
  gl_core_utils,Dialogs, Classes, SysUtils,OpenGLContext;

type
  TViewGPU = class(TOpenGLControl)
    procedure  InitGL(var GLcontrol: TViewGPU);
    procedure MyPaint(Sender: TObject);
  end;

var
  gOffsetXY : TPoint =  (x:0; y:0);
  gSize : single = 100;

implementation

type
TShader = record
  {$IFDEF COREGL}
  vbo_point, vao_point,
  {$ELSE}
  displayLst,
  {$ENDIF}
  shaderProgram, vertexArrayObject: GLuint;
  uniform_viewportSize: GLint;
end;

var
  gShader: TShader;

{$IFDEF COREGL}
const
 //Simple Vertex Shader
    kVert = '#version 330'
+#10'layout(location = 0) in vec2 position;'
+#10'layout(location = 1) in vec3 color;'
+#10'out vec3 fClr;'
+#10'uniform vec2 viewportSize;'
+#10'void main() {'
+#10'    gl_Position = vec4((position / (viewportSize/2)), 0.0, 1.0);'
+#10'    fClr = color;'
+#10'}';

//Simple Fragment Shader
kFrag = '#version 330'
+#10'in vec3 fClr;'
+#10'out vec4 color;'
+#10'void main() {'
+#10'    color = vec4(fClr, 1);'
+#10'}';
{$ELSE}
const
 //Simple Vertex Shader
    kVert = '#version 120'
+#10'varying vec4 fClr;'
+#10'uniform vec2 viewportSize;'
+#10'void main() {'
+#10'    gl_Position = vec4((gl_Vertex.xy / (viewportSize/2)), 0.0, 1.0);'
+#10'    fClr = gl_Color;'
+#10'}';

//Simple Fragment Shader
kFrag = '#version 120'
+#10'varying vec4 fClr;'
+#10'void main() {'
+#10'    gl_FragColor = fClr;'
+#10'}';

{$ENDIF}

type
  TAAPLVertex = packed record
    position: array [0..1] of single;
    color: array [0..2] of single;
  end;

function AAPLVertex(x,y, r,g,b: single): TAAPLVertex;
begin
    result.position[0] := x;
    result.position[1] := y;
    result.color[0] := r;
    result.color[1] := g;
    result.color[2] := b;
end;

procedure LoadBufferData (isInit: boolean);
const
    kATTRIB_POINT = 0;
    kATTRIB_COLOR = 1;
var
   verts: array[0..2] of TAAPLVertex;
   {$IFNDEF COREGL}
   i: integer;
   {$ENDIF}
begin
  verts[0] := AAPLVertex(gOffsetXY.x + gSize, gOffsetXY.y -gSize, 1, 0, 0);
  verts[1] := AAPLVertex(gOffsetXY.x - gSize, gOffsetXY.y -gSize, 0, 1, 0);
  verts[2] := AAPLVertex(gOffsetXY.x + 0,     gOffsetXY.y + gSize, 0, 0, 1);
  {$IFDEF COREGL}
  if (not isInit) then begin
     glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point);
     glBufferSubData(GL_ARRAY_BUFFER,0,sizeof(verts), @verts[0]);
     glBindBuffer(GL_ARRAY_BUFFER, 0);
     exit;
  end;
  glGenBuffers(1, @gShader.vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point);
  glBufferData(GL_ARRAY_BUFFER, sizeof(verts), @verts[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  //   glGenVertexArrays intoduced with OpenGL 3.0
  glGenVertexArrays(1, @gShader.vao_point);
  glBindVertexArray(gShader.vao_point);
  glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point);
  glVertexAttribPointer(kATTRIB_POINT, 2, GL_FLOAT, GL_FALSE, 5*4, PChar(0));
  glEnableVertexAttribArray(kATTRIB_POINT);
  glVertexAttribPointer(kATTRIB_COLOR, 3, GL_FLOAT, GL_FALSE, 5*4, PChar(sizeof(single)*2));
  glEnableVertexAttribArray(kATTRIB_COLOR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  gShader.uniform_viewportSize := glGetUniformLocation(gShader.shaderProgram, pAnsiChar('viewportSize'));
  {$ELSE}
  if not isInit then
     glDeleteLists(gShader.displayLst, 1);
  gShader.displayLst := glGenLists(1);
  glNewList(gShader.displayLst, GL_COMPILE);
  //glBegin(GL_TRIANGLES);
  glBegin(GL_TRIANGLE_STRIP);
  for i := 0 to (2) do begin
      glColor3f(verts[i].color[0], verts[i].color[1], verts[i].color[2]);
      glVertex3f(verts[i].position[0], verts[i].position[1], 0);
  end;
  glEnd();
  glEndList();
  {$ENDIF}
end;

procedure  TViewGPU.InitGL(var GLcontrol: TViewGPU);
begin
  GLcontrol.MakeCurrent();
  {$IFDEF COREGL}
  if not Load_GL_version_3_3_CORE() then begin
  {$ELSE}
  if not Load_GL_version_2_1() then begin
  {$ENDIF}
     GLcontrol.ReleaseContext;
     {$IFNDEF Windows} writeln('Unable to load OpenGL');{$ENDIF}
     showmessage('Unable to load OpenGL 3.3');
     halt();
  end;
  gShader.shaderProgram :=  initVertFrag(kVert,  kFrag);
  LoadBufferData(true);
  GLcontrol.ReleaseContext;
  if GLErrorStr <> '' then begin
     showmessage(GLErrorStr);
     GLErrorStr := '';
  end;
end;

procedure TViewGPU.MyPaint(Sender: TObject);
begin
  glViewPort(0,0, self.Width, self.Height);
  LoadBufferData(false);
  glClearColor(0.0, 0.0, 0.7, 1.0); //Set blue background
  glClear(GL_COLOR_BUFFER_BIT);
  glDisable(GL_DEPTH_TEST);
  glUseProgram(gShader.shaderProgram);
  glUniform2f(gShader.uniform_viewportSize, self.Width, self.Height);
  {$IFDEF COREGL}
  glBindVertexArray(gShader.vao_point);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 3);
  glBindVertexArray(0);
  {$ELSE}
  glCallList(gShader.displayLst);
  {$ENDIF}
  glUseProgram(0);
  Self.SwapBuffers;
end;

end.
