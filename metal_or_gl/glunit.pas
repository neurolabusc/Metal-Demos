unit glunit;

{$mode objfpc}{$H+}
interface

uses
  gl_core_utils,Dialogs, Classes, SysUtils, glcorearb, OpenGLContext;

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
  vbo_point, vao_point, shaderProgram, vertexArrayObject: GLuint;
  uniform_viewportSize: GLint;
end;

var
  gShader: TShader;

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
begin
  verts[0] := AAPLVertex(gOffsetXY.x + gSize, gOffsetXY.y -gSize, 1, 0, 0);
  verts[1] := AAPLVertex(gOffsetXY.x - gSize, gOffsetXY.y -gSize, 0, 1, 0);
  verts[2] := AAPLVertex(gOffsetXY.x + 0,     gOffsetXY.y + gSize, 0, 0, 1);
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
end;

procedure  TViewGPU.InitGL(var GLcontrol: TViewGPU);
begin
  GLcontrol.MakeCurrent();
  if not Load_GL_version_3_3_CORE() then begin
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
  glUseProgram(gShader.shaderProgram);
  glUniform2f(gShader.uniform_viewportSize, self.Width, self.Height);
  glBindVertexArray(gShader.vao_point);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 3);
  glBindVertexArray(0);
  glUseProgram(0);
  Self.SwapBuffers;
end;

end.
