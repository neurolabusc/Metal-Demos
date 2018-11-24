unit glvolume;
{$mode objfpc}
{$H+}
interface

uses
 SimdUtils, glcorearb, gl_core_utils, VectorMath, Classes, SysUtils, Graphics,
    math, OpenGLContext, dialogs, loadNifti;
{$DEFINE STRIP} //we can define cube as either a triangle or triangle strip - no implications on performance
{$DEFINE GPUGRADIENTS} //Computing volume gradients on the GPU is much faster than using the CPU
const
 kDefaultDistance = 2.25;
 kMaxDistance = 40;
type
  TGPUVolume = class
      private
        RayCastQuality1to10, slices,fAzimuth,fElevation: integer;
        fDistance: single;
        fLightPos: TVec4;
        glControl: TOpenGLControl;
        rayDirLoc,intensityVolLoc, gradientVolLoc,mvpLoc, imvLoc, lightPositionLoc,
        sliceSizeLoc, stepSizeLoc, loopsLoc : GLint;
        {$IFDEF GPUGRADIENTS}programSobel, programBlur: GLuint;  {$ENDIF}
        gradientTexture3D, intensityTexture3D, vao, programRaycast, vboBox3D: GLuint;
        procedure LoadCube();
        {$IFDEF GPUGRADIENTS}procedure CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer);{$ENDIF}
        function LoadTexture(var vol: TNIfTI): boolean;
        procedure Prepare();
      public
        property Azimuth: integer read fAzimuth write fAzimuth;
        property Elevation: integer read fElevation write fElevation;
        property Distance: single read fDistance write fDistance;
        property LightPosition: TVec4 read fLightPos write fLightPos;
        constructor Create(fromView: TOpenGLControl);
        procedure Paint(var vol: TNIfTI);
        procedure SetShader(shaderName: string);
  end;

implementation

uses vrForm;

{$IFDEF GPUGRADIENTS}
function bindBlankGL(Xsz,Ysz,Zsz: integer): GLuint;
begin //creates an empty texture in VRAM without requiring memory copy from RAM
    //later run glDeleteTextures(1,&oldHandle);
    glGenTextures(1, @result);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glBindTexture(GL_TEXTURE_3D, result);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE); //, GL_CLAMP_TO_BORDER) will wrap
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
    glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA16, XSz, YSz, ZSz, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
end;

procedure glUniform1ix(prog: GLuint; name: AnsiString; value: integer);
begin
    glUniform1i(glGetUniformLocation(prog, pAnsiChar(Name)), value) ;
end;

procedure glUniform1fx(prog: GLuint; name: AnsiString; value: single );
begin
    glUniform1f(glGetUniformLocation(prog, pAnsiChar(Name)), value) ;
end;

const kBlurSobelVert = '#version 330 core'
+#10'layout(location = 0) in vec3 vPos;'
+#10'out vec2 TexCoord;'
+#10'void main() {'
+#10'    TexCoord = vPos.xy;'
+#10'    gl_Position = vec4( (vPos.xy-vec2(0.5,0.5))* 2.0, 0.0, 1.0);'
+#10'//    gl_Position = vec4( (vPos-vec3(0.5,0.5,0.5))* 2.0, 1.0);'
+#10'}';

const kBlurFrag = '#version 330 core'
+#10'in vec2 TexCoord;'
+#10'out vec4 FragColor;'
+#10'uniform float coordZ, dX, dY, dZ;'
+#10'uniform sampler3D intensityVol;'
+#10'void main(void) {'
+#10' vec3 vx = vec3(TexCoord.xy, coordZ);'
+#10' vec4 samp = texture(intensityVol,vx+vec3(+dX,+dY,+dZ));'
+#10' samp += texture(intensityVol,vx+vec3(+dX,+dY,-dZ));'
+#10' samp += texture(intensityVol,vx+vec3(+dX,-dY,+dZ));'
+#10' samp += texture(intensityVol,vx+vec3(+dX,-dY,-dZ));'
+#10' samp += texture(intensityVol,vx+vec3(-dX,+dY,+dZ));'
+#10' samp += texture(intensityVol,vx+vec3(-dX,+dY,-dZ));'
+#10' samp += texture(intensityVol,vx+vec3(-dX,-dY,+dZ));'
+#10' samp += texture(intensityVol,vx+vec3(-dX,-dY,-dZ));'
+#10' FragColor = samp*0.125;'
+#10'}';

const kSobelFrag = '#version 330 core'
+#10'in vec2 TexCoord;'
+#10'out vec4 FragColor;'
+#10'uniform float coordZ, dX, dY, dZ;'
+#10'uniform sampler3D intensityVol;'
+#10'void main(void) {'
+#10'  vec3 vx = vec3(TexCoord.xy, coordZ);'
+#10'  float TAR = texture(intensityVol,vx+vec3(+dX,+dY,+dZ)).a;'
+#10'  float TAL = texture(intensityVol,vx+vec3(+dX,+dY,-dZ)).a;'
+#10'  float TPR = texture(intensityVol,vx+vec3(+dX,-dY,+dZ)).a;'
+#10'  float TPL = texture(intensityVol,vx+vec3(+dX,-dY,-dZ)).a;'
+#10'  float BAR = texture(intensityVol,vx+vec3(-dX,+dY,+dZ)).a;'
+#10'  float BAL = texture(intensityVol,vx+vec3(-dX,+dY,-dZ)).a;'
+#10'  float BPR = texture(intensityVol,vx+vec3(-dX,-dY,+dZ)).a;'
+#10'  float BPL = texture(intensityVol,vx+vec3(-dX,-dY,-dZ)).a;'
+#10'  vec4 gradientSample = vec4 (0.0, 0.0, 0.0, 0.0);'
+#10'  gradientSample.r =   BAR+BAL+BPR+BPL -TAR-TAL-TPR-TPL;'
+#10'  gradientSample.g =  TPR+TPL+BPR+BPL -TAR-TAL-BAR-BAL;'
+#10'  gradientSample.b =  TAL+TPL+BAL+BPL -TAR-TPR-BAR-BPR;'
+#10'  gradientSample.a = (abs(gradientSample.r)+abs(gradientSample.g)+abs(gradientSample.b))*0.29;'
+#10'  gradientSample.rgb = normalize(gradientSample.rgb);'
+#10'  gradientSample.rgb =  (gradientSample.rgb * 0.5)+0.5;'
+#10'  FragColor = gradientSample;'
+#10'}';

procedure TGPUVolume.CreateGradientVolumeGPU(Xsz,Ysz,Zsz: integer);
//procedure TGPUVolume.performBlurSobel(rData: tVolB; Xsz,Ysz,Zsz: integer; lIsOverlay: boolean);
//http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
//http://www.opengl.org/wiki/Framebuffer_Object_Examples
var
   i: integer;
   coordZ: single;
   fb, tempTex3D: GLuint;
begin
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  glGenFramebuffers(1, @fb);
  glBindFramebuffer(GL_FRAMEBUFFER, fb);
  glDisable(GL_CULL_FACE);
  //{$IFNDEF COREGL}glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);{$ENDIF}// <- REQUIRED
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  glViewport(0, 0, XSz, YSz);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  //STEP 1: run smooth program gradientTexture -> tempTex3D
  tempTex3D := bindBlankGL(Xsz,Ysz,Zsz);
  glUseProgram(programBlur);
  glActiveTexture( GL_TEXTURE1);
  //glBindTexture(GL_TEXTURE_3D, gRayCast.gradientTexture3D);//input texture
  glBindTexture(GL_TEXTURE_3D, intensityTexture3D);//input texture is overlay
  glUniform1ix(programBlur, 'intensityVol', 1);
  glUniform1fx(programBlur, 'dX', 0.7/XSz); //0.5 for smooth - center contributes
  glUniform1fx(programBlur, 'dY', 0.7/YSz);
  glUniform1fx(programBlur, 'dZ', 0.7/ZSz);
  glBindVertexArray(vao);
  for i := 0 to (ZSz-1) do begin
      coordZ := 1/ZSz * (i + 0.5);
      glUniform1fx(programBlur, 'coordZ', coordZ);
      //glFramebufferTexture3D(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, tempTex3D, 0, i);//output texture
      //Ext required: Delphi compile on Winodws 32-bit XP with NVidia 8400M
      glFramebufferTexture3D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, tempTex3D, 0, i);//output texture
      glClear(GL_DEPTH_BUFFER_BIT);  // clear depth bit (before render every layer)
      {$IFDEF STRIP}
      glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nil);
      {$ELSE}
      glDrawElements(GL_TRIANGLES, 2*3, GL_UNSIGNED_INT, nil);
      {$ENDIF}
  end;
  glUseProgram(0);
  //STEP 2: run sobel program gradientTexture -> tempTex3D
  //glUseProgramObjectARB(gRayCast.glslprogramSobel);
  glUseProgram(programSobel);
  glActiveTexture(GL_TEXTURE1);
  //x glBindTexture(GL_TEXTURE_3D, gRayCast.intensityTexture3D);//input texture
  glBindTexture(GL_TEXTURE_3D, tempTex3D);//input texture
    glUniform1ix(programSobel, 'intensityVol', 1);
    glUniform1fx(programSobel, 'dX', 1.2/XSz ); //1.0 for SOBEL - center excluded
    glUniform1fx(programSobel, 'dY', 1.2/YSz);
    glUniform1fx(programSobel, 'dZ', 1.2/ZSz);
    glBindVertexArray(vao);
    for i := 0 to (ZSz-1) do begin
        coordZ := 1/ZSz * (i + 0.5);
        glUniform1fx(programSobel, 'coordZ', coordZ);
        glFramebufferTexture3D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_3D, gradientTexture3D, 0, i);//output is background
        glClear(GL_DEPTH_BUFFER_BIT);
        {$IFDEF STRIP}
        glDrawElements(GL_TRIANGLE_STRIP, 4, GL_UNSIGNED_INT, nil);
        {$ELSE}
        glDrawElements(GL_TRIANGLES, 2*3, GL_UNSIGNED_INT, nil);
        {$ENDIF}
    end;
    glUseProgram(0);
     //clean up:
     glDeleteTextures(1,@tempTex3D);
     glBindFramebuffer(GL_FRAMEBUFFER, 0);
     glDeleteFramebuffers(1, @fb);
     glActiveTexture( GL_TEXTURE0 );  //required if we will draw 2d slices next
end;
{$ENDIF}

const kVert = '#version 330 core'
+#10'layout(location = 0) in vec3 vPos;'
+#10'out vec3 TexCoord1;'
+#10'out vec4 vPosition;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void main() {'
+#10'  TexCoord1 = vPos;'
+#10'  gl_Position = ModelViewProjectionMatrix * vec4(vPos, 1.0);'
+#10'  vPosition = gl_Position;'
+#10'}';

kFrag ='#version 330 core'
+#10'in vec3 TexCoord1;'
+#10'out vec4 FragColor;'
+#10'in vec4 vPosition;'
+#10'uniform int loops;'
+#10'uniform float stepSize, sliceSize;'
+#10'uniform sampler3D intensityVol, gradientVol;'
+#10'uniform vec3 lightPosition, rayDir;'
+#10'uniform float ambient = 1.0;'
+#10'uniform float diffuse = 0.3;'
+#10'uniform float specular = 0.25;'
+#10'uniform float shininess = 10.0;'
+#10'vec3 GetBackPosition (vec3 startPosition) { //when does ray exit unit cube http://prideout.net/blog/?p=64'
+#10'	vec3 invR = 1.0 / rayDir;'
+#10'    vec3 tbot = invR * (vec3(0.0)-startPosition);'
+#10'    vec3 ttop = invR * (vec3(1.0)-startPosition);'
+#10'    vec3 tmax = max(ttop, tbot);'
+#10'    vec2 t = min(tmax.xx, tmax.yz);'
+#10'	return startPosition + (rayDir * min(t.x, t.y));'
+#10'}'
+#10'void main() {'
+#10'	//FragColor = vec4(0.0, 1.0, 0.0, 1.0); return;'
+#10'	vec3 start = TexCoord1.xyz;'
+#10'	vec3 backPosition = GetBackPosition(start);'
+#10'	//FragColor = vec4(start, 1.0); return;'
+#10'	//FragColor = vec4(backPosition, 1.0); return;'
+#10'	vec3 dir = backPosition - start;'
+#10'	//FragColor = vec4(dir, 1.0); return;'
+#10'	float len = length(dir);'
+#10'	dir = normalize(dir);'
+#10'	vec3 deltaDir = dir * stepSize;'
+#10'	vec4 colorSample,gradientSample,colAcc = vec4(0.0,0.0,0.0,0.0);'
+#10'	float lengthAcc = 0.0;'
+#10'	vec3 samplePos = start.xyz + deltaDir* (fract(sin(gl_FragCoord.x * 12.9898 + gl_FragCoord.y * 78.233) * 43758.5453));'
+#10'	vec4 prevNorm = vec4(0.0,0.0,0.0,0.0);'
+#10'	for(int i = 0; i < loops; i++) {'
+#10'		//float tex = texture(intensityVol,samplePos).r;'
+#10'		//colorSample.rgba = vec4(tex,tex,tex,tex);'
+#10'		colorSample.rgba = texture(intensityVol,samplePos);'
+#10'		colorSample.a = 1.0-pow((1.0 - colorSample.a), stepSize/sliceSize);'
+#10'		if (colorSample.a > 0.01) {'
+#10'			gradientSample= texture(gradientVol,samplePos);'
+#10'			gradientSample.rgb = normalize(gradientSample.rgb*2.0 - 1.0);'
+#10'			if (gradientSample.a < prevNorm.a)'
+#10'				gradientSample.rgb = prevNorm.rgb;'
+#10'			prevNorm = gradientSample;'
+#10'			float lightNormDot = dot(gradientSample.rgb, lightPosition);'
+#10'			vec3 a = colorSample.rgb * ambient;'
+#10'			vec3 d = max(lightNormDot, 0.0) * colorSample.rgb * diffuse;'
+#10'			float s =   specular * pow(max(dot(reflect(lightPosition, gradientSample.rgb), dir), 0.0), shininess);'
+#10'			colorSample.rgb = a + d + s;'
+#10'		}'
+#10'		colorSample.rgb *= colorSample.a;'
+#10'		colAcc= (1.0 - colAcc.a) * colorSample + colAcc;'
+#10'		samplePos += deltaDir;'
+#10'		lengthAcc += stepSize;'
+#10'		if ( lengthAcc >= len || colAcc.a > 0.95 )'
+#10'			break;'
+#10'	}'
+#10'	colAcc.a = colAcc.a/0.95;'
+#10'	FragColor = colAcc;'
+#10'}';

procedure TGPUVolume.SetShader(shaderName: string);
var
  VertexProgram, FragmentProgram: string;
begin
  glControl.MakeCurrent();
  glUseProgram(0);
  if (programRaycast <> 0) then glDeleteProgram(programRaycast);
  loadVertFrag(shaderName, VertexProgram, FragmentProgram);
  if VertexProgram = '' then VertexProgram := kVert;
  if FragmentProgram = '' then FragmentProgram := kFrag;
  programRaycast :=  initVertFrag(VertexProgram, FragmentProgram);
  imvLoc := glGetUniformLocation(programRaycast, pAnsiChar('ModelViewMatrixInverse'));
  mvpLoc := glGetUniformLocation(programRaycast, pAnsiChar('ModelViewProjectionMatrix'));
  rayDirLoc := glGetUniformLocation(programRaycast, pAnsiChar('rayDir'));
  sliceSizeLoc := glGetUniformLocation(programRaycast, pAnsiChar('sliceSize'));
  stepSizeLoc := glGetUniformLocation(programRaycast, pAnsiChar('stepSize'));
  loopsLoc := glGetUniformLocation(programRaycast, pAnsiChar('loops'));
  lightPositionLoc := glGetUniformLocation(programRaycast, pAnsiChar('lightPosition'));
  intensityVolLoc := glGetUniformLocation(programRaycast, pAnsiChar('intensityVol'));
  gradientVolLoc := glGetUniformLocation(programRaycast, pAnsiChar('gradientVol'));
  if GLErrorStr <>  '' then begin
   glControl.ReleaseContext;
   showmessage(GLErrorStr);
   GLErrorStr := '';
  end;
end;

procedure TGPUVolume.Prepare();
begin
  glControl.MakeCurrent();
  SetShader('');
  {$IFDEF GPUGRADIENTS}
  programBlur := initVertFrag(kBlurSobelVert,kBlurFrag);
  programSobel := initVertFrag(kBlurSobelVert,kSobelFrag);
  {$ELSE}
  programBlur := 1;
  {$ENDIF}
  LoadCube();
  //glControl.ReleaseContext;
end;

constructor TGPUVolume.Create(fromView: TOpenGLControl);
begin
  glControl := fromView;
  fDistance := kDefaultDistance;
  fAzimuth := 70;
  fElevation := 30;
  RaycastQuality1to10 := 6;
  fLightPos := Vec4(0,0.707,0.707, 0.0);
  vao:= 0;
  programBlur := 0;
end;

procedure TGPUVolume.LoadCube();
var
  vtx : packed array[0..23] of GLfloat = (
      0,0,0,
      0,1,0,
      1,1,0,
      1,0,0,
      0,0,1,
      0,1,1,
      1,1,1,
      1,0,1
      ); //vtx = 8 vertex positions (corners) of cube
  {$IFDEF STRIP}
  //https://stackoverflow.com/questions/28375338/cube-using-single-gl-triangle-strip
  idx : packed array[0..13] of GLuint = (0,1,3,2,6,1,5,4, 6,7,3, 4, 0, 1); //reversed winding
  //idx : packed array[0..13] of GLuint = (1,0,4,3,7,6,4,5,1,6,2,3,1,0);
  {$ELSE}
  idx : packed array[0..35] of GLuint = (
      0,2,1,
      0,3,2,
      4,5,6,
      4,6,7,
      0,1,5,
      0,5,4,
      3,6,2,
      3,7,6,
      1,6,5,
      1,2,6,
      0,4,7,
      0,7,3
      ); //idx = each cube has 6 faces, each composed of two triangles = 12 tri indices
{$ENDIF}
    vbo_point: gluint;
begin  //vboCube, vaoCube,
  vbo_point := 0;
  vao := 0;
  vboBox3D := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, 8*3*sizeof(GLfloat), @vtx[0], GL_STATIC_DRAW); //cube has 8 vertices, each 3 coordinates X,Y,Z
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenVertexArrays(1, @vao);
  // vao like a closure binding 3 buffer object: verlocdat vercoldat and veridxdat
  glBindVertexArray(vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nil);
  glEnableVertexAttribArray(0); // for vertexloc
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nil);
  glEnableVertexAttribArray(1); // for vertexcol
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenBuffers(1, @vboBox3D);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  {$IFDEF STRIP}
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, 14*sizeof(GLuint), @idx[0], GL_STATIC_DRAW); //cube is 6 faces, 2 triangles per face, 3 indices per triangle
  {$ELSE}
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, 36*sizeof(GLuint), @idx[0], GL_STATIC_DRAW); //cube is 6 faces, 2 triangles per face, 3 indices per triangle
  {$ENDIF}
  //do not delete the VBOs! http://stackoverflow.com/questions/25167562/how-to-dispose-vbos-stored-in-a-vao
end;

function TGPUVolume.LoadTexture(var vol: TNIfTI): boolean;
var
 i: GLint;
 gradData: TRGBAs;
begin
 result := true;
 if (Vol.VolRGBA = nil) then exit;
 if (intensityTexture3D <> 0) then glDeleteTextures(1,@intensityTexture3D);
 if (gradientTexture3D <> 0) then glDeleteTextures(1,@gradientTexture3D);
 //next: see if our video card can show this texture
 glTexImage3D(GL_PROXY_TEXTURE_3D, 0, GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL);
 glGetTexLevelParameteriv(GL_PROXY_TEXTURE_3D, 0, GL_TEXTURE_WIDTH, @i);
 (*if i = 0 then begin //video card can not support this texture - report an error but not in OpenGL context
    LoadBorg(64,rawdata, X,Y,Z, isRGBA, ScaleDim);
    result := false; //we failed to load the requested image
 end;*)
 //next copy the image to the GPU
 glPixelStorei(GL_UNPACK_ALIGNMENT,1);
 glGenTextures(1, @intensityTexture3D);
 glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
 glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Vol.VolRGBA[0]);
 glPixelStorei(GL_UNPACK_ALIGNMENT,1);
 glGenTextures(1, @gradientTexture3D);
 glBindTexture(GL_TEXTURE_3D, gradientTexture3D);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
 glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);
 //startTime := Now;
 {$IFDEF GPUGRADIENTS}
 SetLength (gradData, Vol.Dim.X*Vol.Dim.Y*Vol.Dim.Z);
 glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
 gradData := nil;
 CreateGradientVolumeGPU (Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z);
 //Form1.Caption := 'GLSL gradients '+inttostr(MilliSecondsBetween(Now,startTime))+' ms ';
 {$ELSE}
 gradData := Vol.GenerateGradientVolume;
 //CreateGradientVolume (Vol.VolRGBA, Vol.Dim.X, Vol.Dim.Y, Vol.Dim.Z, gradData);
 glTexImage3D(GL_TEXTURE_3D, 0,GL_RGBA, Vol.Dim.X, Vol.Dim.Y,Vol.Dim.Z, 0, GL_RGBA, GL_UNSIGNED_BYTE,@gradData[0]);
 gradData := nil;
 //Form1.Caption := 'CPU gradients '+inttostr(MilliSecondsBetween(Now,startTime))+' ms ';
 {$ENDIF}
 slices := max(Vol.Dim.X,max(Vol.Dim.Y,Vol.Dim.Z));
 Vol.GPULoadDone;
end;

procedure addFuzz (var v: TVec4); //avoid shader divide by zero error
const
     kEPS = 0.0001;
begin
   if (abs(v.x) < kEPS) then v.x := kEPS;
   if (abs(v.y) < kEPS) then v.y := kEPS;
   if (abs(v.z) < kEPS) then v.z := kEPS;
   if (abs(v.w) < kEPS) then v.w := kEPS;
end;

function ComputeStepSize (Quality1to10, Slices: integer): single;
var
  f: single;
begin
  f := Quality1to10;
  if (f <= 1) or (f > 10) then
    f := 5;
  f := (slices*0.25)+ (slices*1.75)* (f/10);
  result := 1/f;
end;

procedure TGPUVolume.Paint(var vol: TNIfTI);
var
  modelViewProjectionMatrixInverse, modelViewProjectionMatrix, projectionMatrix, modelMatrix: TMat4;
  modelLightPos, v, rayDir: TVec4;
  whratio, scale: single;
begin
  if programBlur = 0 then
    Prepare();
  if vao = 0 then // only once
    LoadCube();
  if (vol.VolRGBA <> nil) then
     LoadTexture(vol);
  if (intensityTexture3D = 0) then
    exit;
  glUseProgram(programRaycast);
  glBindFramebuffer(GL_FRAMEBUFFER, 0); //draw to screen
  glActiveTexture(GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_3D, intensityTexture3D);
  glUniform1i(intensityVolLoc, 2);
  glActiveTexture(GL_TEXTURE3);
  glBindTexture(GL_TEXTURE_3D, gradientTexture3D);
  glUniform1i(gradientVolLoc, 3);
  glUniform1f(stepSizeLoc, ComputeStepSize(RayCastQuality1to10, slices)) ;
  glUniform1f(sliceSizeLoc, 1/slices);
  glUniform1i(loopsLoc,round(slices*2.2));
  //glUniform3f(clearColorLoc, fClearColor.r/255, fClearColor.g/255, fClearColor.b/255);
  modelMatrix := TMat4.Identity;
  modelMatrix *= TMat4.Translate(0, 0, -fDistance);
  modelMatrix *= TMat4.RotateX(-DegToRad(90-fElevation));
  modelMatrix *= TMat4.RotateZ(DegToRad(fAzimuth));
  modelMatrix *= TMat4.Translate(-vol.Scale.X/2, -vol.Scale.Y/2, -vol.Scale.Z/2);
  modelLightPos := (modelMatrix.Transpose * fLightPos);
  modelMatrix *= TMat4.Scale(vol.Scale.X, vol.Scale.Y, vol.Scale.Z); //for volumes that are rectangular not square
  if (fDistance = -1.0) or (glControl.clientheight = 0)  then exit;//avoid divide by zero
  scale := 0.5 * 1/abs(kDefaultDistance/(fDistance+1.0));
  //glUniform3f(lightPositionLoc,fLightPos.x,fLightPos.y,fLightPos.z);
  glUniform3f(lightPositionLoc,modelLightPos.x, modelLightPos.y, modelLightPos.z);
  whratio := glControl.clientwidth/glControl.clientheight;
  if (whratio > 1) then //Wide window
     projectionMatrix := TMat4.OrthoGL (-scale * whratio, scale * whratio, -scale, scale, 0.01, 5.0)
  else
      projectionMatrix := TMat4.OrthoGL (-scale, scale, -scale/whratio, scale/whratio, 0.01, 5.0);
  modelViewProjectionMatrix := ( projectionMatrix * modelMatrix);
  rayDir.x := 0; RayDir.y := 0; rayDir.z := 1; RayDir.w := 0;
  v := rayDir;
  rayDir := (modelViewProjectionMatrix.Inverse * v);
  rayDir.w := 0;
  rayDir := rayDir.Normalize;
  addFuzz(rayDir);
  modelViewProjectionMatrixInverse := modelViewProjectionMatrix.Inverse;
  glUniformMatrix4fv(mvpLoc, 1, GL_FALSE, @modelViewProjectionMatrix);
  glUniformMatrix4fv(imvLoc, 1, GL_FALSE, @modelViewProjectionMatrixInverse);
  glUniform3f(rayDirLoc,rayDir.x,rayDir.y,rayDir.z);
  glViewport(0, 0, glControl.ClientWidth, glControl.ClientHeight); //required for form resize
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_CULL_FACE);
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboBox3D);
  {$IFDEF STRIP}
  glCullFace(GL_BACK);
  glDrawElements(GL_TRIANGLE_STRIP, 14, GL_UNSIGNED_INT, nil);
  {$ELSE}
  glCullFace(GL_FRONT);
  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, nil);
  {$ENDIF}
  glDisable(GL_CULL_FACE);
  glControl.SwapBuffers;
end;

end.

