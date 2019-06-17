//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
flat out vec3 vN, vL, vV;
out vec4 vClr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
uniform vec3 lightPosition = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+
void main() {
    //vN = normalize((NormalMatrix * Norm));
    vN = normalize((NormalMatrix * vec4(Norm,1.0)).xyz);
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vL = normalize(lightPosition);
    vV = -vec3(ModelViewMatrix*vec4(Vert,1.0));
    vClr = Clr;
}
//frag
#version 330
in vec4 vClr;
flat in vec3 vN, vL, vV;
out vec4 color;
uniform float Ambient = 0.4;
uniform float Diffuse = 0.7;
uniform float Specular = 0.6;
uniform float Roughness = 0.1;
void main() {
 vec3 n = normalize(vN);
 //color = vec4(abs(n * 2.0) - 1.0,1.0); return;
 vec3 v = normalize(vV);
 vec3 h = normalize(vL+v);
 float diffuse = dot(vL,n);
 vec3 AmbientColour = vClr.rgb;
 vec3 DiffuseColour = vClr.rgb;
 vec3 SpecularColour = vec3(1.0, 1.0, 1.0);
 float specular =  pow(max(0.0,dot(n,h)),1.0/(Roughness * Roughness));
 color = vec4(AmbientColour*Ambient + DiffuseColour*diffuse*Diffuse +SpecularColour*specular* Specular, 1.0);
}