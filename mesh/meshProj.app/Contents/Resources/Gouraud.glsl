//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
out vec4 vClr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
uniform vec3 lightPosition = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+
uniform float Ambient = 0.4;
uniform float Diffuse = 0.7;
uniform float Specular = 0.6;
uniform float Roughness = 0.1;
void main() {
	gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vec3 n = normalize((NormalMatrix * vec4(Norm,1.0)).xyz);
    vec3 l = normalize(lightPosition);
    vec3 v = -vec3(ModelViewMatrix*vec4(Vert,1.0));
 	vec3 h = normalize(l+v);
 	float diffuse = dot(l,n);
 	vec3 AmbientColour = Clr.rgb;
 	vec3 DiffuseColour = Clr.rgb;
 	vec3 SpecularColour = vec3(1.0, 1.0, 1.0);
 	float specular =  pow(max(0.0,dot(n,h)),1.0/(Roughness * Roughness));
 	vClr = vec4(AmbientColour*Ambient + DiffuseColour*diffuse*Diffuse +SpecularColour*specular* Specular, 1.0);
}
//frag
#version 330
in vec4 vClr;
out vec4 color;
void main() {
 color = vClr;
}