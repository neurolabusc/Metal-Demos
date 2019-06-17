//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
out vec4 vClr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
void main() {
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    float d = 1.0 - (2.0* (gl_Position.z+1.0));
	vClr = vec4(d, d, d, 1.0);
}
//frag
#version 330
in vec4 vClr;
out vec4 color;
void main() {
 color = vClr;
}