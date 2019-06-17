//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
out vec3 vN;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
void main() {
    vN = normalize((NormalMatrix * vec4(Norm,1.0)).xyz);
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
}
//frag
#version 330
in vec3 vN;
out vec4 color;
uniform sampler2D MatCap;

void main() {
	vec3 n = normalize(vN);
	vec2 uv = n.xy * 0.5 + 0.5;
	color = vec4(texture(MatCap,uv.xy).rgb, 1.0);
}