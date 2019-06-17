//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
out vec3 vN;
out vec4 vClr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
void main() {
    vN = normalize((NormalMatrix * vec4(Norm,1.0)).xyz);
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vClr = Clr;
}
//frag
#version 330
in vec3 vN;
in vec4 vClr;
out vec4 color;
uniform sampler2D MatCap;

void main() {
	vec3 n = normalize(vN);
	vec2 uv = n.xy * 0.5 + 0.5;
	vec3 mClr = texture(MatCap,uv.xy).rgb;
	vec3 clr = mClr * vClr.rgb;

	color = vec4(clr, 1.0);
}