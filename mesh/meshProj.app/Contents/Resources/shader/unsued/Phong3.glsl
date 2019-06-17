//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
//uniform mat3 NormalMatrix;
uniform mat4 NormalMatrix;
//uniform mat3 NormalMatrix3;
uniform vec3 LightPosition = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+
out vec3 vN, vL, vV;
out vec4 vClr;
void main() {
    //vN = normalize((NormalMatrix3 * Norm));
    vN = normalize((NormalMatrix * vec4(Norm,1.0)).xyz);
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vL = normalize(LightPosition);
    vV = -vec3(ModelViewMatrix*vec4(Vert,1.0));
    vClr = Clr;

}

//frag
#version 330
in vec4 vClr;
in vec3 vN, vL, vV;
out vec4 color;
uniform bool LightBackfaces = false;
uniform float Ambient = 0.35;
uniform float Diffuse = 0.9;
uniform float Specular = 0.4;
uniform float DiffuseRough = 1;
uniform float SpecularRough = 0.1;
uniform float Sharpness =0.0;
uniform float Edge = 0;



vec3 desaturate(vec3 color, float amount) {
    vec3 gray = vec3(dot(vec3(0.2126,0.7152,0.0722), color));
    return vec3(mix(color, gray, amount));
}

void main() {
	vec3 l = normalize(vL);
	vec3 n = normalize(vN);
	vec3 v = normalize(vV);
	vec3 h = normalize(l+v);
	vec3 a = vClr.rgb;
	vec3 d = a * Diffuse;
	a *= Ambient;
	vec3 backcolor = desaturate(0.75 * a + 0.75 * d *  abs(dot(n,l)), 0.5);
	float backface = 1.0 - step(0.0, n.z); //1=backface
	n = mix(n, -n, backface * float(LightBackfaces)); //reverse normal if backface AND two-sided lighting
	d *= max(pow(max(dot( l, n), 0.0), DiffuseRough), 0.0);
	float s = pow(max(0.0,dot(n,h)), 1.0/(SpecularRough * SpecularRough));
	float w = 0.72*(1.0-Sharpness);
	s = smoothstep(0.72-w,0.72+w,s) * Specular;
	vec3 frontcolor = a + d +  s;
	frontcolor *= min((max(dot(n,normalize(v)), 0.0) - 0.5) * Edge, 0.0) + 1.0;
	backface = 1.0 - step(0.0, n.z); //1=backface
	//backcolor = vec3(1.0,0.0,0.0);
	color = vec4(mix(frontcolor,backcolor,   backface), 1.0);
}
