//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
out vec3 vN, vV;
out vec4 vClr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
void main() {
    //vN = normalize((NormalMatrix * Norm));
    vN = normalize((NormalMatrix * vec4(Norm,1.0)).xyz);
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vV = -vec3(ModelViewMatrix*vec4(Vert,1.0));
    vClr = Clr;
}
//frag
#version 330
in vec4 vClr;
in vec3 vN, vV;
out vec4 color;
uniform float Ambient = 0.0;
uniform float Diffuse = 1.0;
uniform float Specular = 0.6;
uniform float SpecularRough = 0.1;

vec3 SH(vec3 vNormal) {
//Spherical harmonics constants
const float C1 = 0.429043;
const float C2 = 0.511664;
const float C3 = 0.743125;
const float C4 = 0.886227;
const float C5 = 0.247708;
// Ramamoorthi, R., and P. Hanrahan. 2001b. "An Efficient Representation for Irradiance Environment Maps." In Proceedings of SIGGRAPH 2001, pp. 497–500.
// https://github.com/eskimoblood/processingSketches/blob/master/data/shader/shinyvert.glsl
// https://github.com/eskimoblood/processingSketches/blob/master/data/shader/shinyvert.glsl
// See table on page 397 of OpenGL programming Guide, 8th Edition Shreiner et al.

// Constants for Old Town Square lighting
const vec3 L00 = vec3( 0.871297, 0.875222, 0.864470);
const vec3 L1m1 = vec3( 0.175058, 0.245335, 0.312891);
const vec3 L10 = vec3( 0.034675, 0.036107, 0.037362);
const vec3 L11 = vec3(-0.004629, -0.029448, -0.048028);
const vec3 L2m2 = vec3(-0.120535, -0.121160, -0.117507);
const vec3 L2m1 = vec3( 0.003242, 0.003624, 0.007511);
const vec3 L20 = vec3(-0.028667, -0.024926, -0.020998);
const vec3 L21 = vec3(-0.077539, -0.086325, -0.091591);
const vec3 L22 = vec3(-0.161784, -0.191783, -0.219152);

	vNormal = vec3(vNormal.x,vNormal.z,-vNormal.y);
	vec3 diffuseColor =  C1 * L22 * (vNormal.x * vNormal.x - vNormal.y * vNormal.y) +
                    C3 * L20 * vNormal.z * vNormal.z +
                    C4 * L00 -
                    C5 * L20 +
                    2.0 * C1 * L2m2 * vNormal.x * vNormal.y +
                    2.0 * C1 * L21  * vNormal.x * vNormal.z +
                    2.0 * C1 * L2m1 * vNormal.y * vNormal.z +
                    2.0 * C2 * L11  * vNormal.x +
                    2.0 * C2 * L1m1 * vNormal.y +
                    2.0 * C2 * L10  * vNormal.z;
    return diffuseColor;
}

vec3 desaturate(vec3 color, float amount) {
    vec3 gray = vec3(dot(vec3(0.2126,0.7152,0.0722), color));
    return vec3(mix(color, gray, amount));
}

void main() {
	vec3 l = normalize(vec3(0.0, 20.0, 30.0));
	vec3 n = normalize(vN);
	vec3 v = normalize(vV);
	vec3 h = normalize(l+v);
	vec3 a = vClr.rgb;
	vec3 d = a * Diffuse;
	a *= Ambient;
	vec3 backcolor = desaturate(0.75 * a + 0.75 * d *  abs(dot(n,l)), 0.5);
	d *= SH(-reflect(n, l) );
	float specular = max(0.0,dot(n,h));
	specular = pow(specular, 1.0/(SpecularRough * SpecularRough));
	color = vec4(a + d + specular* Specular, 1.0);
	float backface = step(0.0, n.z);
	color = vec4(mix(backcolor.rgb, color.rgb,  backface), 1.0);
}