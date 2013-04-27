#version 330

layout(vertices = 3) out;

uniform float TessLevelInner;
uniform float TessLevelOuter;

in vec3 vPosition[];
out vec3 tcPosition[]

void main ()
{
    tcPosition[gl_InvocationID] = vPosition[gl_InvocationID];
    if (gl_InvocationID == 0) {
        gl_TessLevelInner[0] = TessLevelInner;
        gl_TessLevelOuter[0] = TessLevelOuter;
        gl_TessLevelOuter[1] = TessLevelOuter;
        gl_TessLevelOuter[2] = TessLevelOuter;
    }
}
