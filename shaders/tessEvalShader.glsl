#version 400

layout (triangles, equal_spacing, cw) in;
uniform mat4 uProj;
uniform mat4 uModelView;

in vec3 tcNormal[];
in vec3 tcPosition[];
out vec3 teNormal;
out vec3 tePosition;

void main()
{
    vec3 p0 = gl_TessCoord.x * tcPosition[0];
    vec3 p1 = gl_TessCoord.y * tcPosition[1];
    vec3 p2 = gl_TessCoord.z * tcPosition[2];

    vec3 n0 = gl_TessCoord.x * tcNormal[0];
    vec3 n1 = gl_TessCoord.y * tcNormal[1];
    vec3 n2 = gl_TessCoord.z * tcNormal[2];

    teNormal = n0 + n1 + n2;
    tePosition = p0 + p1 + p2;
    gl_Position = uProj * uModelView * vec4(tePosition, 1);
}
