#version 400

layout (triangles_adjacency) in;
layout (triangle_strip, max_vertices = 3) out;
uniform mat4 uModelView;

in vec3 tePosition[];
out vec3 gNormal;

void main()
{
    vec3 x1 = tePosition[0] - tePosition[1];
    vec3 x2 = tePosition[0] - tePosition[2];

    vec3 y1 = tePosition[0] - tePosition[2];
    vec3 y2 = tePosition[0] - tePosition[4];

    vec3 z1 = tePosition[0] - tePosition[4];
    vec3 z2 = tePosition[0] - tePosition[5];

    vec3 n1 = cross(x1, x2);
    vec3 n2 = cross(y1, y2);
    vec3 n3 = cross(z1, z2);

    gl_Position = gl_in[0].gl_Position;
    EmitVertex();

    gPatchDistance = tePatchDistance[0];
    gTriangleDistance = vec3(1, 0, 0);
    gl_Position = gl_in[0].gl_Position;
    EmitVertex();

    gPatchDistance = tePatchDistance[1];
    gTriangleDistance = vec3(0, 1, 0);
    gl_Position = gl_in[1].gl_Position;
    EmitVertex();

    gPatchDistance = tePatchDistance[2];
    gTriangleDistance = vec3(0, 0, 1);
    gl_Position = gl_in[2].gl_Position;
    EmitVertex();

    EndPrimitive();
}
