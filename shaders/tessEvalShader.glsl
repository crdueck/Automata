layout (triangles, equal_spacing, cw) in;

uniform mat4 uProj;
uniform mat4 uModel;

in vec3 tcPosition[];
out vec3 tePosition;
out vec3 tePatchDistance;

void main()
{
    vec3 p0 = gl_TessCoord.x * tcPosition[0]
    vec3 p1 = gl_TessCoord.y * tcPosition[1]
    vec3 p2 = gl_TessCoord.z * tcPosition[2]
    tePatchDistance = gl_TessCoord;
    tePosition = normalize(p0 + p1 + p2);
    gl_Position = uProj * uModel  * vec4(tePosition, 1);
}
