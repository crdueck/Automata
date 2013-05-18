#version 400

layout (vertices = 3) out;
uniform mat4 uModelView;

in vec3 vNormal[];
in vec3 vPosition[];
out vec3 tcPosition[];
out vec3 tcNormal[];

void main()
{
    tcPosition[gl_InvocationID] = vPosition[gl_InvocationID];
    tcNormal[gl_InvocationID] = vNormal[gl_InvocationID];

    vec4 eyePosition = uModelView * vec4(tcPosition[gl_InvocationID], 1);

    float tessLevelInner;
    float tessLevelOuter;
    if (abs(eyePosition.z / eyePosition.w) < 300) {
        tessLevelInner = 1.0;
        tessLevelOuter = 1.0;
    } else {
        tessLevelInner = 1.0;
        tessLevelOuter = 1.0;
    }

    gl_TessLevelInner[0] = tessLevelInner;
    gl_TessLevelOuter[0] = tessLevelOuter;
    gl_TessLevelOuter[1] = tessLevelOuter;
    gl_TessLevelOuter[2] = tessLevelOuter;
}
