#version 330

uniform mat4 uModel;
uniform vec4 uDiffuseColor;

in vec4 vColor;
in vec3 vLightDist;
in vec3 vNormal;

out vec4 fColor;

void main()
{
    float diffuse = max(0.0, dot(vNormal, vLightDist));
    fColor = diffuse * uDiffuseColor + vColor;
}
