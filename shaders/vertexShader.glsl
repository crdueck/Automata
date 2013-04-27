#version 330

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

uniform mat4 uProj;
uniform mat4 uModel;
uniform vec3 uLightPosition;

out vec4 vColor;
out vec3 vLightDist;
out vec3 vPosition;
out vec3 vNormal;

void main()
{
    vColor = vec4(0.0, 0.0, position.y, 1.0);
    vLightDist = (uModel * vec4(position, 1.0)).xyz - uLightPosition;
    vPosition = vec4(position.x, 10.0 * position.y, position.z, 0.1);
    vPosition = uProj * uModel * vPosition;
    vNormal = (uModel * vec4(normal, 1.0)).xyz;
    vNormal = normalize(vNormal);
}
