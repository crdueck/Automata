#version 400

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

out vec3 vNormal;
out vec3 vPosition;

void main()
{
    vNormal = normal;
    vPosition = position;
}
