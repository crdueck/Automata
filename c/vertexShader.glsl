#version 330

in vec3 position;

uniform mat4 uProj;
uniform mat4 uModel;

out vec4 vColor;

void main()
{
    vColor = vec4(0.1, 0.1, position.y, 1.0);
    gl_Position = vec4(position.x, 10.0 * position.y, position.z, 0.1);
    gl_Position = uProj * uModel * gl_Position;
}
