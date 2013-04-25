#version 120

uniform mat4 u_Proj;
uniform mat4 u_Model;

varying vec3 v_Color;
varying vec3 v_Normal;

void main() {
    vec3 position = vec3(gl_Vertex.x, 10.0 * gl_Vertex.y, gl_Vertex.z);

    v_Color = vec3(0.0, 0.0, gl_Vertex.y);
    v_Normal = normalize(gl_NormalMatrix * gl_Normal);

    gl_Position = u_Proj * u_Model * vec4(position, 0.1);;
}
