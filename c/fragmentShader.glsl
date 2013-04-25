#version 120

varying vec3 v_Color;
varying vec3 v_Normal;

void main() {
    gl_FragColor = vec4(v_Color, 1.0);
}
