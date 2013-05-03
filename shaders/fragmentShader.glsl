#version 400

uniform vec3 uLightPosition;
uniform mat4 uModelView;
uniform sampler2D uTex;

in vec3 teNormal;
in vec3 tePosition;
out vec4 fColor;

const vec3 ambientMaterial = vec3(0.0, 0.0, 0.0);
const vec3 diffuseMaterial = vec3(0.3, 0.3, 0.3);

const vec3 fogColor = vec3(0.5, 0.7, 1.0);
const int fogStart = 350;
const int fogEnd = 700;

void main()
{
    vec3 N = normalize(teNormal);

    vec3 L = normalize(uLightPosition - tePosition);
    float diffuse = max(0, dot(N, L));

    vec3 R = normalize(reflect(-L, N));
    float specular = pow(max(0, dot(N, R)), 32);

    vec3 color = ambientMaterial;
    color += diffuse * diffuseMaterial;
    if (diffuse != 0) {
        color += vec3(specular);
    }

    vec4 eyePosition = uModelView * vec4(tePosition, 1);
    float fogCoord = abs(eyePosition.z / eyePosition.w);
    float fog = (fogEnd - fogCoord) / (fogEnd - fogStart);
    fog = 1.0 - clamp(fog, 0.0, 1.0);

    color = mix(color, fogColor, fog);

    fColor = vec4(color, 1);
}
