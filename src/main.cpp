#include <GL/glew.h>
#include <GL/glfw.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/noise.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <fstream>
#include <string>

#include "callbacks.hpp"

Camera g_camera;

GLchar *loadShaderSource(const char *file)
{
    FILE *fptr;
    char *buf;
    long length;

    fptr = fopen(file, "rb");
    if (!fptr) return NULL;
    fseek(fptr, 0, SEEK_END);
    length = ftell(fptr);
    buf = (char*)malloc(length + 1);
    fseek(fptr, 0, SEEK_SET);
    fread(buf, length, 1, fptr);
    fclose(fptr);
    buf[length] = 0;
    return buf;
}

void updateWorld(float dt)
{
    float moveSpeed = g_camera.speed * dt;
    float phi = glm::radians(g_camera.rotY);

    if (g_camera.movement & MOVE_FORWARD) {
        g_camera.position.x -= moveSpeed * sin(phi);
        g_camera.position.z += moveSpeed * cos(phi);
    } else if (g_camera.movement & MOVE_BACKWARD) {
        g_camera.position.x += moveSpeed * sin(phi);
        g_camera.position.z -= moveSpeed * cos(phi);
    }

    if (g_camera.movement & MOVE_LEFT) {
        g_camera.position.x += moveSpeed * cos(phi);
        g_camera.position.z += moveSpeed * sin(phi);
    } else if (g_camera.movement & MOVE_RIGHT) {
        g_camera.position.x -= moveSpeed * cos(phi);
        g_camera.position.z -= moveSpeed * sin(phi);
    }

    if (g_camera.movement & MOVE_UP) {
        g_camera.position.y -= moveSpeed;
    } else if (g_camera.movement & MOVE_DOWN) {
        g_camera.position.y += moveSpeed;
    }
}

float harmonic2D(size_t octave, float freq, float x, float y)
{
    float r;
    float noise = 0.0f;
    for (size_t i = 1; i <= octave; i++) {
        r = 1 << (i - 1);
        noise += glm::simplex(glm::vec2(x * r / freq, y * r / freq)) / r;
    }
    return noise / (2.0f - 1.0f / (1 << (octave - 1)));
}

void glInit()
{
    glClearColor(0.5f, 0.7f, 1.0f, 1.0f);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glLineWidth(1.5f);
}

static const size_t WIDTH = 256;
static const size_t HEIGHT = 256;

int main(int argc, char const *argv[])
{
    glfwInit();
    glfwOpenWindow(1080, 720, 8, 8, 8, 0, 32, 0, GLFW_WINDOW);

    glfwDisable(GLFW_MOUSE_CURSOR);
    glfwSetKeyCallback(&keyCallback);
    glfwSetMousePosCallback(&mousePosCallback);
    glfwSetWindowSizeCallback(&windowSizeCallback);
    glfwSwapInterval(1);

    glewInit();
    glInit();

    INIT_CAMERA(g_camera);
    g_camera.rotY = 135.0f;
    g_camera.position.y -= 200.0f;

    float *heightmap = new float[WIDTH * HEIGHT * 3];
    for (size_t i = 0, ix = 0; i < WIDTH; i++) {
        for (size_t j = 0; j < HEIGHT; j++) {
            float noise = 0.0f;
            noise += 0.2f * harmonic2D(3, 5.0f, i, j);
            noise += harmonic2D(3,  45.0f, i, j);
            noise += harmonic2D(5, 110.0f, i, j);
            noise += harmonic2D(5, 220.0f, i, j);
            noise = glm::max(0.0f, noise);

            heightmap[ix++] = (float)i;
            heightmap[ix++] = noise;
            heightmap[ix++] = (float)j;
        }
    }

    const int numIndices = (WIDTH - 1) * (HEIGHT - 1) * 6;
    GLuint *indices = new GLuint[numIndices];
    for (size_t i = 0, ix = 0; i < WIDTH - 1; i++) {
        for (size_t j = 0; j < HEIGHT - 1; j++) {
            indices[ix++] = (i + 0) + (j + 0) * WIDTH;
            indices[ix++] = (i + 1) + (j + 0) * WIDTH;
            indices[ix++] = (i + 0) + (j + 1) * WIDTH;
            indices[ix++] = (i + 0) + (j + 1) * WIDTH;
            indices[ix++] = (i + 1) + (j + 0) * WIDTH;
            indices[ix++] = (i + 1) + (j + 1) * WIDTH;
        }
    }

    float *normals = new float[WIDTH * HEIGHT * 3];
    for (size_t i = 0, ix = 0; i < WIDTH * HEIGHT * 3; ) {
        float x1 = heightmap[i++];
        float y1 = heightmap[i++];
        float z1 = heightmap[i++];

        if (i >= WIDTH * HEIGHT * 3) break;

        float x2 = heightmap[i++];
        float y2 = heightmap[i++];
        float z2 = heightmap[i++];

        if (i >= WIDTH * HEIGHT * 3) break;

        float x3 = heightmap[i++];
        float y3 = heightmap[i++];
        float z3 = heightmap[i++];

        glm::vec3 u(x1, y1, z1);
        glm::vec3 v(x2, y2, z2);
        glm::vec3 z(x3, y3, z3);
        glm::vec3 normal = glm::cross(z - u, z - v);
        normals[ix++] = normal.x;
        normals[ix++] = normal.y;
        normals[ix++] = normal.z;
    }

    // VERTEX ARRAY OBJECT
    GLuint vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);

    // ELEMENT BUFFER OBJECT
    GLuint ebo;
    glGenBuffers(1, &ebo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, numIndices * sizeof(GLuint), indices, GL_STATIC_DRAW);
    delete[] indices;

    // VERTEX BUFFER OBJECT
    GLuint vbo;
    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, WIDTH * HEIGHT * 3 * sizeof(float), heightmap, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
    delete[] heightmap;

    // NORMAL BUFFER OBJECT
    GLuint nbo;
    glGenBuffers(1, &nbo);
    glBindBuffer(GL_ARRAY_BUFFER, nbo);
    glBufferData(GL_ARRAY_BUFFER, WIDTH * HEIGHT * 3 * sizeof(float), normals, GL_STATIC_DRAW);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, 0);
    delete[] normals;

    GLchar *shaderSource;

    // COMPILE VERTEX SHADER
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    shaderSource = loadShaderSource("vertexShader.glsl");
    glShaderSource(vertexShader, 1, (const GLchar**)&shaderSource, 0);
    free(shaderSource);
    glCompileShader(vertexShader);

    // COMPILE FRAGMENT SHADER
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    shaderSource = loadShaderSource("fragmentShader.glsl");
    glShaderSource(fragmentShader, 1, (const GLchar**)&shaderSource, 0);
    free(shaderSource);
    glCompileShader(fragmentShader);

    // LINK SHADERS
    GLuint shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);
    glUseProgram(shaderProgram);

    GLint logLength;
    glGetProgramiv(shaderProgram, GL_INFO_LOG_LENGTH, &logLength);
    GLchar *log = new GLchar[logLength];
    glGetProgramInfoLog(shaderProgram, logLength, 0, log);
    printf("%s\n", log);

    // PROJECTION MATRIX
    glm::mat4 proj = glm::perspective(45.0f, 1.5f, 1.0f, 10000.0f);
    GLint uProj = glGetUniformLocation(shaderProgram, "uProj");
    glUniformMatrix4fv(uProj, 1.0, GL_FALSE, glm::value_ptr(proj));

    // LIGHTING
    glm::vec4 diffuseColor(0.6f, 0.8f, 1.0f, 1.0f);
    GLint uDiffuseColor = glGetUniformLocation(shaderProgram, "uDiffuseColor");
    glUniform3fv(uDiffuseColor, 1.0, glm::value_ptr(diffuseColor));

    glm::vec3 lightPosition(100.0f, 60.0f, 100.f);
    GLint uLightPosition = glGetUniformLocation(shaderProgram, "uLightPosition");
    glUniform3fv(uLightPosition, 1.0, glm::value_ptr(lightPosition));

    double lastTime = glfwGetTime();

    do {
        double currentTime = glfwGetTime();
        double dt = currentTime - lastTime;
        lastTime = currentTime;

        updateWorld(dt);

        // MODEL MATRIX
        glm::mat4 model;
        model = glm::rotate(model, g_camera.rotX, glm::vec3(1.0f, 0.0f, 0.0f));
        model = glm::rotate(model, g_camera.rotY, glm::vec3(0.0f, 1.0f, 0.0f));
        model = glm::translate(model, g_camera.position);
        GLint uModel = glGetUniformLocation(shaderProgram, "uModel");
        glUniformMatrix4fv(uModel, 1, GL_FALSE, glm::value_ptr(model));

        // CLEAR BUFFERS AND RENDER
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glBindVertexArray(vao);
        glDrawElements(GL_TRIANGLES, numIndices, GL_UNSIGNED_INT, NULL);
        glfwSwapBuffers();
    } while(glfwGetKey(GLFW_KEY_ESC) != GLFW_PRESS);

    // FREE RESOURCES
    glDeleteProgram(shaderProgram);
    glDeleteShader(fragmentShader);
    glDeleteShader(vertexShader);

    glDeleteBuffers(1, &vbo);
    glDeleteBuffers(1, &ebo);
    glDeleteBuffers(1, &nbo);

    glDeleteVertexArrays(1, &vao);

    glfwCloseWindow();
    glfwTerminate();
    return 0;
}
