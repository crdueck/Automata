#include <cstdlib>
#include <cassert>
#include <GL/glew.h>
#include <GL/glfw.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/noise.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "callbacks.hpp"

const char *vertexSource =
    "#version 330\n"
    "in vec3 position;\n" // GLSL 3.30
    //"attribute vec3 position;\n" // GLSL 1.20
    "uniform mat4 model;\n"
    "uniform mat4 proj;\n"
    "out vec3 color;\n" // GLSL 3.30
    //"varying vec3 color;\n" // GLSL 1.20
    "void main() {\n"
    "   gl_Position = proj * model * vec4(position, 1.0);\n"
    "   color = position / 256.0f;\n"
    "}";

const char *fragmentSource =
    "#version 330\n"
    "in vec3 color;\n" // GLSL 3.30
    //"varying vec3 color;\n" // GLSL 1.20
    "void main() {\n"
    //"   gl_FragColor = vec4(0.0, 0.0, color.b, 1.0);\n"
    "   gl_FragColor = vec4(color, 1.0);\n"
    "}";

Camera g_camera;

void glInit()
{
    glClearColor(0.5f, 0.7f, 1.0f, 1.0f);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glLineWidth(1.5f);

    glEnable(GL_FOG);
    glFogi(GL_FOG_MODE, GL_LINEAR);
    glFogf(GL_FOG_DENSITY, 0.2f);
    glFogi(GL_FOG_START, 30);
    glFogi(GL_FOG_END, 100);
    glHint(GL_FOG_HINT, GL_NICEST);

    GLfloat fogColor[4] = { 0.5f, 0.7f, 1.0f, 1.0f };
    glFogfv(GL_FOG_COLOR, fogColor);
}

void updateWorld(float dt)
{
    float moveSpeed = g_camera.speed * dt;
    float phi = glm::radians(g_camera.rotY);

    // UPDATE CAMERA
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
    float noise = 0.0f;
    for (size_t i = 1; i < octave; i++) {
        float r = 1 << (i - 1);
        noise += glm::simplex(glm::vec2(x * r / freq, y * r / freq)) / r;
    }
    return noise / (2.0f - 1.0f / (2 ^ (octave - 1)));
}

static const size_t WIDTH = 256;
static const size_t HEIGHT = 256;

int main(int argc, char const *argv[])
{
    glfwInit();
    glfwOpenWindow(1080, 720, 0, 0, 0, 0, 0, 0, GLFW_WINDOW);

    glfwDisable(GLFW_MOUSE_CURSOR);
    glfwSetKeyCallback(&keyCallback);
    glfwSetMousePosCallback(&mousePosCallback);
    glfwSetWindowSizeCallback(&windowSizeCallback);
    glfwSwapInterval(1);

    glInit();
    glewInit();

    INIT_CAMERA(g_camera);

    float heightmap[WIDTH * HEIGHT * 3];

    for (size_t i = 0; i < WIDTH; ++i) {
        for (size_t j = 0; j < HEIGHT; ++j) {

            //int ix1 = i * 3 + j * 3 * WIDTH + 0;
            //int ix2 = i * 3 + j * 3 * WIDTH + 1;
            //int ix3 = i * 3 + j * 3 * WIDTH + 2;
            //printf("(%d, %d, %d)\n", ix1, ix2, ix3);

            float noise = harmonic2D(5, 25.0f, i, j);
            noise += harmonic2D(3, 45.0f, i, j);
            noise += harmonic2D(3, 110.f, i, j);
            noise += harmonic2D(5, 220.f, i, j);

            heightmap[i * 3 + j * 3 * WIDTH + 0] = (float)i;
            heightmap[i * 3 + j * 3 * WIDTH + 1] = 10.0f * noise;
            heightmap[i * 3 + j * 3 * WIDTH + 2] = (float)j;

        }
    }

    GLuint indices[(WIDTH - 1) * (HEIGHT - 1) * 4];

    size_t ix = 0;
    for (size_t i = 0; i < WIDTH - 2; i++) {
        for (size_t j = 0; j < HEIGHT - 2; j++) {
            indices[ix++] = (i + 0) + (j + 0) * WIDTH;
            indices[ix++] = (i + 0) + (j + 1) * WIDTH;
            indices[ix++] = (i + 1) + (j + 0) * WIDTH;
            indices[ix++] = (i + 1) + (j + 1) * WIDTH;
        }
    }

    // VERTEX ARRAY OBJECT
    GLuint vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);

    // VERTEX BUFFER OBJECT
    GLuint vbo;
    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(heightmap), heightmap, GL_STATIC_DRAW);

    // ELEMENT BUFFER OBJECT
    GLuint ebo;
    glGenBuffers(1, &ebo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);

    // COMPILE VERTEX SHADER
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexSource, 0);
    glCompileShader(vertexShader);

    // COMPILE FRAGMENT SHADER
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentSource, 0);
    glCompileShader(fragmentShader);

    // LINK SHADERS
    GLuint shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);
    glUseProgram(shaderProgram);

    // PROJECTION MATRIX
    glm::mat4 proj = glm::perspective(45.0f, 1.5f, 1.0f, 1000.0f);
    GLint uniProj = glGetUniformLocation(shaderProgram, "proj");
    glUniformMatrix4fv(uniProj, 1.0, GL_FALSE, glm::value_ptr(proj));

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
        GLint uniModel = glGetUniformLocation(shaderProgram, "model");
        glUniformMatrix4fv(uniModel, 1, GL_FALSE, glm::value_ptr(model));

        // CLEAR BUFFERS AND RENDER
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        glBindBuffer(GL_ARRAY_BUFFER, vbo);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

        glDrawElements(GL_TRIANGLE_STRIP, ix, GL_UNSIGNED_INT, NULL);
        glfwSwapBuffers();
    } while(glfwGetKey(GLFW_KEY_ESC) != GLFW_PRESS);

    // FREE RESOURCES
    glDeleteProgram(shaderProgram);
    glDeleteShader(fragmentShader);
    glDeleteShader(vertexShader);

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);

    glfwCloseWindow();
    glfwTerminate();
    return 0;
}
