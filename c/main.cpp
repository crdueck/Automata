#include <cstdlib>
#include <cassert>
#include <GL/glew.h>
#include <GL/glfw.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/noise.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "callbacks.hpp"

Camera g_camera;

void glInit()
{
    glClearColor(0.5f, 0.7f, 1.0f, 1.0f);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glLineWidth(1.5f);

    //glEnable(GL_FOG);
    //glFogi(GL_FOG_MODE, GL_LINEAR);
    //glFogf(GL_FOG_DENSITY, 0.2f);
    //glFogi(GL_FOG_START, 30.0f);
    //glFogi(GL_FOG_END, 100.0f);
    //glHint(GL_FOG_HINT, GL_NICEST);
    //GLfloat fogColor[4] = { 0.5f, 0.7f, 1.0f, 1.0f };
    //glFogfv(GL_FOG_COLOR, fogColor);

    //glEnable(GL_LIGHTING);
    //glEnable(GL_LIGHT0);
    //GLfloat diffuseLight[3] = { 1.0f, 1.0f, 1.0f };
    //GLfloat ambientLight[3] = { 0.2f, 0.2f, 0.2f };
    //glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseLight);
    //glLightfv(GL_LIGHT0, GL_AMBIENT, ambientLight);
    //GLfloat lightPosition[4] = { 100.0f, 120.0f, 100.0f };
    //glLightfv(GL_LIGHT0, GL_POSITION, lightPosition);
    //GLfloat diffuseMat[3] = { 0.2f, 0.2f, 0.2f };
    //GLfloat ambientMat[3] = { 0.0f, 0.0f, 0.0f };
    //GLfloat specularMat[3] = { 0.2f, 0.2f, 0.2f };
    //GLfloat shininess = 0.50;

    //glMaterialfv(GL_FRONT, GL_DIFFUSE, diffuseMat);
    //glMaterialfv(GL_FRONT, GL_AMBIENT, ambientMat);
    //glMaterialfv(GL_FRONT, GL_SPECULAR, specularMat);
    //glMaterialfv(GL_FRONT, GL_SHININESS, &shininess);
}

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
    float r;
    float noise = 0.0f;
    for (size_t i = 1; i <= octave; i++) {
        r = 1 << (i - 1);
        noise += glm::simplex(glm::vec2(x * r / freq, y * r / freq)) / r;
    }
    return noise / (2.0f - 1.0f / (1 << (octave - 1)));
}

static const size_t WIDTH = 512;
//static const size_t HEIGHT = 450;
static const size_t HEIGHT = 512;

int main(int argc, char const *argv[])
{
    glfwInit();
    glfwOpenWindow(1080, 720, 8, 8, 8, 0, 32, 0, GLFW_WINDOW);

    glfwDisable(GLFW_MOUSE_CURSOR);
    glfwSetKeyCallback(&keyCallback);
    glfwSetMousePosCallback(&mousePosCallback);
    glfwSetWindowSizeCallback(&windowSizeCallback);
    glfwSwapInterval(1);

    glInit();
    glewInit();

    INIT_CAMERA(g_camera);

    float heightmap[WIDTH * HEIGHT * 3];

    for (size_t i = 0; i < WIDTH; i++) {
        for (size_t j = 0; j < HEIGHT; j++) {
            float noise = 0.0f;
            noise += 0.2f * harmonic2D(3, 5.0f, i, j);
            noise += harmonic2D(3,  45.0f, i, j);
            noise += harmonic2D(5, 110.0f, i, j);
            noise += harmonic2D(5, 220.0f, i, j);

            heightmap[(i * 3) + (j * 3 * WIDTH) + 0] = (float)i;
            heightmap[(i * 3) + (j * 3 * WIDTH) + 1] = glm::max(0.0f, noise);
            heightmap[(i * 3) + (j * 3 * WIDTH) + 2] = (float)j;
        }
    }

    GLuint indices[WIDTH * HEIGHT * 6];

    size_t ix = 0;
    for (size_t i = 0; i < WIDTH - 1; i++) {
        for (size_t j = 0; j < HEIGHT - 1; j++) {
            indices[ix++] = (i + 0) + (j + 0) * WIDTH;
            indices[ix++] = (i + 0) + (j + 1) * WIDTH;
            indices[ix++] = (i + 1) + (j + 0) * WIDTH;
            indices[ix++] = (i + 1) + (j + 0) * WIDTH;
            indices[ix++] = (i + 0) + (j + 1) * WIDTH;
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

    // PROJECTION MATRIX
    glm::mat4 proj = glm::perspective(45.0f, 1.5f, 1.0f, 10000.0f);
    GLint uniProj = glGetUniformLocation(shaderProgram, "u_Proj");
    glUniformMatrix4fv(uniProj, 1.0, GL_FALSE, glm::value_ptr(proj));

    double lastTime = glfwGetTime();

    do {
        double currentTime = glfwGetTime();
        double dt = currentTime - lastTime;
        lastTime = currentTime;

        // UPDATE WORLD
        updateWorld(dt);

        // MODEL MATRIX
        glm::mat4 model;
        model = glm::rotate(model, g_camera.rotX, glm::vec3(1.0f, 0.0f, 0.0f));
        model = glm::rotate(model, g_camera.rotY, glm::vec3(0.0f, 1.0f, 0.0f));
        model = glm::translate(model, g_camera.position);
        GLint uniModel = glGetUniformLocation(shaderProgram, "u_Model");
        glUniformMatrix4fv(uniModel, 1, GL_FALSE, glm::value_ptr(model));

        // CLEAR BUFFERS AND RENDER
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        glBindBuffer(GL_ARRAY_BUFFER, vbo);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
        glDrawElements(GL_TRIANGLES, ix, GL_UNSIGNED_INT, NULL);
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
