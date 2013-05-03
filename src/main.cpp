#include <GL/glew.h>
#include <GL/glfw.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/noise.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <vector>

#include "glfwCallbacks.h"
#include "glShaderProgram.hpp"
#include "glVertexArrayObject.hpp"

bool wireFrame = false;
Camera g_camera;

GLuint loadTextureTGA(const char *file)
{
    GLuint texID;
    glGenTextures(1, &texID);

    glBindTexture(GL_TEXTURE_2D, texID);
    glfwLoadTexture2D(file, GLFW_BUILD_MIPMAPS_BIT);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

    return texID;
}

void updateWorld(const double dt)
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

    if (g_camera.rotation & LOOK_LEFT) {
        g_camera.rotY -= moveSpeed;
    } else if (g_camera.rotation & LOOK_RIGHT) {
        g_camera.rotY += moveSpeed;
    }

    if (g_camera.rotation & LOOK_UP) {
        g_camera.rotX -= moveSpeed;
    } else if (g_camera.rotation & LOOK_DOWN) {
        g_camera.rotX += moveSpeed;
    }
}

float harmonic2D(const size_t octave, const float freq, const float x, const float y)
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
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);
    glCullFace(GL_BACK);
    glDepthFunc(GL_LESS);
    glPatchParameteri(GL_PATCH_VERTICES, 3);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
}

static const size_t WIDTH  = 512;
static const size_t HEIGHT = 512;

int main(int argc, char const *argv[])
{
    glfwInit();
    glfwOpenWindow(1280, 720, 8, 8, 8, 0, 32, 0, GLFW_WINDOW);

    glfwDisable(GLFW_MOUSE_CURSOR);
    glfwSetKeyCallback(&keyCallback);
    glfwSetMousePosCallback(&mousePosCallback);
    glfwSetWindowSizeCallback(&windowSizeCallback);
    glfwSwapInterval(1);

    glewInit();
    glInit();

    INIT_CAMERA(g_camera);
    g_camera.rotY = 135.0f;
    g_camera.rotX = 15.0f;
    g_camera.position.y -= 50.0f;

    // BIND BUFFER OBJECTS
    glVertexArrayObject terrain = glVertexArrayObject();

    const int numVertices = WIDTH * HEIGHT;
    std::vector<glm::vec3> heightmap;
    heightmap.reserve(numVertices);
    for (size_t i = 0; i < WIDTH; i++) {
        for (size_t j = 0; j < HEIGHT; j++) {
            float noise = 0.0f;
            noise += harmonic2D(10,  75.0f, i, j);
            noise += harmonic2D(10, 135.0f, i, j);
            noise += harmonic2D(3,  170.0f, i, j);
            noise += harmonic2D(3,  225.0f, i, j);
            noise *= 15.0f;
            heightmap.push_back(glm::vec3(i, noise, j));
        }
    }
    terrain.bindBuffer(GL_ARRAY_BUFFER, numVertices, &heightmap[0]);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

    const int numIndices = (WIDTH - 1) * (HEIGHT - 1) * 6;
    std::vector<GLuint> indices;
    indices.reserve(numIndices);
    for (size_t i = 0; i < WIDTH - 1; i++) {
        for (size_t j = 0; j < HEIGHT - 1; j++) {
            indices.push_back((i + 0) + (j + 0) * WIDTH);
            indices.push_back((i + 0) + (j + 1) * WIDTH);
            indices.push_back((i + 1) + (j + 0) * WIDTH);
            indices.push_back((i + 1) + (j + 0) * WIDTH);
            indices.push_back((i + 0) + (j + 1) * WIDTH);
            indices.push_back((i + 1) + (j + 1) * WIDTH);
        }
    }
    terrain.bindBuffer(GL_ELEMENT_ARRAY_BUFFER, numIndices, &indices[0]);

    std::vector<glm::vec3> normals;
    normals.reserve(numVertices);
    for (std::vector<GLuint>::const_iterator i = indices.begin(); i != indices.end(); std::advance(i, 3)) {
        glm::vec3 u = heightmap[*(i + 0)];
        glm::vec3 v = heightmap[*(i + 1)];
        glm::vec3 w = heightmap[*(i + 2)];
        normals[*i] = glm::normalize(glm::cross(v - u, w - u));
    }
    terrain.bindBuffer(GL_ARRAY_BUFFER, numVertices, &normals[0]);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, 0);

    // COMPILE AND LINK SHADERS
    glShaderProgram shaderProgram = glShaderProgram();
    shaderProgram.loadShaderSource(GL_VERTEX_SHADER, "shaders/vertexShader.glsl");
    shaderProgram.loadShaderSource(GL_TESS_CONTROL_SHADER, "shaders/tessControlShader.glsl");
    shaderProgram.loadShaderSource(GL_TESS_EVALUATION_SHADER, "shaders/tessEvalShader.glsl");
    shaderProgram.loadShaderSource(GL_FRAGMENT_SHADER, "shaders/fragmentShader.glsl");
    shaderProgram.link();
    shaderProgram.bind();

    // LIGHTING
    GLint uLightPosition = shaderProgram.getUniformLocation("uLightPosition");
    glUniform3f(uLightPosition, 200.0f, 50.0f, 500.0f);

    // TEXTURES
    GLuint sandTex = loadTextureTGA("textures/grass.tga");

    // PROJECTION MATRIX
    GLint uProj = shaderProgram.getUniformLocation("uProj");
    glm::mat4 proj = glm::perspective(45.0f, 1.5f, 1.0f, 10000.0f);
    glUniformMatrix4fv(uProj, 1.0, GL_FALSE, &proj[0][0]);

    GLint uModelView = shaderProgram.getUniformLocation("uModelView");
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
        glUniformMatrix4fv(uModelView, 1.0, GL_FALSE, &model[0][0]);

        // CLEAR BUFFERS AND RENDER
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glPolygonMode(GL_FRONT_AND_BACK, (wireFrame ? GL_LINE : GL_FILL));
        terrain.bind();
        glDrawElements(GL_PATCHES, numIndices, GL_UNSIGNED_INT, 0);
        glfwSwapBuffers();
    } while(glfwGetKey(GLFW_KEY_ESC) != GLFW_PRESS);

    glfwCloseWindow();
    glfwTerminate();
    return 0;
}
