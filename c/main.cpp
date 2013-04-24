#include <cassert>
#include <GL/glew.h>
#include <GL/glfw.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/noise.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "callbacks.hpp"

const char *vertexSource =
    "#version 120\n"
    //"in vec3 position;\n" // GLSL 3.30
    "attribute vec3 position;\n" // GLSL 1.20
    "uniform mat4 model;\n"
    "uniform mat4 proj;\n"
    //"out vec3 color;\n" // GLSL 3.30
    "varying vec3 color;\n" // GLSL 1.20
    "void main() {\n"
    "   gl_Position = proj * model * vec4(position, 1.0);\n"
    "   color = position;\n"
    "}";

const char *fragmentSource =
    "#version 120\n"
    //"in vec3 color;\n" // GLSL 3.30
    "varying vec3 color;\n" // GLSL 1.20
    "void main() {\n"
    "   gl_FragColor = vec4(0.0, 0.0, color.b, 1.0);\n"
    "}";

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
    glFogi(GL_FOG_START, 250);
    glFogi(GL_FOG_END, 2000);

    float fogColor[4] = { 0.5f, 0.7f, 1.0f, 1.0f };
    glFogfv(GL_FOG_COLOR, fogColor);
}

static const size_t WIDTH = 64;
static const size_t HEIGHT = 64;

Camera g_camera;

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

            heightmap[i * 3 + j * 3 * WIDTH + 0] = (float)i;
            heightmap[i * 3 + j * 3 * WIDTH + 1] = glm::simplex(glm::vec2(i, j));
            heightmap[i * 3 + j * 3 * WIDTH + 2] = (float)j;

        }
    }

    GLuint indices[WIDTH * HEIGHT * 4];

    size_t ix = 0;
    for (size_t i = 0; i < WIDTH - 1; i++) {
        for (size_t j = 0; j < HEIGHT - 1; j++) {

            indices[ix++] = (i + 0) + (j + 0) * WIDTH;
            indices[ix++] = (i + 0) + (j + 1) * WIDTH;
            indices[ix++] = (i + 1) + (j + 0) * WIDTH;
            indices[ix++] = (i + 1) + (j + 1) * WIDTH;

        }
    }

    //for (size_t i = 0; i < ix; i += 4) {
        //int a = indices[i + 0];
        //int b = indices[i + 1];
        //int c = indices[i + 2];
        //int d = indices[i + 3];
        //printf("(%d, %d, %d, %d)", a, b, c, d);
    //}

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

    GLint status;
    GLint infoLogLength;
    GLchar *strInfoLog;

    // COMPILE VERTEX SHADER
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexSource, 0);
    glCompileShader(vertexShader);

    // CHECK COMPILE STATUS
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &status);
    if (status == GL_FALSE) {
        glGetShaderiv(vertexShader, GL_INFO_LOG_LENGTH, &infoLogLength);
        strInfoLog = new GLchar[infoLogLength + 1];
        glGetShaderInfoLog(vertexShader, infoLogLength, NULL, strInfoLog);
        printf("%s\n", strInfoLog);
        delete[] strInfoLog;
    }

    // COMPILE FRAGMENT SHADER
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentSource, 0);
    glCompileShader(fragmentShader);

    // CHECK COMPILE STATUS
    glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &status);
    if (status == GL_FALSE) {
        glGetShaderiv(fragmentShader, GL_INFO_LOG_LENGTH, &infoLogLength);
        strInfoLog = new GLchar[infoLogLength + 1];
        glGetShaderInfoLog(fragmentShader, infoLogLength, NULL, strInfoLog);
        printf("%s\n", strInfoLog);
        delete[] strInfoLog;
    }

    // LINK SHADERS
    GLuint shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);
    glUseProgram(shaderProgram);

    // CHECK LINK STATUS
    glGetProgramiv(shaderProgram, GL_LINK_STATUS, &status);
    if (status == GL_FALSE) {
        glGetProgramiv(shaderProgram, GL_INFO_LOG_LENGTH, &infoLogLength);
        strInfoLog = new GLchar[infoLogLength + 1];
        glGetProgramInfoLog(shaderProgram, infoLogLength, NULL, strInfoLog);
        printf("Linker failure: %s\n", strInfoLog);
        delete[] strInfoLog;
    }

    // PROJECTION MATRIX
    glm::mat4 proj = glm::perspective(45.0f, 1.5f, 1.0f, 1000.0f);
    GLint uniProj = glGetUniformLocation(shaderProgram, "proj");
    glUniformMatrix4fv(uniProj, 1.0, GL_FALSE, glm::value_ptr(proj));

    do {
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
