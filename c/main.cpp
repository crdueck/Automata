#include <cstdlib>
#include <GL/glew.h>
#include <GL/glfw.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/noise.hpp>
#include <glm/gtc/type_ptr.hpp>

#define INIT_CAMERA(camera) do \
{ \
    (camera).x = 0.0f; \
    (camera).y = 0.0f; \
    (camera).z = 0.0f; \
    (camera).rotX = 0.0f; \
    (camera).rotY = 0.0f; \
} while(0)

typedef struct camera_t
{
    float x;
    float y;
    float z;
    float rotX;
    float rotY;
} Camera;

Camera g_camera;

const char *vertexShader =
    "#version 330\n"
    "in vec3 position;\n"
    "uniform mat4 model;\n"
    "uniform mat4 view;\n"
    "uniform mat4 proj;\n"
    "smooth out vec3 color;\n"
    "void main() {\n"
    //"   gl_Position = proj * view * model * vec4(position, 1.0);\n"
    "   gl_Position = model * vec4(position, 1.0);\n"
    //"   gl_Position = vec4(position, 1.0);\n"
    "   color = position;\n"
    "}";

const char *fragmentShader =
    "#version 330\n"
    "smooth in vec3 color;\n"
    "void main() {\n"
    "   gl_FragColor = vec4(color, 1.0);\n"
    "}";

void GLFWCALL keyCallback(int key, int action)
{
    if (action == GLFW_PRESS) {
        switch (key) {
            case 'W':
                g_camera.x += sin(g_camera.rotY);
                g_camera.z -= cos(g_camera.rotY);
                break;
            case 'S':
                g_camera.x -= sin(g_camera.rotY);
                g_camera.z += cos(g_camera.rotY);
                break;
            case 'A':
                g_camera.x -= cos(g_camera.rotY);
                g_camera.z -= sin(g_camera.rotY);
                break;
            case 'D':
                g_camera.x += cos(g_camera.rotY);
                g_camera.z += sin(g_camera.rotY);
                break;
            case '-':
                g_camera.y += 1;
                break;
            case '=':
                g_camera.y -= 1;
                break;
            default:
                break;
        }
    }
}

void GLFWCALL mousePosCallback(int x, int y)
{
    int w, h;
    glfwGetWindowSize(&w, &h);

    int midX = w * 0.5f;
    int midY = h * 0.5f;

    g_camera.rotX += y - midY;
    g_camera.rotY += midX - x;

    glm::clamp(g_camera.rotX, -90.0f, 90.0f);
    glm::clamp(g_camera.rotY, -180.0f, 180.0f);

    glfwSetMousePos(midX, midY);
}

void GLFWCALL windowSizeCallback(int w, int h)
{
    glViewport(0, 0, w, h);
}

void glInit()
{
    glClearColor(0.5f, 0.7f, 1.0f, 1.0f);
    glCullFace(GL_BACK);
    glDepthFunc(GL_LESS);
    glLineWidth(1.5f);

    glEnable(GL_FOG);
    glFogi(GL_FOG_MODE, GL_LINEAR);
    glFogi(GL_FOG_START, 250);
    glFogi(GL_FOG_END, 2000);

    float fogColor[4] = { 0.5f, 0.7f, 1.0f, 1.0f };
    glFogfv(GL_FOG_COLOR, fogColor);
}

#define INIT_HEIGHTMAP(map, width, height) do \
{ \
    for (size_t i = 0; i < (width); i++) { \
        for (size_t j = 0; j < (height); j++) { \
            (map)[i * 3 + j * 3 * (width) + 0] = (float)i; \
            (map)[i * 3 + j * 3 * (width) + 1] = glm::simplex(glm::vec2(i, j)); \
            (map)[i * 3 + j * 3 * (width) + 2] = (float)j; \
        } \
    } \
} while(0)

static const size_t WIDTH = 10;
static const size_t HEIGHT = 10;

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
    INIT_HEIGHTMAP(heightmap, WIDTH, HEIGHT);

    //for (size_t i = 0; i < WIDTH * HEIGHT * 3; i += 3) {
        //float x = heightmap[i + 0];
        //float y = heightmap[i + 1];
        //float z = heightmap[i + 2];
        //printf("(%f, %f, %f)\n", x, y, z);
    //}

    GLuint indices[WIDTH * HEIGHT * 4];

    int ix = 0;
    for (size_t i = 0; i < WIDTH; i++) {
        for (size_t j = 0; j < HEIGHT; j++) {
            indices[ix++] = (i + 0) + (j + 0) * WIDTH;
            indices[ix++] = (i + 0) + (j + 1) * WIDTH;
            indices[ix++] = (i + 1) + (j + 0) * WIDTH;
            indices[ix++] = (i + 1) + (j + 1) * WIDTH;
        }
    }

    //for (size_t i = 0; i < WIDTH * HEIGHT * 4; i += 4) {
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

    // COMPILE VERTEX SHADER
    GLuint vShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vShader, 1, &vertexShader, NULL);
    glCompileShader(vShader);

    // COMPILE FRAGMENT SHADER
    GLuint fShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fShader, 1, &fragmentShader, NULL);
    glCompileShader(fShader);

    // LINK SHADERS
    GLuint shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vShader);
    glAttachShader(shaderProgram, fShader);
    glLinkProgram(shaderProgram);
    glUseProgram(shaderProgram);

    GLint posAttrib = glGetAttribLocation(shaderProgram, "position");
    glEnableVertexAttribArray(posAttrib);
    glVertexAttribPointer(posAttrib, 3, GL_FLOAT, GL_FALSE, 0, 0);

    // PROJECTION MATRIX
    glm::mat4 proj = glm::perspective(45.0f, 1.5f, 1.0f, 1000.0f);
    GLint uniProj = glGetUniformLocation(shaderProgram, "proj");
    glUniformMatrix4fv(uniProj, 1.0, GL_FALSE, glm::value_ptr(proj));

    // VIEW MATRIX
    glm::mat4 view = glm::lookAt(
        glm::vec3(0.0f, 10.0f, 0.0f),
        glm::vec3(5.0f, 0.0f, 5.0f),
        glm::vec3(0.0f, 1.0f, 0.0f)
    );
    GLint uniView = glGetUniformLocation(shaderProgram, "view");
    glUniformMatrix4fv(uniView, 1, GL_FALSE, glm::value_ptr(view));

    do {
        // MODEL MATRIX
        glm::mat4 model;
        GLint uniModel = glGetUniformLocation(shaderProgram, "model");
        model = glm::rotate(model, g_camera.rotX, glm::vec3(1.0f, 0.0f, 0.0f));
        model = glm::rotate(model, g_camera.rotY, glm::vec3(0.0f, 1.0f, 0.0f));
        model = glm::translate(model, glm::vec3(g_camera.x, g_camera.y, g_camera.z));
        glUniformMatrix4fv(uniModel, 1, GL_FALSE, glm::value_ptr(model));

        // CLEAR BUFFERS AND RENDER
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glDrawElements(GL_LINE_STRIP, WIDTH * HEIGHT * 4, GL_UNSIGNED_INT, NULL);
        glfwSwapBuffers();
    } while(glfwGetKey(GLFW_KEY_ESC) != GLFW_PRESS);

    // FREE RESOURCES
    glDeleteProgram(shaderProgram);
    glDeleteShader(fShader);
    glDeleteShader(vShader);

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);

    glfwCloseWindow();
    glfwTerminate();
    return 0;
}
