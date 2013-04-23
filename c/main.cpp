#include <cstdlib>
#define GLFW_INCLUDE_GL3
#include <GL/glfw.h>
#include <GL/glu.h>
#include <glm/glm.hpp>
#include <glm/gtc/noise.hpp>

#define RENDER_VERTEX(x, y, z) do \
{ \
    glColor3f(0.0f, 0.0f, (y)); \
    glVertex3f(10.0f * (x), 100.0f * (y), 10.0f * (z)); \
} while(0)

#define INIT_CAMERA(camera) do \
{ \
    (camera).x = 0.0f; \
    (camera).y = 10.0f; \
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

#define INIT_HEIGHTMAP(hmap, width, height) do \
{ \
    (hmap).w = (width); \
    (hmap).h = (height); \
    (hmap).heightmap = (float *)malloc(3.0f * (width) * (height) * sizeof(float)); \
    for (size_t i = 0; i < (width); i += 3) { \
        for (size_t j = 0; j < (height); j += 3) { \
            (hmap).heightmap[i + j * (width) + 0] = (float)i; \
            (hmap).heightmap[i + j * (width) + 1] = (float)j; \
            (hmap).heightmap[i + j * (width) + 2] = glm::simplex(glm::vec2(i, j)); \
        } \
    } \
} while(0)

typedef struct heightmap_t
{
    size_t w;
    size_t h;
    float *heightmap;
} HeightMap;

void renderWorld(Camera camera, HeightMap world)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45.0f, 1.5f, 1.0f, 10000.0f);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glRotatef(camera.rotX, 1.0f, 0.0f, 0.0f);
    glRotatef(camera.rotY, 0.0f, 1.0f, 0.0f);
    glTranslatef(camera.x, camera.y, camera.z);

    GLuint buf;
    glGenBuffers(1, &buf);
    glBindBuffer(GL_ARRAY_BUFFER, buf);
    glBufferData(GL_ARRAY_BUFFER, sizeof(world.heightmap), world.heightmap, GL_STATIC_DRAW);

    glEnableVertexArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, buf);
    glDrawArrays(GL_TRIANGLES, 0, sizeof(world.heightmap) / 3);
}

void updateWorld(Camera camera, HeightMap world)
{
    return;
}

void GLFWCALL keyCallback(int key, int action)
{
    if (action == GLFW_PRESS) {
        switch (key) {
            case 'W':
            case 'S':
            case 'A':
            case 'D':
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
    glm::clamp(g_camera.rotX, -90.0f, 90.0f);

    g_camera.rotY += midX - x;

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

    float fogColor[4] = { 0.2f, 0.2f, 0.2f, 1.0f };
    glFogfv(GL_FOG_COLOR, fogColor);
}

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

    printf("%s\n", glGetString(GL_VERSION));

    HeightMap world;

    INIT_CAMERA(g_camera);
    INIT_HEIGHTMAP(world, 256, 256);

    do {
        renderWorld(g_camera, world);
        updateWorld(g_camera, world);
        glfwSwapBuffers();
    } while(glfwGetKey(GLFW_KEY_ESC) != GLFW_PRESS);

    free(world.heightmap);
    glfwTerminate();
    return 0;
}
