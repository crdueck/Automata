#include <GL/glfw.h>
#include <glm/glm.hpp>

void GLFWCALL keyCallback(int, int);
void GLFWCALL mousePosCallback(int, int);
void GLFWCALL windowSizeCallback(int, int);

enum {
    MOVE_FORWARD    = 0x01,
    MOVE_BACKWARD   = 0x02,
    MOVE_LEFT       = 0x04,
    MOVE_RIGHT      = 0x08,
    MOVE_UP         = 0x10,
    MOVE_DOWN       = 0x20,
};

typedef struct camera_t
{
    glm::vec3 position;
    float rotX;
    float rotY;
    float speed;
    uint8_t movement;
} Camera;

#define INIT_CAMERA(camera) do \
{ \
    (camera).position = glm::vec3(0); \
    (camera).rotX = 0.0f; \
    (camera).rotY = 0.0f; \
    (camera).movement = 0; \
    (camera).speed = 1000.0f; \
} while(0)
