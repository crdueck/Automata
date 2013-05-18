#include <GL/glfw.h>
#include <glm/glm.hpp>

void GLFWCALL keyCallback(int, int);
void GLFWCALL mousePosCallback(int, int);
void GLFWCALL windowSizeCallback(int, int);

enum {
    MOVE_FORWARD  = 0x01,
    MOVE_BACKWARD = 0x02,
    MOVE_LEFT     = 0x04,
    MOVE_RIGHT    = 0x08,
    MOVE_UP       = 0x10,
    MOVE_DOWN     = 0x20,
};

enum {
    LOOK_LEFT     = 0x01,
    LOOK_RIGHT    = 0x02,
    LOOK_UP       = 0x04,
    LOOK_DOWN     = 0x08,
};

struct Camera
{
    glm::vec3 position;
    float rotX;
    float rotY;
    float speed;
    uint8_t movement;
    uint8_t rotation;

    Camera()
        : position(glm::vec3(0))
        , rotX(0.0f)
        , rotY(0.0f)
        , speed(100.0f)
        , movement(0)
        , rotation(0)
    {}
};
