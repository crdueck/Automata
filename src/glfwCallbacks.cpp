#include "glfwCallbacks.h"

extern Camera g_camera;

void GLFWCALL keyCallback(const int key, const int action)
{
    if (action == GLFW_PRESS) {
        switch (key) {
            case 'W':
                g_camera.movement |= MOVE_FORWARD; break;
            case 'S':
                g_camera.movement |= MOVE_BACKWARD; break;
            case 'A':
                g_camera.movement |= MOVE_LEFT; break;
            case 'D':
                g_camera.movement |= MOVE_RIGHT; break;
            case '-':
                g_camera.movement |= MOVE_DOWN; break;
            case '=':
                g_camera.movement |= MOVE_UP; break;
            case 'H':
                g_camera.rotation |= LOOK_LEFT; break;
            case 'J':
                g_camera.rotation |= LOOK_DOWN; break;
            case 'K':
                g_camera.rotation |= LOOK_UP; break;
            case 'L':
                g_camera.rotation |= LOOK_RIGHT; break;
            default:
                break;
        }
    } else {
        switch (key) {
            case 'W':
                g_camera.movement &= ~MOVE_FORWARD; break;
            case 'S':
                g_camera.movement &= ~MOVE_BACKWARD; break;
            case 'A':
                g_camera.movement &= ~MOVE_LEFT; break;
            case 'D':
                g_camera.movement &= ~MOVE_RIGHT; break;
            case '-':
                g_camera.movement &= ~MOVE_DOWN; break;
            case '=':
                g_camera.movement &= ~MOVE_UP; break;
            case 'H':
                g_camera.rotation &= ~LOOK_LEFT; break;
            case 'J':
                g_camera.rotation &= ~LOOK_DOWN; break;
            case 'K':
                g_camera.rotation &= ~LOOK_UP; break;
            case 'L':
                g_camera.rotation &= ~LOOK_RIGHT; break;
            default:
                break;
        }
    }
}

template <typename T>
static inline T clamp(const T& x, const T& low, const T& high)
{
    return (x < low ? low : (x > high ? high : x));
}

template <typename T>
static inline T roll(const T& x, const T& low, const T& high)
{
    return (x < low ? high : (x > high ? low : x));
}

void GLFWCALL mousePosCallback(const int x, const int y)
{
    int w, h;
    glfwGetWindowSize(&w, &h);

    int midX = w * 0.5f;
    int midY = h * 0.5f;

    g_camera.rotX += y - midY;
    g_camera.rotY += x - midX;

    g_camera.rotX = clamp(g_camera.rotX, -90.0f, 90.0f);
    g_camera.rotY = roll(g_camera.rotY, -180.0f, 180.0f);

    glfwSetMousePos(midX, midY);
}

void GLFWCALL windowSizeCallback(int w, int h)
{
    glViewport(0, 0, w, h);
}
