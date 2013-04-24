#include <glm/glm.hpp>

#include "callbacks.hpp"

extern Camera g_camera;

void GLFWCALL keyCallback(int key, int action)
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
            default:
                break;
        }
    }
}

double lastTime;

void GLFWCALL mousePosCallback(int x, int y)
{
    int w, h;
    glfwGetWindowSize(&w, &h);

    int midX = w * 0.5f;
    int midY = h * 0.5f;

    double currentTime = glfwGetTime();
    double dt = currentTime - lastTime;
    lastTime = currentTime;

    g_camera.rotX += 10.0f * dt * (y - midY);
    g_camera.rotY += 10.0f * dt * (x - midX);

    glm::clamp(g_camera.rotX, -90.0f, 90.0f);
    glm::clamp(g_camera.rotY, -180.0f, 180.0f);

    glfwSetMousePos(midX, midY);
}

void GLFWCALL windowSizeCallback(int w, int h)
{
    glViewport(0, 0, w, h);
}
