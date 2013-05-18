#include <GL/glew.h>
#include <GL/glfw.h>

#include <vector>

class glVertexArrayObject
{
public:
    glVertexArrayObject()
    {
        glGenVertexArrays(1, &vao_);
        glBindVertexArray(vao_);
    }

    ~glVertexArrayObject()
    {
        glDeleteBuffers(buffer_objs_.size(), &buffer_objs_[0]);
        glDeleteVertexArrays(1, &vao_);
    }

    void bind() { glBindVertexArray(vao_); }

    template <typename T>
    void bindBuffer(GLenum bufferType, size_t length, const T *buffer);

private:
    GLuint vao_;
    std::vector<GLuint> buffer_objs_;
};

template <typename T>
void glVertexArrayObject::bindBuffer(GLenum bufferType, size_t length, const T *buffer)
{
    GLuint bufferID;
    glGenBuffers(1, &bufferID);
    glBindBuffer(bufferType, bufferID);
    glBufferData(bufferType, length * sizeof(T), buffer, GL_STATIC_DRAW);
    buffer_objs_.push_back(bufferID);
}
