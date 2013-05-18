#include <GL/glew.h>
#include <GL/glfw.h>
#include <cstdio>
#include <vector>

class glShaderProgram
{
public:
    glShaderProgram()
        : programID_(glCreateProgram())
    {}

    ~glShaderProgram()
    {
        glDeleteProgram(programID_);
    }

    void loadShaderSource(GLenum shaderType, const char *source);
    void link() { glLinkProgram(programID_); }
    void use() { glUseProgram(programID_); }

    GLint getUniformLocation(const char *name)
    {
        return glGetUniformLocation(programID_, name);
    }

private:
    GLuint programID_;
};

void glShaderProgram::loadShaderSource(GLenum shaderType, const char *source)
{
    FILE *fptr = fopen(source, "rb");
    if (!fptr) return;

    fseek(fptr, 0, SEEK_END);
    long length = ftell(fptr);
    fseek(fptr, 0, SEEK_SET);

    char *sourceBuf = new char[length + 1];
    fread(sourceBuf, length, 1, fptr);
    fclose(fptr);
    sourceBuf[length] = 0;

    GLuint shaderID = glCreateShader(shaderType);
    glShaderSource(shaderID, 1, (const char**)&sourceBuf, 0);
    delete[] sourceBuf;

    glCompileShader(shaderID);

    GLint compileStatus, logLength;
    glGetShaderiv(shaderID, GL_COMPILE_STATUS, &compileStatus);
    if (!compileStatus) {
        glGetShaderiv(shaderID, GL_INFO_LOG_LENGTH, &logLength);
        char *log = new char[logLength];
        glGetShaderInfoLog(shaderID, logLength, 0, log);
        printf("%s\n", log);
        delete[] log;
        glDeleteShader(shaderID);
        return;
    }

    glAttachShader(programID_, shaderID);
    glDeleteShader(shaderID);
}
