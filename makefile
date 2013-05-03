CXX=g++
CXXFLAGS=-Wall -O2
LDFLAGS=-lGL -lGLEW -lglfw

SRCDIR=src
SOURCES=glfwCallbacks.cpp glShaderProgram.cpp glVertexArrayObject.cpp main.cpp
OBJECTS=$(SOURCES:%.cpp=$(SRCDIR)/%.o)
EXECUTABLE=automata

$(EXECUTABLE): $(OBJECTS)
	$(CXX) $(OBJECTS) $(LDFLAGS) -o $@

.cpp.o:
	$(CXX) $(CXXFLAGS) -g $< -c -o $@

clean:
	rm -rf $(OBJECTS) $(EXECUTABLE)

.PHONY: clean
