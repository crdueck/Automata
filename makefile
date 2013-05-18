CXX=g++
CXXFLAGS=-Wall -O2 -std=c++11
LDFLAGS=-lGL -lGLEW -lglfw

SRCDIR=src
SOURCES=glfwCallbacks.cpp main.cpp
OBJECTS=$(SOURCES:%.cpp=$(SRCDIR)/%.o)
EXECUTABLE=automata

$(EXECUTABLE): $(OBJECTS)
	$(CXX) $(OBJECTS) $(LDFLAGS) -o $@

.cpp.o:
	$(CXX) $(CXXFLAGS) -g $< -c -o $@

clean:
	rm -rf $(OBJECTS) $(EXECUTABLE)

.PHONY: clean
