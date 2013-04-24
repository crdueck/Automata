CC=g++ -Wall
CFLAGS=-O2
LDFLAGS=-lGL -lGLEW -lglfw
SOURCES=callbacks.cpp main.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=automata

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -c -o $@

clean:
	rm -rf $(OBJECTS) $(EXECUTABLE)

.PHONY: clean
