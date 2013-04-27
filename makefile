CC=g++
CFLAGS=-Wall -O2
LDFLAGS=-lGL -lGLEW -lglfw

SRCDIR=src
SOURCES=$(SRCDIR)/callbacks.cpp $(SRCDIR)/main.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=automata

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -c -o $@

clean:
	rm -rf $(SRCDIR)/*.o $(EXECUTABLE)

.PHONY: clean
