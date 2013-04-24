CC=g++ -Wall
CFLAGS=-O2
LIB=-lGL -lGLEW -lglfw

EXEC=automata

all: main

main:
	$(CC) $(CFLAGS) main.cpp $(LIB) -o $(EXEC)

clean:
	rm -rf $(EXEC)

.PHONY: clean
