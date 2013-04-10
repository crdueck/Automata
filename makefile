GHC=ghc
FLAGS=-O2 -fllvm -optlo-O3 -fno-liberate-case -funbox-strict-fields
PROF=-prof -auto-all
GLUT=-package GLUT
TARGET=Automata

all: automata

automata:
	$(GHC) $(FLAGS) $(TARGET).hs

prof: automata
	./$(TARGET) +RTS -s

clean:
	rm -rf *.hi *.o *.prof *.dump-simpl *.aux *.hp *.ps $(TARGET)

.PHONY: clean
