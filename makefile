GHC=ghc
FLAGS=-O2 -Odph -fllvm -optlo-O3 -fno-liberate-case
GLUT=-package GLUT
EXEC=Automata

all: automata

automata:
	$(GHC) $(FLAGS) $(GLUT) Automata.hs

clean:
	rm -rf *.hi *.o *.prof *.dump-simpl $(EXEC)

.PHONY: clean
