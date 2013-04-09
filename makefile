GHC=ghc
FLAGS=-O2 -fllvm -optlo-O3 -fno-liberate-case
PROF=-prof -auto-all
GLUT=-package GLUT
TARGET=Automata

all: automata

automata:
	$(GHC) $(FLAGS) $(GLUT) $(TARGET).hs

prof: automata
	$(GHC) $(PROF) -osuf $(TARGET).o $(TARGET).hs;
	./$(TARGET) +RTS -p -hy && hp2ps -e8in -c $(TARGET).hp

clean:
	rm -rf *.hi *.o *.prof *.dump-simpl *.aux *.hp *.ps $(TARGET)

.PHONY: clean
