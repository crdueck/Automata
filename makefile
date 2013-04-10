GHC=ghc
FLAGS=-O2 -fllvm -optlo-O3 -funbox-strict-fields -fexcess-precision
PROF=-prof -auto-all
GLUT=-package GLUT
TARGET=Simplex

all: target

<<<<<<< HEAD
target:
=======
automata:
>>>>>>> a9a449adff11748710caba250fa6e87fbaf0f751
	$(GHC) $(FLAGS) $(TARGET).hs

time: target
	./$(TARGET) +RTS -s

prof:
	$(GHC) $(FLAGS) $(PROF) $(TARGET).hs
	./$(TARGET) +RTS -p -hy && hp2ps -e8in -c $(TARGET).hp

clean:
	rm -rf *.hi *.o *.prof *.dump-simpl *.aux *.hp *.ps $(TARGET)

.PHONY: clean
