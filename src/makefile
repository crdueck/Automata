GHC=ghc
FLAGS=-O2 -threaded -fllvm -optlo-O3 -funbox-strict-fields -fexcess-precision
PROF=-prof -auto-all
GLUT=-package GLUT
TEST=Test
TARGET=Automata

all: target

target:
	$(GHC) $(FLAGS) $(TARGET).hs

run: target
	./$(TARGET)

time: target
	./$(TARGET) +RTS -s

test:
	$(GHC) $(FLAGS) $(TEST).hs
	./$(TEST) +RTS -N3 -s && sxiv out.png
	rm out.png

prof:
	$(GHC) $(FLAGS) $(PROF) $(TARGET).hs
	./$(TARGET) +RTS -p -xt -hy && hp2ps -e8in -c $(TARGET).hp

clean:
	rm -rf *.hi *.o *.prof *.dump-simpl *.aux *.hp *.ps $(TARGET) $(TEST)

.PHONY: clean
