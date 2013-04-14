GHC=ghc
FLAGS=-O2 -fllvm -optlo-O3 -threaded
OPTFLAGS=-Odph -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000
PROF=-prof -auto-all
GLUT=-package GLUT
TEST=Test
TARGET=Simplex

all: target

target:
	$(GHC) $(FLAGS) $(OPTFLAGS) $(TARGET).hs

run: target
	./$(TARGET)

time: target
	./$(TARGET) +RTS -s

test:
	$(GHC) $(FLAGS) $(TEST).hs
	./$(TEST) +RTS -N3 -s && sxiv out.png
	rm out.png

prof:
	$(GHC) $(FLAGS) $(OPTFLAGS) $(PROF) $(TARGET).hs
	./$(TARGET) +RTS -p -xt -hy && hp2ps -e8in -c $(TARGET).hp

clean:
	rm -rf *.hi *.o *.prof *.dump-simpl *.aux *.hp *.ps $(TARGET) $(TEST)

.PHONY: clean
