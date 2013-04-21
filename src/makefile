GHC=ghc
FLAGS=-O2
OPT=-fllvm -optlo-O3 -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000
CORE=-ddump-simpl -ddump-to-file -dsuppress-all -dsuppress-uniques -fforce-recomp
PROF=-prof -auto-all
TARGET=Automata

all: automata

automata: callbacks simplex worldgen
	$(GHC) $(FLAGS) Automata.hs

callbacks:
	$(GHC) $(FLAGS) Callbacks.hs

simplex:
	$(GHC) $(FLAGS) $(OPT) Simplex.hs

worldgen: simplex
	$(GHC) $(FLAGS) $(OPT) WorldGen.hs

target:
	$(GHC) $(FLAGS) $(OPT) $(TARGET).hs

time: target
	./$(TARGET) +RTS -s

core:
	$(GHC) $(FLAGS) $(OPT) $(CORE) $(TARGET).hs

prof:
	$(GHC) $(FLAGS) $(OPT) $(PROF) $(TARGET).hs
	./$(TARGET) +RTS -p -xt -hy && hp2ps -e8in -c $(TARGET).hp

clean:
	rm -rf *.hi *.o *.prof *.dump-simpl *.aux *.hp *.ps $(TARGET)

.PHONY: clean
