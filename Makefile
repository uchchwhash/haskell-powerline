all: build clean

build:
	ghc --make -O2 -o powerline Main.hs

build-static:
	ghc -O2 --make -static -optc-static -optl-static -optl-pthread -o powerline Main.hs

clean:
	rm -rf *.hi *.o
