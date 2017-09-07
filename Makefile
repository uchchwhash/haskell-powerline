all: build clean

build:
	ghc --make -O2 -o haskell-powerline haskell-powerline.hs

build-static:
	ghc -O2 --make -static -optc-static -optl-static -optl-pthread -o haskell-powerline haskell-powerline.hs

clean:
	rm -rf *.hi *.o
