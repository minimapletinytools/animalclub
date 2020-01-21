all: ctest

stack: src package.yaml stack.yaml
	rm animalclub.cabal
	stack build
	find .stack-work/ -name 'libanimalclub.*' -exec cp {} ./ctest/ \;

ctest: stack
	cd ctest && make

test: stack
	cd ctest && make test

clean:
	stack clean && cd ctest && make clean

.PHONY: all clean
