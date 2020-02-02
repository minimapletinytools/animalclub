all: ctest

stack: src package.yaml stack.yaml
	rm animalclub.cabal
	stack build
	find .stack-work/ -name 'libanimalclub.*' -exec cp {} ./ctest/ \;

ctest: stack
	cd ctest && $(MAKE)

test: stack
	cd ctest && $(MAKE) test

clean:
	stack clean && cd ctest && make clean

.PHONY: all clean stack ctest test
