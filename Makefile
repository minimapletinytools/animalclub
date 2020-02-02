all: ctest

clean_if_csrc_changes: csrc
	stack clean

stack: clean_if_csrc_changes src package.yaml stack.yaml
	stack build
	find .stack-work/ -name 'libanimalclub.*' -exec cp {} ./ctest/ \;

ctest: stack
	cd ctest && $(MAKE)

test: stack
	cd ctest && $(MAKE) test

clean:
	stack clean && cd ctest && make clean

.PHONY: all clean_if_csrc_changes clean stack ctest test
