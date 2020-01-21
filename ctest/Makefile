export ROOT_DIR=${PWD}
UNAME := $(shell uname)

# make sure you build parent project first or it won't find the libraries

test_all: test_all.cpp
# set linker path on Linux
ifeq ($(UNAME), Linux)
	g++ -g -Wall test_all.cpp -o $@ \
	-I../csrc \
	-Wl,-rpath='$$ORIGIN' \
	-lanimalclub \
	-L./
else
	g++ -g -Wall test_all.cpp -o $@ \
	-I../csrc \
	-lanimalclub \
	-L./
endif
# set dylib path on Mac
# I don't get it either, I just copy pasted from here https://medium.com/@donblas/fun-with-rpath-otool-and-install-name-tool-e3e41ae86172
ifeq ($(UNAME), Darwin)
	install_name_tool -add_rpath @executable_path/. test_all
endif


test: test_all
	./test_all

clean:
	rm -rf *.o *.a test_all

.PHONY: clean test test_all
