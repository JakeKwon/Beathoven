default: build

all: clean build

build:
	cd src; make

test: build
	cd test; make

.PHONY: clean
clean:
	cd src; make clean
	cd test; make clean
