default: build

all: clean build

build:
	cd src; make

test: clean
	cd test; make

tests:
	cd test; make test-only

.PHONY: clean
clean:
	cd src; make clean
	cd test; make clean
