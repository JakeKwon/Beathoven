default: build

all: clean build

build:
	cd src; make

test: clean
	cd test; make

tests:
	cd test; make test-only

test-fail:
	cd test; make test-fail

test-pass:
	cd test; make test-pass

.PHONY: clean
clean:
	cd src; make clean
	cd test; make clean
