# Beathoven: tests for scanner makefile
# builds tokenize executeable for printing scanned tokens from stdin

default: test

all:
	cd ..; make all
	make

test: build
	./scanner_test.sh

build:
	cd scanner; make

.PHONY: clean
clean:
	cd scanner; make clean

