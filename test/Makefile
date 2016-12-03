# Beathoven: tests for scanner makefile
# builds tokenize executeable for printing scanned tokens from stdin

default: test

all:
	cd ..; make all
	make

test: build
	# cd scanner; make; ./scanner_test.sh
	# cd analyzer; ./testall.sh
	cd compiler; ./pass_test.sh
test-only:
	cd compiler; ./pass_test.sh
build:
	cd ..; make all

.PHONY: clean
clean:
	# cd scanner; make clean
	# cd compiler; make clean
