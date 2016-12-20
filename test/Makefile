# Beathoven: tests for scanner makefile
# builds tokenize executeable for printing scanned tokens from stdin

default: test

all:
	cd ..; make all
	make

test: build
	# cd scanner; make; ./scanner_test.sh
	# cd analyzer; ./testall.sh
	cd compiler; ./all_test.sh
test-only:
	cd compiler; ./all_test.sh

test-pass:
	cd compiler; ./pass_test.sh

test-fail:
	cd compiler; ./fail_test.sh

tests-pass: build
	cd compiler; ./pass_test.sh

tests-fail: build
	cd compiler; ./fail_test.sh

test-clear:
	cd compiler; ./clearlogs.sh

build:
	cd ..; make all

.PHONY: clean
clean:
	# cd scanner; make clean
	# cd compiler; make clean
