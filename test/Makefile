
default: test

all:
	cd ..; make all
	make

test: build
	./scanner_test.sh
	./paser_test.sh
	./pass_test.sh
	./fail_test.sh

build:
	cd scanner; make
	cd parser; make

.PHONY: clean
clean:
	cd scanner; make clean
	cd parser; make clean
