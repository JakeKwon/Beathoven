#!/bin/bash

MYDIR="$(dirname "$(which "$0")")"
BEAT_FILE="$MYDIR/src/beathoven"
pwd
cp $MYDIR/src/stdlib.bc .

if [ ! -f $BEAT_FILE ]; then
    printf "ERROR: not yet compiled, run 'make' first.\n" 1>&2
    exit 1
fi

# beathoven.sh (-c ) <BEAT_FILE> <output_file>
if [ "$#" -eq 3 ]; then
    if [ "$1" == "-c" ]; then
        cat $LIST_LIB $DIST_LIB $2 | $BEAT_FILE $1 > $3
        exit 0
    else
        printf "ERROR: invalid arguments supplied for command $0 $1\n" 1>&2
        exit 1
    fi
fi

# beathoven.sh -s <BEAT_FILE>
if [ "$#" -eq 2 ] && [ "$1" == "-s" ]; then
    $BEAT_FILE $1 < $2
    exit 0
fi

# beathoven.sh -h
if [ "$#" -eq 1 ] && [ "$1" == "-h" ]; then
    printf """Beathoven Usage: beathoven.sh <flag> [input_file] [output_file]\n"
    printf "  -c\tCompile beathoven input_file to llvm code in output_file with stdlib\n"
    printf "  -h\tDisplay this list of options\n"
    printf "  -s\tPrint a json tree of our semantically checked abstract syntax tree\n"
    exit 1
fi

printf "ERROR: invalid arguments. Run $0 -h for usage instructions\n" 1>&2
exit 1
