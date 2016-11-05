#!/bin/bash

MYDIR="$(dirname "$(which "$0")")"
BEAT_FILE="$MYDIR/src/beathoven"
# LIST_LIB="$MYDIR/src/lib/list.bet"
# DIST_LIB="$MYDIR/src/lib/dist.bet"
# C_LIB="$MYDIR/src/lib/core.c"

if [ ! -f $BEAT_FILE ]; then
    printf "ERROR: not yet compiled, run 'make' first.\n" 1>&2
    exit 1
fi

# beathoven.sh (-c | -r) <BEAT_FILE> <output_file>
if [ "$#" -eq 3 ]; then
    if [ "$1" == "-c" ]; then
        cat $LIST_LIB $DIST_LIB $2 | $BEAT_FILE $1 $3 $PY_LIB
        exit 0
    elif [ "$1" == "-r" ]; then
        $BEAT_FILE $1 $3 < $2
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
    $BEAT_FILE -h
    exit 0
fi

printf "ERROR: invalid arguments. Run $0 -h for usage instructions\n" 1>&2
exit 1
