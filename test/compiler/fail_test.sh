#!/bin/bash
# Tests specific error messages we should be generating
# Add the code to test/compiler/fail


NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

INPUTS="fail/*.bt"
TMP_FILE=$(mktemp "compiled.XXXXX")
printf "\n\n${CYAN} Running Compiler Fail Tests! ${NC}"

for infile in $INPUTS; do
    outfile=${infile/.bet/.out}

    ../../beathoven.sh -r $infile $TMP_FILE 2>&1 | cmp -s $output_file -
    if [ "$?" -eq 0 ]; then
        printf "Running: ../../beathoven.sh -r $infile $TMP_FILE 2>&1 | cmp -s $output_file"
        printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $input_file..."
    else
        printf "Running: ../../beathoven.sh -r $infile $TMP_FILE 2>&1 | cmp -s $output_file"
        printf "%-65s ${RED}ERROR\n${NC}" "  - checking $input_file..." 1>&2
        rm -f $TMP_FILE
        exit 1
    fi

done

rm -f $TMP_FILE
exit 0
