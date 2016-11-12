#!/bin/bash
NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

# Tests specific outputs we should be generating
# Add the code to test/compiler/pass

INPUTS="scanner/*.in"
TMP_FILE=$(mktemp "compiled.XXXXX")
printf "${CYAN} Running compiler pass Tests!${NC}"

for infile in $INPUTS; do
    outfile=${infile/.in/.out}
    # llvm_file=${infile/.bet/.ll}

    # compile odds program to temp python file
    # will not work because we haven't compiled into raw LL yet
    ../beathoven.sh -r $infile $TMP_FILE

    # # if ll file exists compare
    # if [ -e "$llvm_file" ]; then
    #     cmp -s $llvm_file $TMP_FILE
    #     if [ "$?" -ne 0 ]; then
    #         printf "%-65s ${RED}ERROR\n${NC}" "  - checking $llvm_file..." 1>&2
    #         rm -f $TMP_FILE
    #         exit 1
    #     fi
    # fi

    # if test output file exists, compare compiled output to it
    if [ -e "$outfile" ]; then
        python $TMP_FILE | cmp -s $outfile -
        if [ "$?" -ne 0 ]; then
            printf "%-65s ${RED}ERROR\n${NC}" "  - checking $outfile..." 1>&2
            rm -f $TMP_FILE
            exit 1
        fi
    fi
printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $infile..."
done

exit 0