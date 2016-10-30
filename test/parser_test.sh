#!/bin/bash
NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

INPUTS="scanner/*.in"
printf "${CYAN} Running Scanner Tests!${NC}"

for infile in $INPUTS; do
    outfile=${infile/.in/.out}
    scanner/tokenize < $infile | cmp -s $outfile -
    if [ "$?" -eq 0]; then
        printf "%-65s ${GREEN}SUCCESS\n${NC}" " - checking $infile..."
    else
        printf "%-65s ${RED}ERROR\n${NC}" " - checking $infile..." 1>&2
        exit 1
    fi
done

exit 0
