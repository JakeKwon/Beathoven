#!/bin/bash
NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

INPUT_FILES="tests/*.in"
printf "\n${CYAN}Running Scanner Tests!\n${NC}"

for input_file in $INPUT_FILES; do
    output_file=${input_file/.in/.out}
    ./tokenize < $input_file | cmp -s $output_file -
    if [ "$?" -eq 0 ]; then
       printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $input_file..."
       printf "  - test using: ./tokenize < $input_file \n"
    else
       printf "%-65s ${RED}ERROR\n${NC}" "  - checking $input_file..." 1>&2
       printf "  - test using: ./tokenize < $input_file \n"
    fi
done

exit 0
