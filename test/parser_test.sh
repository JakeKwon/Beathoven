#!/bin/bash
NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'
pass=0
fail=0
errorOut=errors.log
excpTestFlag=0
testOption=$1 #stores the test flag since functions can't see the $1
vFlag=$2 #stores the -v


INPUTS="scanner/*.in"
printf "${CYAN} Running Scanner Tests!${NC}"

createBet(){
  echo "Compiling bet executable"
  cd ..
  make clean 2>&1 > /dev/null
  make
  #cp dice ../Test\ Suite/Hello_World_Demo/dice
  # cd Test\ Suite
  echo "Compilation of bet executable complete"
}

test_function() {
  for infile in $INPUTS; do
    filename=$(basename "$infile")

  echo "==================================" >> session_file
  echo "Testing: $filename" >> session_file
      outfile=${infile/.in/.out}
      scanner/tokenize < $infile | cmp -s $outfile -
      if [ "$?" -eq 0]; then
          printf "%-65s ${GREEN}SUCCESS\n${NC}" " - checking $infile..."
      else
          printf "%-65s ${RED}ERROR\n${NC}" " - checking $infile..." 1>&2
          exit 1
      fi
  done

}


exit 0
