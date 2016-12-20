w#!/bin/bash
NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

# Tests specific outputs we should be generating
# Add the code to test/compiler/pass

INPUTS="pass/*.bt"
LLI="lli-3.8"
BEAT="../../src/beathoven"

printf "${CYAN}####  Running Compiler pass Tests!  ####${NC}\n\n"

# Set time limit for all operations
ulimit -t 30

globallog=logs/globallog.log
rm -f logs/globallog.log
rm -f logs/*.ll
rm -f logs/*.out
rm -f logs/*.err
rm -f logs/*.diff
error=0
globalerror=0
totalfiles=0
totalerrors=0
one=1

keep=0

Usage() {
    echo "Usage: ./pass_test.sh"
    exit 1
}

LLIFail() {
  echo "Could not find the LLVM interpreter \"$LLI\"."
  echo "Check your LLVM installation and/or modify the LLI variable in pass_test.sh"
  exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
  printf "${RED}FAILURE${NC}"
  error=1
  totalerrors=$(($totalerrors + $one))
    fi
    echo "  $1"
}


which "$LLI" >> $globallog || LLIFail

# Run <args>
# Report the command, run it, and report any errors
Run() {
    printf "Running... $* \n"
    echo $* 1>&2
    eval $*
    #     SignalError "$1 failed on $*"
    #     return 1
    # }
}

RunFail() {
    printf "Running... $* \n"
    echo $* 1>&2
    eval $*
  #   && {
  # SignalError "failed: $* did not report an error"
  # return 1
  #   }
  #   return 0
}

Compare() {
    # generatedfiles="$generatedfiles $3"
    printf "Comparing... $* \n"
    printf "diff -b $1 $2 > $3\n"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
        SignalError "$1 differs. See globallog.log file for breakdown."
        echo "FAILED $1 differs from $2" 1>&2
    }
}


Check(){
  error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.bt//'`
    reffile=`echo $1 | sed 's/.bt$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    printf "\n${CYAN}Running Pass Test: $basename ${NC}\n"

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    # generatedfiles=""

    # generatedfiles="$generatedfiles ${basename}.ll ${basename}.out"
    # printf "$BEAT $1 $TMP_LLI_FILE \n"
    # printf "$LLI $TMP_LLI_FILE > $TMP_OUT_FILE \n"

    # Run "$MICROC" "<" $1 ">" "${basename}.ll" &&
    # Run "$LLI" "${basename}.ll" ">" "${basename}.out" &&
    Run "$BEAT" "<" $1 ">" "$TMP_LLI_FILE"
    Run "$LLI" "$TMP_LLI_FILE" ">" "$TMP_OUT_FILE"
    # printf $TMP_OUT_FILE ${reffile}.out logs/${basename}.diff
    Compare "$TMP_OUT_FILE" ${reffile}.out logs/${basename}.diff
    # printf "\nRunning $BEAT < $1 ${basename}.ll > ${basename}.ll\n"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
  if [ $keep -eq 0 ] ; then
      rm -f $generatedfiles
  fi
  printf "${GREEN}SUCCESS${NC}\n"
  echo "###### OK" 1>&2
    else
  echo "###### FAILED" 1>&2
  globalerror=$error
    fi
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.bt//'`
    reffile=`echo $1 | sed 's/.bt$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    printf "\n${CYAN}Running Fail Test: $basename ${NC}\n"

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    # generatedfiles=""

    # generatedfiles="$generatedfiles ${basename}.ll ${basename}.out"
    # printf "$BEAT $1 $TMP_LLI_FILE \n"
    # printf "$LLI $TMP_LLI_FILE > $TMP_OUT_FILE \n"
    # to llvm
    # Run "$BEAT" "$1" "$TMP_LLI_FILE" original
    RunFail "$BEAT" "<" $1 "2>" "$TMP_ERR_FILE" ">>" $globallog
    eval "head -3" $TMP_ERR_FILE ">" "TEMPORARY"
    eval "cp TEMPORARY " $TMP_ERR_FILE
    rm TEMPORARY
    # rm "TEMPORARY"
    # Run "$LLI" "$TMP_LLI_FILE" "2>" "$TMP_OUT_FILE"
    Compare "$TMP_ERR_FILE" ${reffile}.err logs/${basename}.diff

    # generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
    # RunFail "$MICROC" "<" $1 "2>" "${basename}.err" ">>" $globallog &&
    # Compare ${basename}.err ${reffile}.err ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
  if [ $keep -eq 0 ] ; then
      rm -f $generatedfiles
  fi
  printf "${GREEN}SUCCESS${NC}\n"
  echo "###### OK" 1>&2
    else
  echo "###### FAILED" 1>&2
  globalerror=$error
    fi
}

files="pass/*.bt"

for file in $files
do
  # printf ${file:5}
  totalfiles=$(($totalfiles + $one))
  TMP_LLI_FILE=$(mktemp "logs/${file:5}.ll")
  TMP_OUT_FILE=$(mktemp "logs/${file:5}.out")

  # TMP_LLI_FILE= $(mktemp "ll.XXXX")
  # TMP_OUT_FILE= $(mktemp "out.XXXX")

  Check $file 2>> $globallog
done
printf "You have $totalerrors out of $totalfiles pass errors"
printf "\n\n${CYAN}####  End of Pass Compiler Tests!  ####${NC}"


# printf "\n\n${CYAN}####  Starting Fail Compiler Tests!  ####${NC}"
# failfiles="fail/*.bt"

# for file in $failfiles
# do
#   # printf ${file:5}
#   totalfiles=$(($totalfiles + $one))
#   TMP_LLI_FILE=$(mktemp "logs/${file:5}.ll")
#   TMP_ERR_FILE=$(mktemp "logs/${file:5}.err")

#   # TMP_LLI_FILE= $(mktemp "ll.XXXX")
#   # TMP_OUT_FILE= $(mktemp "out.XXXX")

#   CheckFail $file 2>> $globallog
# done
# printf "\n\n${CYAN}####  End of Fail Compiler Tests!  ####${NC}"

