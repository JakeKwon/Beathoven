w#!/bin/bash
NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

# Tests specific outputs we should be generating
# Add the code to test/compiler/pass

INPUTS="pass/*.bt"
LLI="lli"
BEAT="../../beathoven.sh -c"

# Set time limit for all operations
ulimit -t 30
helperPrint=false
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



which "$LLI" >> $globallog || LLIFail

# Run <args>
# Report the command, run it, and report any errors
Run() {
    if [ $helperPrint -eq 1 ] ; then
      printf "Running... $* \n"
    fi
    echo $* 1>&2
    eval $*
    #     SignalError "$1 failed on $*"
    #     return 1
    # }
}

RunFail() {
    if [ $helperPrint -eq 1 ] ; then
      printf "Running... $* \n"
    fi
    echo $* 1>&2
    eval $*
  #   && {
  # SignalError "failed: $* did not report an error"
  # return 1
  #   }
  #   return 0
}

SignalError() {
    if [ $error -eq 0 ] ; then
  printf "\n${RED}FAILURE${NC}\t"
  error=1
  totalerrors=$(($totalerrors + $one))
    fi
    echo "$1"
}

Compare() {
    # generatedfiles="$generatedfiles $3"
    if [ $helperPrint -eq 1 ] ; then
      printf "Comparing... $* \n"
    fi

    diff -b "$1" "$2" > "logs/$3" 2>&1 || {
        echo diff -b $1 $2 "> logs/" $3 1>&2
        SignalError "Output differs. See $3"
        diff -b "$1" "$2" > "diffs/$3" 2>&1
        echo "FAILED $1 differs from $2" 1>&2
    }
}


Check(){
  error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.bt//'`
    reffile=`echo $1 | sed 's/.bt$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    printf "\n${CYAN}Running Pass Test: $basename ${NC}\t\t"

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    # generatedfiles=""

    # generatedfiles="$generatedfiles ${basename}.ll ${basename}.out"
    # printf "$BEAT $1 $TMP_LLI_FILE \n"
    # printf "$LLI $TMP_LLI_FILE > $TMP_OUT_FILE \n"

    # Run "$MICROC" "<" $1 ">" "${basename}.ll" &&
    # Run "$LLI" "${basename}.ll" ">" "${basename}.out" &&
    Run $BEAT $1 $TMP_LLI_FILE
    Run "$LLI" "$TMP_LLI_FILE" ">" "$TMP_OUT_FILE"
    # printf $TMP_OUT_FILE ${reffile}.out logs/${basename}.diff
    Compare "$TMP_OUT_FILE" ${reffile}.out ${basename}.diff
    # printf "\nRunning $BEAT < $1 ${basename}.ll > ${basename}.ll\n"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
  if [ $keep -eq 0 ] ; then
      rm -f $generatedfiles
  fi
  printf "${GREEN}SUCCESS${NC}"
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

    printf "\n${CYAN}Running Fail Test: $basename ${NC}\t"

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    # generatedfiles=""

    # generatedfiles="$generatedfiles ${basename}.ll ${basename}.out"
    # printf "$BEAT $1 $TMP_LLI_FILE \n"
    # printf "$LLI $TMP_LLI_FILE > $TMP_OUT_FILE \n"
    # to llvm
    # Run "$BEAT" "$1" "$TMP_LLI_FILE" original
#     RunFail "$BEAT" "<" $1 "2>" "$TMP_ERR_FILE" ">>" $globallog

    RunFail $BEAT $1 "$TMP_ERR_FILE" "2>" "$TMP_ERR_FILE" ">>" $globallog
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
  printf "${GREEN}SUCCESS${NC}"
  echo "###### OK" 1>&2
    else
  echo "###### FAILED" 1>&2
  globalerror=$error
    fi
}

files="pass/*.bt"
printf "${CYAN}####  Running Compiler pass Tests!  ####${NC}\n\n"

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
printf "\nYou have $totalerrors out of $totalfiles PASS errors"
printf "\n\n${CYAN}####  End of Pass Compiler Tests!  ####${NC}"


