#!/bin/bash
NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

# Tests specific outputs we should be generating
# Add the code to test/compiler/pass

INPUTS="pass/*.bt"
LLI="lli"
BEAT="../../beathoven.sh -c "

printf "\n\n${CYAN} Running Compiler pass Tests!${NC}"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
rm -f lli.*
rm -f out.*
error=0
globalerror=0

keep=0

LLIFail() {
  echo "Could not find the LLVM interpreter \"$LLI\"."
  echo "Check your LLVM installation and/or modify the LLI variable in testall.sh"
  exit 1
}

which "$LLI" >> $globallog || LLIFail

Run() {
    echo $* 1>&2
    eval $* || {
  SignalError "$1 failed on $*"
  return 1
    }
}

Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
  SignalError "$1 differs"
  echo "FAILED $1 differs from $2" 1>&2
    }
}

Check(){
  error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.bt//'`
    reffile=`echo $1 | sed 's/.bt$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.ll ${basename}.out" &&
    printf "../../beathoven.sh -c $infile $TMP_LLI_FILE > $TMP_LLI_FILE"
    # to llvm
    ../../beathoven.sh -c $1 $TMP_LLI_FILE > $TMP_LLI_FILE
    Run "$LLI" "$TMP_LLI_FILE" ">" "$TMP_OUT_FILE"
    # Run "$BEAT" "<" $1 ${basename}.ll ">" "${basename}.ll" &&
    # Run "$LLI" "${basename}.ll" ">" "${basename}.out" &&
    Compare "$TMP_OUT_FILE" ${reffile}.out ${basename}.diff
    # printf "\nRunning $BEAT < $1 ${basename}.ll > ${basename}.ll\n"

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
  if [ $keep -eq 0 ] ; then
      rm -f $generatedfiles
  fi
  echo "OK"
  echo "###### SUCCESS" 1>&2
    else
  echo "###### FAILED" 1>&2
  globalerror=$error
    fi
}

files="pass/*.bt"

for file in $files
do
  # TMP_LLI_FILE=$(mktemp "lli.XXXXX")
  # TMP_OUT_FILE=$(mktemp "out.XXXXX")

  TMP_LLI_FILE=$file.lli
  TMP_OUT_FILE=$file.out

  Check $file 2>> $globallog
done

# for infile in $INPUTS; do
#     outfile=${infile/.bt/.out}
#     llvm_file=${infile/.bt/.ll}

#     # compile odds program to temp python file
#     # will not work because we haven't compiled into raw LL yet
#     ../../beathoven.sh -c $infile $TMP_FILE > $TMP_FILE
#     printf "\nTesting $infile\n"
#     # if ll file exists compare
#     if [ -e "$llvm_file" ]; then
#         cmp -s $llvm_file $TMP_FILE
#         printf $llvm_file
#         if [ "$?" -ne 0 ]; then
#             printf "%-65s ${RED}ERROR\n${NC}" "  - checking $llvm_file..." 1>&2
#             rm -f $TMP_FILE
#             exit 1
#         fi
#     fi

#     # # if test output file exists, compare compiled output to it
#     # if [ -e "$outfile" ]; then
#     #     cat $TMP_FILE | cmp -s $outfile -
#     #     if [ "$?" -ne 0 ]; then
#     #         printf "../../beathoven.sh -c $infile $TMP_FILE"
#     #         printf "%-65s ${RED}ERROR\n${NC}" "  - checking $outfile..." 1>&2
#     #         rm -f $TMP_FILE
#     #         exit 1
#     #     fi
#     # fi
# printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $infile..."
# done

# exit 0