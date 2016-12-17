#!/bin/bash

# This Runs all the things necessary to make it work
BREW="ruby -e '$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)' "
OPAM="brew install opam"
OPAMINIT="opam init"
LLVM="brew install homebrew/versions/llvm38"
CONFIG_ENV="opam config env"
CORE="opam install core"
LLVM38="opam install llvm.3.8"
YOJSON="opam install yojson"
SYMLINK="sudo ln -s /usr/local/opt/llvm38/bin/lli-3.8 /usr/bin/lli"

Install(){
echo "Running... $*"
 eval $*
}

# Install $BREW
Install $OPAM
Install $OPAMINIT
Install $LLVM
Install $CONFIG_ENV
Install $CORE
Install $LLVM38
Install $YOJSON
Install $SYMLINK

# declare -a toInstall=($BREW $OPAM)

# for thing in $toInstall
# do
#   echo $thing
# done
