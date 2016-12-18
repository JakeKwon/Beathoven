#!/bin/bash

cd ../bet_midi_library/
clang++-3.8 -O3 -Wall -Iinclude -std=c++11 -o betmidi src-programs/betmidi.cpp -Llib -lmidifile
./betmidi
