// clang -emit-llvm -o stdlib.bc -c stdlib.c
// clang -S -emit-llvm -c stdlib.c
#include <stdio.h>
#include "beathoven.h"

char _buffer[20];
int _pitch_values[7] = {0,2,4,5,7,9,11};


string _print_pitch(pitch *p) {
    char c = '\0';
    if (p->alter == 1) c = '#';
    else if (p->alter == -1) c = 'b';
    sprintf(_buffer, "%c%d%c", p->key, p->octave, c);
    // printf("%s%d", p->key, p->octave);
    return _buffer;
}

duration* _allocate_duration(int a, int b) {
    return NULL;
}

int _get_midi_pitch(pitch *p) {
  int note_number_index = 0;

  if( (int)(p->key) == 'A') note_number_index = 5;
  else if( (int)(p->key) == 'B') note_number_index = 6;
  else note_number_index = (int)(p->key) - (int)'C';


  int note_number = ((p->octave + 1)*12) + _pitch_values[note_number_index] + (p->alter);

  return note_number;

}



/*
extern struct pitch p;
extern void f(struct pitch p);
*/
