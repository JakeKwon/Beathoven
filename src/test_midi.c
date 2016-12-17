#include <stdio.h>
#include "beathoven.h"


int main(){

pitch pitch_struct;

pitch* test_pitch = &pitch_struct;

test_pitch->key = 'C';
test_pitch -> octave = 4;
test_pitch -> alter = 1;

int final_note_number = _get_midi_pitch(test_pitch);

char str_number_note[10];
sprintf(str_number_note,"%d\n", final_note_number);
puts(str_number_note);
}
