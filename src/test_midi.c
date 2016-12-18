#include <stdio.h>
#include <stdlib.h>
#include "beathoven.h"


int main(){

// testing note ------------------------------------

_pitch pitch_struct;

pitch_struct.key = 'C';
pitch_struct.octave = 4;
pitch_struct.alter = 1;

_duration duration_struct;

duration_struct.a = 1;
duration_struct.b = 4;

pitch test_pitch = &pitch_struct;
duration test_duration = &duration_struct;

Note test_note;
test_note.p = test_pitch;
test_note.d = test_duration;

printf("Note duration: %.4f \n", ((double)test_note.d->a) / test_note.d->b);
printf("Note pitch: %c %d \n", test_note.p->key, test_note.p->octave);

// test get midi pitch function
int final_pitch_number = _get_midi_pitch(test_pitch);

char str_number_pitch[10];
sprintf(str_number_pitch,"%d\n", final_pitch_number);
puts(str_number_pitch);


// make and test sequence, 60 - 62 - 52 - 47 - 48, C4 - D4 - E4 - B3 - C4

FILE *file_pointer;
char sentenc[1000];
file_pointer = fopen("/Users/manubete/Desktop/plt/Beathoven/bet_midi_library/midi_text.txt","w");

if(file_pointer == NULL){
    printf("Error! \n");
    exit(1);
}

char pitches[5] = {'C','D','E','B','C'};
int octaves[5] = {4,4,4,3,4};

int midi_pitches[5];
float midi_duration[5];

_pitch seq_pitch_struct;
_duration seq_duration_struct;

pitch seq_test_pitch = &seq_pitch_struct;
duration seq_test_duration = &seq_duration_struct;

Note seq_test_note;
int i;

  for( i = 0; i < 5; i++ ){
    seq_pitch_struct.key = pitches[i];
    seq_pitch_struct.octave = octaves[i];
    seq_pitch_struct.alter = 0;


    seq_duration_struct.a = 1;
    seq_duration_struct.b = 4;


    seq_test_note.p = seq_test_pitch;
    seq_test_note.d = seq_test_duration;

    midi_pitches[i] = _get_midi_pitch(seq_test_note.p);
    midi_duration[i] = (double)seq_test_note.d->a;

    printf("midi duration: %.2f \n", midi_duration[i]);
    printf("Midi note pitch: %d \n", midi_pitches[i]);

  }

  // write all pitches
  for( i = 0; i < 5; i++){
    fprintf(file_pointer, "%d,", midi_pitches[i]);
  }
    fprintf(file_pointer, "-1\n");

  // write all durations
  for( i = 0; i < 5; i++){
    fprintf(file_pointer, "%f,", midi_duration[i]);
  }
    fprintf(file_pointer, "-1\n");
// Note c4_seq_test_note_ii;
// c4_seq_test_note_ii.p = c4_test_pitch;


// // D4
// _pitch d4_seq_pitch_struct;

// d4_seq_pitch_struct.key = 'D';
// d4_seq_pitch_struct.octave = 4;
// d4_seq_pitch_struct.alter = 0;

// _duration c4_seq_duration_struct;

// c4_seq_duration_struct.a = 1;
// c4_seq_duration_struct.b = 4;

// pitch test_pitch = &d4_seq_pitch_struct;
// duration test_duration = &c4_seq_duration_struct;

// Note c4_seq_test_note;
// c4_seq_test_note.p = test_pitch;
// c4_seq_test_note.d = test_duration;



}
