/*
 * Authors:
 *  - Rodrigo Manubens
 *  - Ruonan Xu
 */

// clang -emit-llvm -o stdlib.bc -c stdlib.c
// clang -S -emit-llvm -c stdlib.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include "beathoven.h"

char _buffer[20];

int _pitch_values[7] = {0,2,4,5,7,9,11};


int len(ptr_t arr_struct_p) {
    return (int)(*((size_t *) arr_struct_p));
}

string _str_of_pitch(pitch p) {
    string _buffer = malloc(4); // garbage!
    char c = '\0';
    if (p->alter == 1) c = '#';
    else if (p->alter == -1) c = 'b';
    sprintf(_buffer, "%c%d%c", p->key, p->octave, c);

    return _buffer;
}


string _str_of_duration(duration d) {
    string _buffer = malloc(10); // garbage!
    sprintf(_buffer, "%d/%d", d->a, d->b);
    return _buffer;
}

string _str_of_Note(Note *note) { // cannot pass the whole struct as parameter
    string _buffer = malloc(14); // garbage!
    pitch p = note->p;
    duration d = note->d;
    if (p->alter == 1)
        sprintf(_buffer, "%c%d#:%d/%d", p->key, p->octave, d->a, d->b);
    else if (p->alter == -1)
        sprintf(_buffer, "%c%d#:%d/%d", p->key, p->octave, d->a, d->b);
    else sprintf(_buffer, "%c%d:%d/%d", p->key, p->octave, d->a, d->b);
    return _buffer;
}

void _write_sequence_midi_text(Seq input_sequence, int seqi){

  int midi_pitches[input_sequence.len];
  float midi_durations[input_sequence.len];

  FILE *file_pointer;
  char sentenc[1000];
  char cwd[1000];
  if (getcwd(cwd, sizeof(cwd)) != NULL) {
      fprintf(stdout, "Current working dir: %s\n", cwd);
      strcat(cwd, "/../bet_midi_library/file_example.txt");
  }
   else
       perror("getcwd() error");

  if (seqi == 0) file_pointer = fopen(cwd, "w");
  else file_pointer = fopen(cwd, "a");

  if(file_pointer == NULL){
      printf("Error! \n");
      exit(1);
  }

  int i;

  for(i=0; i < input_sequence.len; i++){
    midi_pitches[i] = _get_midi_pitch(input_sequence.arr[i].p);
    midi_durations[i] = 4.0 * (input_sequence.arr[i].d->a) / input_sequence.arr[i].d->b;
  }

  for(i=0; i < input_sequence.len; i++){
    fprintf(file_pointer, "%d,", midi_pitches[i]);
  }
  fprintf(file_pointer, "-1\n");

  for(i=0; i < input_sequence.len; i++){
    fprintf(file_pointer, "%.2f,", midi_durations[i]);
  }
  fprintf(file_pointer, "-1\n");

  fclose(file_pointer);

}

void _make_midi_from_midi_text(){
  const char * script = "./betmidi.sh";
  system(script);
}

void render_seqs_as_midi(int num, ...) {
    va_list valist;
    int i;
    /* initialize valist for num number of arguments */
    va_start(valist, num);

    /* access all the arguments assigned to valist */
    for (i = 0; i < num; i++) {
        _write_sequence_midi_text(*(va_arg(valist, Seq *)), i);
    }

    /* clean memory reserved for valist */
    va_end(valist);

    _make_midi_from_midi_text();
}

void render_as_midi(Seq * input_sequence){
    _write_sequence_midi_text(*input_sequence, 0);
    _make_midi_from_midi_text();
}

int _get_midi_pitch(pitch p) {
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
