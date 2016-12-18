// clang -emit-llvm -o stdlib.bc -c stdlib.c
// clang -S -emit-llvm -c stdlib.c
#include <stdio.h>
#include <stdlib.h>
#include "beathoven.h"

char _buffer[20];

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

/*
extern struct pitch p;
extern void f(struct pitch p);
*/
