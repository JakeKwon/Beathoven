// clang -emit-llvm -o stdlib.bc -c stdlib.c
// clang -S -emit-llvm -c stdlib.c
#include <stdio.h>
#include "beathoven.h"

char _buffer[20];

string _print_pitch(pitch *p) {
    char c = '\0';
    if (p->alter == 1) c = '#';
    else if (p->alter == -1) c = 'b';
    sprintf(_buffer, "%s%d%c", p->key, p->octave, c);
    // printf("%s%d", p->key, p->octave);
    return _buffer;
}

// _duration d;
//
// duration _allocate_duration(int a, int b) {
//     return &d;
// }

string _str_of_duration(duration d) {
    sprintf(_buffer, "%d/%d", d->a, d->b);
    return _buffer;
}

/*
extern struct pitch p;
extern void f(struct pitch p);
*/
