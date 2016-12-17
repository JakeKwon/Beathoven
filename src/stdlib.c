// clang -emit-llvm -o stdlib.bc -c stdlib.c
// clang -S -emit-llvm -c stdlib.c
#include <stdio.h>
#include "beathoven.h"

char _buffer[20];

string _str_of_pitch(pitch* _p) {
    const pitch p = *_p;
    char c = '\0';
    printf("%d\n", p->alter);
    if (p->alter == 1) c = '#';
    else if (p->alter == -1) c = 'b';
    sprintf(_buffer, "%c%d%c", p->key, p->octave, c);
    return _buffer;
}

// _duration d;
//
// duration _allocate_duration(int a, int b) {
//     return &d;
// }

string _str_of_duration(duration* _d) {
    const duration d = *_d;
    sprintf(_buffer, "%d/%d", d->a, d->b);
    return _buffer;
}

/*
extern struct pitch p;
extern void f(struct pitch p);
*/
