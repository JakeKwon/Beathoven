// clang -emit-llvm -o stdlib.bc -c stdlib.c
// clang -S -emit-llvm -c stdlib.c
#include <stdio.h>
#include "beathoven.h"

char buffer[20];

string _print_pitch(pitch *p) {
    char c = '\0';
    if (p->alter == 1) c = '#';
    else if (p->alter == -1) c = 'b';
    sprintf(buffer, "%s%d%c", p->key, p->octave, c);
    // printf("%s%d", p->key, p->octave);
    return buffer;
}

duration* _allocate_duration(int a, int b) {
    return NULL;
}

/*
extern struct pitch p;
extern void f(struct pitch p);
*/
