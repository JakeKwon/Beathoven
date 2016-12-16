// clang -emit-llvm -o stdlib.bc -c stdlib.c
// clang -S -emit-llvm -c stdlib.c
#include <stdio.h>
#include "beathoven.h"

char buffer[20];

char* _print_pitch(struct pitch *p) {
    char c = '\0';
    if (p->alter == 1) c = '#';
    else if (p->alter == -1) c = 'b';
    sprintf(buffer, "%s%d%c", p->key, p->octave, c);
    // printf("%s%d", p->key, p->octave);
    return buffer;
}


/*
extern struct pitch p;
extern void f(struct pitch p);
*/
