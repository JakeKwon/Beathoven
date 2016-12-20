// clang -emit-llvm -o stdlib.bc -c stdlib.cpp
// clang -S -emit-llvm -c stdlib.cpp
#include <stdlib.h>
#include "beathoven.h"
#include <vector>
#include <string>

char _buffer[20];
// std::vector<int> v;
//std::string str;

ptr_t get_arr_ptr() {
    // return (ptr_t)(&v);
}

/*
string _str_of_Note(Note *note) { // cannot pass the whole struct as parameter
    string _buffer = (string) malloc(14); // garbage!
    pitch p = note->p;
    duration d = note->d;
    if (p->alter == 1)
        sprintf(_buffer, "%c%d#:%d/%d", p->key, p->octave, d->a, d->b);
    else if (p->alter == -1)
        sprintf(_buffer, "%c%d#:%d/%d", p->key, p->octave, d->a, d->b);
    else sprintf(_buffer, "%c%d:%d/%d", p->key, p->octave, d->a, d->b);
    return _buffer;
}
*/
