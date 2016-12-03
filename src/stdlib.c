// clang -emit-llvm -o stdlib.bc -c stdlib.c

struct _pitch {
    char key;
    int octave;
    int alter;
};

/*
extern void f(struct pitch p);

void g() {
    f(p);
}
int main() {
    return 0;
}
*/
