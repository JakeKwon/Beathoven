// clang -emit-llvm -o stdlib.bc -c stdlib.c

struct pitch {
    char key;
    int octave;
    int alter;
} p;

/*
int main() {
    return 0;
}
*/
