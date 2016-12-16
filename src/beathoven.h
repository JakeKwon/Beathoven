typedef char * string;

typedef struct pitch {
    char* key;
    int octave;
    int alter;
} pitch;

typedef struct duration {
    int a;
    int b;
} duration;
