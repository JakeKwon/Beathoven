typedef char * string;

typedef struct pitch {
    char* key;
    int octave;
    int alter;
} pitch;

// typedef _pitch * pitch;

typedef struct _duration {
    int a;
    int b;
} _duration;

typedef _duration * duration;
