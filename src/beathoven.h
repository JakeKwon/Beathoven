/*
 * Authors:
 *  - Ruonan Xu
 */

/*
1. Structs with names starting with '_' are invisible to users.
2. struct Part and its fields are visible to users.
3. This language shouldn't be implemented in LLVM, not even C.
*/

/* Basic types */

typedef char * string;

// Arraytype(Int)
typedef struct _Arr_int {
    int len;
    int* arr;
} _Arr_int;


/* Basic music types */

// Musictype(Pitch)
typedef struct _pitch {
    char key;
    int octave;
    int alter;
} _pitch;
typedef _pitch * pitch;

// Musictype(Duration)
typedef struct _duration {
    int a;
    int b;
} _duration;
typedef _duration * duration;

// Musictype(Note)
typedef struct Note {
    pitch p;
    duration d;
} Note;

/*
// Skip these types

typedef struct Chord {
    int len;
    Note* notes;
    // or
    //Note notes[4];
} Chord;

typedef struct _note_or_chord {
    int type;
    union {
        Note *note;
        Chord *chord;
    } p;
    // or
    // union {
    //     Note note;
    //     Chord chord;
    // } ele;
    // But, avoid pointers unless it's inevitable
} _Seq_ele;

*/

/* Composite music types */

// Musictype(Seq)
typedef struct Seq {
    int len;
    // _Seq_ele *arr; // the terrible version
    Note *arr;
} Seq;

typedef struct _Sequence {
    Seq seq;
    double startTime;
    // Meter timeSignature; // is it important for Midi ??
} _Sequence;

// Sequence[]
typedef struct _Arr_Sequence {
    int len;
    _Sequence* arr;
} _Arr_Sequence;


typedef struct Part {
    _Arr_Sequence seqs;
    // Chord keySignature;
    // Enum Instrument instrument;
} Part;

// Part[]
typedef struct _Arr_Part {
    int len;
    Part* arr;
} _Arr_Part;

typedef struct _Score_Singleton {
    _Arr_Part parts;
    // Chord keySignature;
    // int Tempo;
    // Meter timeSignature = {4, q};
} _Score_Singleton;
