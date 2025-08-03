#ifndef FLOAT_H
#define FLOAT_H

#include <stdbool.h>

#ifndef SHORT_NAMES_FOR_PRIMITIVE_TYPES_WERE_DEFINED
#define SHORT_NAMES_FOR_PRIMITIVE_TYPES_WERE_DEFINED

#include <stdint.h>
#include <stddef.h>

typedef uint8_t u8;
typedef uint32_t u32;
typedef uint64_t u64;

typedef uintptr_t uptr;
typedef intptr_t isize;
typedef size_t usize;

typedef float f32;
typedef double f64;

#endif // SHORT_NAMES_FOR_PRIMITIVE_TYPES_WERE_DEFINED

typedef struct {
    isize precision;
} FloatFormatParams;

bool is_f32(char const *string, isize string_size);
f32 f32_parse(char const *string, isize string_size);

isize f32_format(f32 value, char *string, isize string_size, FloatFormatParams const *params);

bool is_f64(char const *string, isize string_size);
f64 f64_parse(char const *string, isize string_size);

isize f64_format(f64 value, char *string, isize string_size, FloatFormatParams const *params);

#endif // FLOAT_H
