#ifndef FLOAT_H
#define FLOAT_H

#include <stdbool.h>

// Redefinition of typedefs is a C11 feature.
// This is the official™ guard, which is used across different headers to protect u8 and friends.
// (Or just add a #define before including this header, if you already have short names defined.)
#ifndef SHORT_NAMES_FOR_PRIMITIVE_TYPES_WERE_DEFINED
    #define SHORT_NAMES_FOR_PRIMITIVE_TYPES_WERE_DEFINED

    #include <stdint.h>
    #include <stddef.h>

    typedef int8_t i8;
    typedef uint8_t u8;
    typedef uint16_t u16;
    typedef int16_t i16;
    typedef uint32_t u32;
    typedef int32_t i32;
    typedef uint64_t u64;
    typedef int64_t i64;

    typedef uintptr_t uptr;
    typedef size_t usize;
    typedef ptrdiff_t isize;

    typedef float f32;
    typedef double f64;
#endif

typedef struct {
    void *user_data;
    void *(*alloc)(isize size, void *user_data);
    void(*dealloc)(void *ptr, isize size, void *user_data);
} FloatLibAllocator;

#ifdef FLOAT_LIB_USE_DEFAULT_ALLOCATOR
    #include <stdlib.h> // malloc, free
    #include <stddef.h> // NULL

    static inline void *float_lib_default_alloc(isize size, void *user_data) {
        return malloc(size);
    }

    static inline void float_lib_default_dealloc(void *ptr, isize size, void *user_data) {
        free(ptr);
    }

    #define FLOAT_LIB_DEFAULT_ALLOCATOR &(FloatLibAllocator){   \
        .user_data = NULL,                                      \
        .alloc = float_lib_default_alloc,                       \
        .dealloc = float_lib_default_dealloc,                   \
    }
#endif

typedef struct {
    isize precision;
    bool exponential;
} FloatFormatParams;

// You can pass -1 as string size in case the string is null-terminated.
bool string_is_float(char const *string, isize string_size);
f32 f32_parse(char const *string, isize string_size, FloatLibAllocator *allocator);
f64 f64_parse(char const *string, isize string_size, FloatLibAllocator *allocator);

isize f32_format(f32 value, char *string, isize string_size, FloatFormatParams const *params);
isize f64_format(f64 value, char *string, isize string_size, FloatFormatParams const *params);

#endif // FLOAT_H
