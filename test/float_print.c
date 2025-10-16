#include <stdlib.h> // malloc, free
#include <stdio.h>  // printf
#include <float.h>  // FLT_MIN, FLT_MAX
#include <math.h>   // nextafter, nextafterf, nan, nanf, INFINITY

#define FLOAT_LIB_USE_DEFAULT_ALLOCATOR
#include "../src/float.c"

void f32_print(f32 value, isize precision, bool exponential) {
    FloatFormatParams params = {
        .precision = precision,
        .exponential = exponential,
    };

    isize buffer_size = f32_format(value, NULL, 0, &params);
    char *buffer = malloc(buffer_size);
    f32_format(value, buffer, buffer_size, &params);

    if (params.exponential) {
        printf("%s (via printf: '%.*e')\n", buffer, (int)params.precision, value);
    } else {
        printf("%s (via printf: '%.*f')\n", buffer, (int)params.precision, value);
    }

    free(buffer);
}

void f64_print(f64 value, isize precision, bool exponential) {
    FloatFormatParams params = {
        .precision = precision,
        .exponential = exponential,
    };

    isize buffer_size = f64_format(value, NULL, 0, &params);
    char *buffer = malloc(buffer_size);
    f64_format(value, buffer, buffer_size, &params);

    if (params.exponential) {
        printf("%s (via printf: '%.*e')\n", buffer, (int)params.precision, value);
    } else {
        printf("%s (via printf: '%.*f')\n", buffer, (int)params.precision, value);
    }

    free(buffer);
}

int main(void) {
    // f32 tests

    f32_print(0.0F, 8, true);

    f32_print(123.001F, 3, false);

    f32_print(123.456F, 8, true);
    f32_print(123.456, 8, false);

    f32_print(-4312.12F, 8, true);
    f32_print(-4312.12F, 8, false);

    f32_print(128.125F, 8, true);
    f32_print(128.125F, 8, false);

    f32_print(FLT_MIN, 8, true);
    f32_print(FLT_MAX, 8, true);
    f32_print(nextafterf(0.0F, 1.0F), 8, true);

    f32_print(999.5F, 2, true);
    f32_print(998.5F, 2, true);
    f32_print(123.5F, 2, true);

    f32_print(nanf(""), 8, true);
    f32_print(INFINITY, 8, true);
    f32_print(-INFINITY, 8, true);

    printf("\n");

    // f64 tests

    f64_print(0.0, 16, true);

    f64_print(123.000001, 6, false);

    f64_print(1234567.7654321, 16, true);
    f64_print(1234567.7654321, 16, false);

    f64_print(-7654321.1234567, 16, true);
    f64_print(-7654321.1234567, 16, false);

    f64_print(128.125, 16, true);
    f64_print(128.125, 16, false);

    f64_print(DBL_MIN, 16, true);
    f64_print(DBL_MAX, 16, true);
    f64_print(nextafter(0.0, 1.0), 16, true);

    f64_print(999.5, 2, true);
    f64_print(998.5, 2, true);
    f64_print(123.5, 2, true);

    f64_print(nan(""), 16, true);
    f64_print(INFINITY, 16, true);
    f64_print(-INFINITY, 16, true);

    printf("\n");

    return 0;
}
