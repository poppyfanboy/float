#include "float.h"

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

void f32_print(f32 value, FloatFormatParams *params, char const *label) {
    isize buffer_size = f32_format(value, NULL, 0, params);
    char *buffer = malloc(buffer_size);
    f32_format(value, buffer, buffer_size, params);
    printf("%s = %s\n", label, buffer);
    free(buffer);
}

void f64_print(f64 value, FloatFormatParams *params, char const *label) {
    isize buffer_size = f64_format(value, NULL, 0, params);
    char *buffer = malloc(buffer_size);
    f64_format(value, buffer, buffer_size, params);
    printf("%s = %s\n", label, buffer);
    free(buffer);
}

int main(void) {
    {
        FloatFormatParams params = {.precision = 0};
        f32_print(0.0F, &params, "f32 zero");
        f32_print(123.456F, &params, "regular positive f32 (123.456F)");
        f32_print(-4312.12F, &params, "regular negative f32 (-4312.12F)");
        f32_print(128.125F, &params, "exact f32 (128.125F)");
        f32_print(FLT_MIN, &params, "min f32");
        f32_print(FLT_MAX, &params, "max f32");

        union { u32 i; f32 f; } denormalized = {.i = 1};
        f32_print(denormalized.f, &params, "denormalized f32");

        f32_print(nanf(""), &params, "NaN f32");
        f32_print(INFINITY, &params, "positive infinity f32");
        f32_print(-INFINITY, &params, "negative infinity f32");
    }

    printf("\n");

    {
        FloatFormatParams params = {.precision = 0};
        f64_print(0.0, &params, "f64 zero");
        f64_print(1234567.7654321, &params, "regular positive f64 (1234567.7654321)");
        f64_print(-7654321.1234567, &params, "regular negative f64 (-7654321.1234567)");
        f64_print(128.125, &params, "exact f64 (128.125)");
        f64_print(DBL_MIN, &params, "min f64");
        f64_print(DBL_MAX, &params, "max f64");

        union { u64 i; f64 f; } denormalized = {.i = 1};
        f64_print(denormalized.f, &params, "denormalized f64");

        f64_print(nan(""), &params, "NaN f64");
        f64_print(INFINITY, &params, "positive infinity f64");
        f64_print(-INFINITY, &params, "negative infinity f64");
    }

    return 0;
}
