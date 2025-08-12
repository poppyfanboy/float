#include "float.h"

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

void f32_print(f32 value, FloatFormatParams *params) {
    isize buffer_size = f32_format(value, NULL, 0, params);
    char *buffer = malloc(buffer_size);
    f32_format(value, buffer, buffer_size, params);
    printf("%s (via printf: '%.*e')\n", buffer, (int)params->precision - 1, value);
    free(buffer);
}

void f64_print(f64 value, FloatFormatParams *params) {
    isize buffer_size = f64_format(value, NULL, 0, params);
    char *buffer = malloc(buffer_size);
    f64_format(value, buffer, buffer_size, params);
    printf("%s (via printf: '%.*e')\n", buffer, (int)params->precision - 1, value);
    free(buffer);
}

int main(void) {
    {
        FloatFormatParams params = {.precision = 9};
        f32_print(0.0F, &params);
        f32_print(123.456F, &params);
        f32_print(-4312.12F, &params);
        f32_print(128.125F, &params);
        f32_print(FLT_MIN, &params);
        f32_print(FLT_MAX, &params);
        f32_print(nextafterf(0.0F, 1.0F), &params);

        f32_print(999.5F, &(FloatFormatParams){.precision = 3});
        f32_print(998.5F, &(FloatFormatParams){.precision = 3});
        f32_print(123.5F, &(FloatFormatParams){.precision = 3});

        f32_print(nanf(""), &params);
        f32_print(INFINITY, &params);
        f32_print(-INFINITY, &params);
    }

    printf("\n");

    {
        FloatFormatParams params = {.precision = 17};
        f64_print(0.0, &params);
        f64_print(1234567.7654321, &params);
        f64_print(-7654321.1234567, &params);
        f64_print(128.125, &params);
        f64_print(DBL_MIN, &params);
        f64_print(DBL_MAX, &params);
        f64_print(nextafter(0.0, 1.0), &params);

        f32_print(999.5, &(FloatFormatParams){.precision = 3});
        f32_print(998.5, &(FloatFormatParams){.precision = 3});
        f32_print(123.5, &(FloatFormatParams){.precision = 3});

        f64_print(nan(""), &params);
        f64_print(INFINITY, &params);
        f64_print(-INFINITY, &params);
    }

    return 0;
}
