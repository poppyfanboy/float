#define FLOAT_LIB_USE_DEFAULT_ALLOCATOR
#include "float.h"

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

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

void f32_test_parse(char const *string) {
    if (!string_is_float(string, -1)) {
        printf("'%s' is not a float.\n", string);
    } else {
        f32 output = f32_parse(string, -1, FLOAT_LIB_DEFAULT_ALLOCATOR);
        printf(
            "%25s => %1.8e (via strtof: %1.8e) (%s)\n",
            string,
            output,
            strtof(string, NULL),
            output == strtof(string, NULL) ? "OK" : "FAIL"
        );
    }
}

void f64_test_parse(char const *string) {
    if (!string_is_float(string, -1)) {
        printf("'%s' is not a float.\n", string);
    } else {
        f64 output = f64_parse(string, -1, FLOAT_LIB_DEFAULT_ALLOCATOR);
        printf(
            "%25s => %1.16e (via strtod: %1.16e) (%s)\n",
            string,
            output,
            strtod(string, NULL),
            output == strtod(string, NULL) ? "OK" : "FAIL"
        );
    }
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

    f32_test_parse("0");
    f32_test_parse("1");
    f32_test_parse("123");
    f32_test_parse("123e10");
    f32_test_parse("33554431");
    f32_test_parse("12345e8");
    f32_test_parse("12345464e10323");

    printf("\n");

    f64_test_parse("0");
    f64_test_parse("1");
    f64_test_parse("12345678912345");
    f64_test_parse("123e30");
    f64_test_parse("333554431355443");
    f64_test_parse("112345123452345e8");
    f64_test_parse("123456789123456789123e42");
    f64_test_parse("12345464e10323");

    printf("\n");

    return 0;
}
