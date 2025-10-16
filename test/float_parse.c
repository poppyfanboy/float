#include <assert.h> // assert
#include <stdlib.h> // malloc, free
#include <stdio.h>  // printf
#include <float.h>  // FLT_MIN, FLT_MAX
#include <math.h>   // nextafter, nextafterf, nan, nanf, isnormal, INFINITY

#define FLOAT_LIB_USE_DEFAULT_ALLOCATOR
#include "../src/float.c"

f32 f32_test_parse(char const *string) {
    if (!string_represents_float(string, -1)) {
        printf("'%s' is not a float.\n", string);

        return nanf("");
    } else {
        f32 expected = strtod(string, NULL);
        f32 actual = f32_parse(string, -1, FLOAT_LIB_DEFAULT_ALLOCATOR);

        printf(
            "%25s => %1.8e (via strtof: %1.8e) (%s)\n",
            string, actual, expected, actual == expected ? "OK" : "FAIL"
        );

        return actual;
    }
}

f64 f64_test_parse(char const *string) {
    if (!string_represents_float(string, -1)) {
        printf("'%s' is not a float.\n", string);

        return nan("");
    } else {
        f64 expected = strtod(string, NULL);
        f64 actual = f64_parse(string, -1, FLOAT_LIB_DEFAULT_ALLOCATOR);

        printf(
            "%25s => %1.16e (via strtod: %1.16e) (%s)\n",
            string, actual, expected, actual == expected ? "OK" : "FAIL"
        );

        return actual;
    }
}

int main(void) {
    f32_test_parse("0");
    f32_test_parse("1");
    f32_test_parse("123");
    f32_test_parse("123e10");
    f32_test_parse("33554431");
    f32_test_parse("12345e8");
    f32_test_parse("12345464e10323");

    f32_test_parse("123.123");
    f32_test_parse("1.40129846e-45");
    f32_test_parse("5e-46");
    f32_test_parse("-5e-46");

    printf("\n");

    f64_test_parse("0");
    f64_test_parse("1");
    f64_test_parse("12345678912345");
    f64_test_parse("123e30");
    f64_test_parse("333554431355443");
    f64_test_parse("112345123452345e8");
    f64_test_parse("123456789123456789123e42");
    f64_test_parse("12345464e10323");

    f64_test_parse("-123.123");
    f64_test_parse("12345678.1234567");
    f64_test_parse("123456789.123456789123");

    // Maximum value.
    f64_test_parse("1.7976931348623157e+308");
    f64_test_parse("1.7976931348623159e+308");

    // Overflows into infinity.
    f64_test_parse("1.8e+308");
    f64_test_parse("-1.8e+308");

    {
        // Trying out numbers between a max denormalized and a min normalized.
        // That is a number between (2^52 - 1) * 2^(-1022 - 52) and 2^(-1022).
        // In other words a number between 2.2250738585072009e-308 and 2.2250738585072014e-308.
        // The middle is in the 2.225073858507201136057409796709131975934819546351645648e-308.

        // Rounds down into a denormal number.
        f64 rounds_down = f64_test_parse("2.2250738585072010e-308");
        assert(!isnormal(rounds_down));

        // Rounds up into a normal number.
        f64 rounds_up = f64_test_parse("2.2250738585072012e-308");
        assert(isnormal(rounds_up));
    }

    // A denormalized number which is getting rounded.
    // 2^(-1072) + 2^(-1073) + 2^(-1074) + 2^(-1075)
    f64_test_parse("3.7054923438093490813242659465e-323");

    // Rounding up into the smallest denormalized number.
    f64_test_parse("2.5e-324");
    // Rounding down into zero.
    f64_test_parse("1.5e-324");
    f64_test_parse("1.5e-999");

    // Overflowing the exponent.
    f64_test_parse("2.5e999999999999999999999999999999999999999999999999999999999999999999999999");
    f64_test_parse("-2.5e999999999999999999999999999999999999999999999999999999999999999999999999");
    // Underflowing the exponent.
    f64_test_parse("1.5e-99999999999999999999999999999999999999999999999999999999999999999999999");

    printf("\n");

    return 0;
}
