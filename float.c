#include "float.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char *data;
    isize size;
    isize capacity;
} String;

void string_push_char(String *string, char value) {
    if (string->size + 1 >= string->capacity) {
        isize new_capacity = 2 * string->capacity;
        if (new_capacity < 16) {
            new_capacity = 16;
        }
        string->data = realloc(string->data, new_capacity);
        string->capacity = new_capacity;
        string->data[string->size] = 0;
    }

    string->data[string->size++] = value;
    string->data[string->size] = 0;
}

void string_reverse(String *string) {
    for (isize i = 0; i < string->size / 2; i += 1) {
        char swap = string->data[i];
        string->data[i] = string->data[string->size - i - 1];
        string->data[string->size - i - 1] = swap;
    }
}

typedef struct {
    bool is_negative;
    isize exponent;
    u32 *mantissa;
    isize mantissa_size;
} Float;

static bool is_zero(u32 *number, isize number_size) {
    for (isize i = 0; i < number_size; i += 1) {
        if (number[i] != 0) {
            return false;
        }
    }

    return true;
}

static void divide(
    u32 *dividend,
    isize dividend_size,
    u32 divisor,
    u32 *result,
    u32 *remainder
) {
    u32 *result_iter = result;

    u32 carry = 0;
    for (isize i = 0; i < dividend_size; i += 1) {
        u64 dividend_small = ((u64)carry << 32) | dividend[i];

        // Result should fit into u32 but I don't know why
        *(result++) = dividend_small / divisor;

        carry = dividend_small % divisor;
    }

    *remainder = carry;
}

static void multiply(
    u32 *multiplicand,
    isize multiplicand_size,
    u32 multiplier,
    u32 *product,
    u32 *final_carry
) {
    u32 *product_iter = product + multiplicand_size - 1;

    u32 carry = 0;
    for (isize i = multiplicand_size - 1; i >= 0; i -= 1) {
        u64 product_small = (u64)carry + (u64)multiplier * (u64)multiplicand[i];

        *(product_iter--) = product_small & 0xffffffff;

        carry = product_small >> 32;
    }

    *final_carry = carry;
}

static inline isize float_format(
    Float value,
    char *string,
    isize string_size,
    FloatFormatParams const *params
) {
    if (string != NULL) {
        printf("is_negative: %s\n", value.is_negative ? "yes" : "no");
        printf("exponent: %lld\n", value.exponent);

        printf("mantissa: ");
        for (isize i = 0; i < value.mantissa_size; i += 1) {
            for (isize j = 0; j < 32; j += 1) {
                printf("%d", (value.mantissa[i] >> (31 - j)) & 1);
            }
        }
        printf("\n");
    }

    if (string != NULL && string_size > 0) {
        string[0] = 0;
    }

    // Size in u32-s
    isize integer_part_size;
    isize fractional_part_size;
    if (value.exponent == 0) {
        integer_part_size = 1;
        fractional_part_size = value.mantissa_size;
    } else if (value.exponent < 0) {
        integer_part_size = 1;
        fractional_part_size += ((-value.exponent) + 31) / 32;
    } else if (value.exponent > 0) {
        integer_part_size = (value.exponent + 31) / 32;

        if (value.mantissa_size * 32 - value.exponent > 0) {
            fractional_part_size = ((value.mantissa_size * 32 - value.exponent) + 31) / 32;
        } else {
            fractional_part_size = 1;
        }
    }

    u32 *float_data = calloc(integer_part_size + fractional_part_size, sizeof(u32));
    u32 *integer_part = float_data;
    u32 *fractional_part = integer_part + integer_part_size;

    if (value.exponent == 0) {
        memcpy(fractional_part, value.mantissa, value.mantissa_size * sizeof(u32));
    } else {
        u32 *float_data_iter;
        int bit_shift;
        if (value.exponent < 0) {
            float_data_iter = fractional_part + (-value.exponent) / 32;
            bit_shift = (-value.exponent) % 32;
        } else {
            float_data_iter = integer_part + integer_part_size - 1 - value.exponent / 32;
            bit_shift = 32 - (value.exponent % 32);
        }

        u32 *mantissa_iter = value.mantissa;

        while (mantissa_iter != value.mantissa + value.mantissa_size) {
            float_data_iter[0] |= *mantissa_iter >> bit_shift;
            float_data_iter[1] |= *mantissa_iter << (32 - bit_shift);

            float_data_iter += 1;
            mantissa_iter += 1;
        }
    }

    if (string != NULL) {
        printf("Float expanded:\n");
        for (isize i = 0; i < integer_part_size; i += 1) {
            for (isize j = 0; j < 32; j += 1) {
                printf("%d", (integer_part[i] >> (31 - j)) & 1);
            }
        }
        printf(".");
        for (isize i = 0; i < fractional_part_size; i += 1) {
            for (isize j = 0; j < 32; j += 1) {
                printf("%d", (fractional_part[i] >> (31 - j)) & 1);
            }
        }
        printf("\n");
    }

    String integer_part_decimal = {0};

    u32 *quotient = malloc(integer_part_size * sizeof(u32));
    while (!is_zero(integer_part, integer_part_size)) {
        u32 remainder;
        divide(integer_part, integer_part_size, 10, quotient, &remainder);

        string_push_char(&integer_part_decimal, '0' + remainder);

        u32 *swap = quotient;
        quotient = integer_part;
        integer_part = swap;
    }

    String fractional_part_decimal = {0};

    if (is_zero(fractional_part, fractional_part_size)) {
        string_push_char(&fractional_part_decimal, '0');
    }

    u32 *product = malloc(fractional_part_size * sizeof(u32));
    while (!is_zero(fractional_part, fractional_part_size)) {
        u32 overflow;
        multiply(fractional_part, fractional_part_size, 10, product, &overflow);

        string_push_char(&fractional_part_decimal, '0' + overflow);

        u32 *swap = product;
        product = fractional_part;
        fractional_part = swap;
    }

    if (string != NULL) {
        string_reverse(&integer_part_decimal);
        printf("%s.%s\n", integer_part_decimal.data, fractional_part_decimal.data);
    }

    return 1;
}

isize f32_format(
    f32 value,
    char *string,
    isize string_size,
    FloatFormatParams const *params
) {
    u32 raw_value;
    memcpy(&raw_value, &value, 4);

    bool is_negative = (raw_value & 0x80000000) != 0;
    raw_value &= 0x7fffffff;

    int mantissa_bit_size = 23;

    // TODO: Handle denormalized floats
    isize exponent = (isize)(raw_value >> mantissa_bit_size) - 127 + 1;
    u32 mantissa = (raw_value & 0x007fffff) << (32 - mantissa_bit_size);

    // Add an implicit 1
    mantissa = 0x80000000 | (mantissa >> 1);

    return float_format(
        (Float){is_negative, exponent, &mantissa, .mantissa_size = 1},
        string,
        string_size,
        params
    );
}

isize f64_format(
    f64 value,
    char *string,
    isize string_size,
    FloatFormatParams const *params
) {
    u64 raw_value;
    memcpy(&raw_value, &value, 8);

    bool is_negative = (raw_value & 0x8000000000000000) != 0;
    raw_value &= 0x7fffffffffffffff;

    // TODO: Handle denormalized floats
    isize exponent = (isize)(raw_value >> 52) - 1023 + 1;
    u32 mantissa[2] = {
        // Add an implicit 1
        // 1 set implicit bit + 31 bit from mantissa goes into the first element
        0x80000000 | ((raw_value >> (52 - 31)) & 0x7fffffff),
        // the rest 21 bit go into the second element
        (raw_value & 0x001fffff) << 11,
    };

    return float_format(
        (Float){is_negative, exponent, mantissa, .mantissa_size = 2},
        string,
        string_size,
        params
    );
}
