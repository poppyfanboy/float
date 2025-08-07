#include "float.h"

#include <string.h> // memcpy, memset, strlen, strcpy
#include <stdlib.h> // abort
#include <assert.h> // assert

typedef struct {
    u8 *begin;
    u8 *end;
} Arena;

#define ARENA_ALIGNMENT 16

static void *arena_alloc(void *arena_void, isize size) {
    Arena *arena = arena_void;

    isize padding = (~(uptr)arena->begin + 1) & (ARENA_ALIGNMENT - 1);
    assert(arena->begin + padding + size <= arena->end);

    void *ptr = arena->begin + padding;
    arena->begin += padding + size;
    return ptr;
}

typedef struct {
    char *dest;
    isize dest_size;
    isize chars_written;
} StringWriter;

static void string_writer_push_char(StringWriter *string_writer, char next_char) {
    if (string_writer->dest != NULL) {
        if (string_writer->chars_written == string_writer->dest_size) {
            abort();
        }
        string_writer->dest[string_writer->chars_written] = next_char;
    }
    string_writer->chars_written += 1;
}

static void string_writer_push_string(StringWriter *string_writer, char const *next_string) {
    isize next_string_size = strlen(next_string);

    if (string_writer->dest != NULL) {
        if (string_writer->chars_written + next_string_size > string_writer->dest_size) {
            abort();
        }
        strcpy(string_writer->dest + string_writer->chars_written, next_string);
    } else {
        string_writer->chars_written += next_string_size;
    }
}

static void string_reverse(char *begin, char *end) {
    char *forwards_iter = begin;
    char *backwards_iter = end - 1;

    while (forwards_iter < backwards_iter) {
        char swap = *forwards_iter;
        *forwards_iter = *backwards_iter;
        *backwards_iter = swap;

        forwards_iter += 1;
        backwards_iter -= 1;
    }
}

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
    u32 *quotient,
    u32 *final_remainder
) {
    u32 remainder = 0;
    for (isize i = 0; i < dividend_size; i += 1) {
        u64 dividend_chunk = ((u64)remainder << 32) | dividend[i];

        // Result should fit into u32, but I don't know why.
        quotient[i] = dividend_chunk / divisor;
        remainder = dividend_chunk % divisor;
    }

    *final_remainder = remainder;
}

static void multiply(
    u32 *multiplicand,
    isize multiplicand_size,
    u32 multiplier,
    u32 *product,
    u32 *final_overflow
) {
    u32 overflow = 0;
    for (isize i = multiplicand_size - 1; i >= 0; i -= 1) {
        u64 product_chunk = (u64)overflow + (u64)multiplier * (u64)multiplicand[i];

        product[i] = product_chunk & 0xffffffff;
        overflow = product_chunk >> 32;
    }

    *final_overflow = overflow;
}

typedef struct {
    bool is_negative;
    isize exponent;
    u32 *mantissa;
    isize mantissa_size;
} Float;

// Mantissa of the Float value here is expected to either start with a 1 or be completely zeroed.
// Which means that when the exponent is 0 the absolute value encoded into the float is less than 1.
static inline isize float_format(
    Float value,
    char *string, isize string_size,
    FloatFormatParams const *params,
    Arena arena
) {
    // Sizes are measured in u32s count.
    isize integer_part_size;
    isize fractional_part_size;
    if (value.exponent == 0) {
        integer_part_size = 1;
        fractional_part_size = value.mantissa_size;
    } else if (value.exponent < 0) {
        integer_part_size = 1;
        fractional_part_size = ((-value.exponent) + 31) / 32;
    } else if (value.exponent > 0) {
        integer_part_size = (value.exponent + 31) / 32;

        if (value.mantissa_size * 32 - value.exponent > 0) {
            fractional_part_size = ((value.mantissa_size * 32 - value.exponent) + 31) / 32;
        } else {
            fractional_part_size = 1;
        }
    }

    u32 *float_data = arena_alloc(&arena, (integer_part_size + fractional_part_size) * sizeof(u32));
    memset(float_data, 0, (integer_part_size + fractional_part_size) * sizeof(u32));

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

    StringWriter string_writer = {
        .dest = string,
        .dest_size = string_size,
        .chars_written = 0,
    };

    if (value.is_negative) {
        string_writer_push_char(&string_writer, '-');
    }

    if (is_zero(integer_part, integer_part_size)) {
        string_writer_push_char(&string_writer, '0');
    } else {
        char *string_integer_part_begin = string + string_writer.chars_written;

        u32 *quotient = arena_alloc(&arena, integer_part_size * sizeof(u32));
        while (!is_zero(integer_part, integer_part_size)) {
            u32 remainder;
            divide(integer_part, integer_part_size, 10, quotient, &remainder);

            string_writer_push_char(&string_writer, '0' + remainder);

            u32 *swap = quotient;
            quotient = integer_part;
            integer_part = swap;
        }

        if (string != NULL) {
            string_reverse(string_integer_part_begin, string + string_writer.chars_written);
        }
    }

    string_writer_push_char(&string_writer, '.');

    if (is_zero(fractional_part, fractional_part_size)) {
        string_writer_push_char(&string_writer, '0');
    } else {
        u32 *product = arena_alloc(&arena, fractional_part_size * sizeof(u32));
        while (!is_zero(fractional_part, fractional_part_size)) {
            u32 overflow;
            multiply(fractional_part, fractional_part_size, 10, product, &overflow);

            string_writer_push_char(&string_writer, '0' + overflow);

            u32 *swap = product;
            product = fractional_part;
            fractional_part = swap;
        }
    }

    string_writer_push_char(&string_writer, '\0');
    return string_writer.chars_written;
}

isize f32_format(f32 value, char *string, isize string_size, FloatFormatParams const *params) {
    u32 raw_value;
    memcpy(&raw_value, &value, 4);

    bool is_negative = (raw_value & 0x80000000) != 0;
    raw_value &= 0x7fffffff;

    // Mantissa size without an implicit 1.
    int const mantissa_bits = 23;
    u32 const mantissa_mask = 0x007fffff;

    isize raw_exponent = (isize)(raw_value >> mantissa_bits);

    isize exponent;
    u32 mantissa;
    if (raw_exponent == 0) {
        // Subnormals and zero.

        exponent = -126;
        mantissa = (raw_value & mantissa_mask) << (32 - mantissa_bits);

        if (mantissa != 0) {
            int leading_zeroes = __builtin_clz(mantissa);
            mantissa <<= leading_zeroes;
            exponent -= leading_zeroes;
        }
    } else if (raw_exponent == 255) {
        // Infinities and NaNs.

        StringWriter string_writer = {
            .dest = string,
            .dest_size = string_size,
            .chars_written = 0,
        };

        mantissa = raw_value & mantissa_mask;
        if (mantissa == 0) {
            if (is_negative) {
                string_writer_push_char(&string_writer, '-');
            }
            string_writer_push_string(&string_writer, "inf");
        } else {
            string_writer_push_string(&string_writer, "nan");
        }

        return string_writer.chars_written;
    } else {
        // Normal floats.

        exponent = raw_exponent - 127;
        mantissa = (raw_value & mantissa_mask) << (32 - mantissa_bits);

        // Add an implicit 1.
        exponent += 1;
        mantissa = 0x80000000 | (mantissa >> 1);
    }

    // Should be enough memory to convert any f32 float?
    u8 arena_buffer[8 * 1024];
    Arena arena = {
        .begin = arena_buffer,
        .end = arena_buffer + 8 * 1024,
    };

    return float_format(
        (Float){
            is_negative, exponent, (u32[]){mantissa},
            .mantissa_size = 1,
        },
        string, string_size,
        params,
        arena
    );
}

isize f64_format(f64 value, char *string, isize string_size, FloatFormatParams const *params) {
    u64 raw_value;
    memcpy(&raw_value, &value, 8);

    bool is_negative = (raw_value & 0x8000000000000000) != 0;
    raw_value &= 0x7fffffffffffffff;

    // Mantissa size without an implicit 1.
    int const mantissa_bits = 52;
    u64 const mantissa_mask = 0xfffffffffffff;

    isize raw_exponent = (isize)(raw_value >> mantissa_bits);

    isize exponent;
    u64 mantissa;
    if (raw_exponent == 0) {
        // Subnormals and zero.

        exponent = -1022;
        mantissa = (raw_value & mantissa_mask) << (64 - mantissa_bits);

        if (mantissa != 0) {
            int leading_zeroes = __builtin_clzll(mantissa);
            mantissa <<= leading_zeroes;
            exponent -= leading_zeroes;
        }
    } else if (raw_exponent == 2047) {
        // Infinities and NaNs.

        StringWriter string_writer = {
            .dest = string,
            .dest_size = string_size,
            .chars_written = 0,
        };

        mantissa = raw_value & mantissa_mask;
        if (mantissa == 0) {
            if (is_negative) {
                string_writer_push_char(&string_writer, '-');
            }
            string_writer_push_string(&string_writer, "inf");
        } else {
            string_writer_push_string(&string_writer, "nan");
        }

        return string_writer.chars_written;
    } else {
        // Normal floats.

        exponent = raw_exponent - 1023;
        mantissa = (raw_value & mantissa_mask) << (64 - mantissa_bits);

        // Add an implicit 1.
        exponent += 1;
        mantissa = 0x8000000000000000 | (mantissa >> 1);
    }

    // Should be enough memory to convert any f64 float?
    u8 arena_buffer[8 * 1024];
    Arena arena = {
        .begin = arena_buffer,
        .end = arena_buffer + 8 * 1024,
    };

    return float_format(
        (Float){
            is_negative, exponent, (u32[]){mantissa >> 32, mantissa & 0xffffffff},
            .mantissa_size = 2,
        },
        string, string_size,
        params,
        arena
    );
}
