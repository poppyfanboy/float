// Algorithm for printing floats:
// https://randomascii.wordpress.com/2012/03/08/float-precisionfrom-zero-to-100-digits-2/
//
// Algorithm for parsing floats:
// https://www.exploringbinary.com/correct-decimal-to-floating-point-using-big-integers/
//
// How many digits are needed for round-trip conversions between decimal and float:
// https://www.exploringbinary.com/number-of-digits-required-for-round-trip-conversions/

#include "float.h"

#include <string.h> // memcpy, memset
#include <assert.h> // assert

static isize isize_max(isize left, isize right) {
    return left > right ? left : right;
}

#define ARENA_ALIGNMENT 16

typedef struct {
    u8 *begin;
    u8 *end;
} Arena;

static void *arena_alloc(Arena *arena, isize size) {
    isize padding = (~(uptr)arena->begin + 1) & (ARENA_ALIGNMENT - 1);
    assert(arena->begin + padding + size <= arena->end);

    void *ptr = arena->begin + padding;
    arena->begin += padding + size;
    return ptr;
}

static void bytes_reverse(void *begin, void *end) {
    u8 *forwards_iter = begin;
    u8 *backwards_iter = (u8 *)end - 1;

    while (forwards_iter < backwards_iter) {
        u8 swap = *forwards_iter;
        *forwards_iter = *backwards_iter;
        *backwards_iter = swap;

        forwards_iter += 1;
        backwards_iter -= 1;
    }
}

typedef struct {
    char const *data;
    isize size;
} StringView;

#define SV(string_literal)                  \
    (StringView) {                          \
        .data = string_literal,             \
        .size = sizeof(string_literal) - 1, \
    }

// Writes data into the dest buffer if it is not null. Otherwise, counts how many chars would have
// been written into the buffer, if there was one.
typedef struct {
    char *dest;
    isize dest_size;
    isize chars_written;
} StringWriter;

static void string_writer_push_char(StringWriter *string_writer, char character) {
    if (string_writer->dest != NULL) {
        assert(string_writer->chars_written + 1 <= string_writer->dest_size);
        string_writer->dest[string_writer->chars_written] = character;
    }

    string_writer->chars_written += 1;
}

static void string_writer_push_string(StringWriter *string_writer, StringView string) {
    if (string_writer->dest != NULL) {
        assert(string_writer->chars_written + string.size <= string_writer->dest_size);
        memcpy(string_writer->dest + string_writer->chars_written, string.data, string.size);
    }

    string_writer->chars_written += string.size;
}

static void string_writer_push_int(StringWriter *string_writer, isize integer) {
    if (integer < 0) {
        string_writer_push_char(string_writer, '-');
    }

    // Should be large enough to hold any 64-bit integer in decimal form.
    char decimal[32];
    char *decimal_iter = decimal;

    // Iterate at least once to cover the case of integer equal to 0.
    do {
        // I stole this idea from here: https://stackoverflow.com/a/23840699
        *decimal_iter = ("9876543210123456789")[9 + (integer % 10)];

        integer /= 10;
        decimal_iter += 1;
    } while (integer != 0);

    isize decimal_size = decimal_iter - decimal;

    if (string_writer->dest != NULL) {
        assert(string_writer->chars_written + decimal_size <= string_writer->dest_size);

        // Write the decimal number in reverse.
        for (isize i = 0; i < decimal_size; i += 1) {
            decimal_iter -= 1;
            string_writer->dest[string_writer->chars_written + i] = *decimal_iter;
        }
    }

    string_writer->chars_written += decimal_size;
}

// A multi-word unsigned integer number.
// The data is in "little-endian" order, meaning that data[0] is the least significant digit.
typedef struct {
    u32 *data;
    isize size;
} Number;

// An empty number (size == 0) is treated as zero.
static bool number_is_zero(Number number) {
    for (isize i = 0; i < number.size; i += 1) {
        if (number.data[i] != 0) {
            return false;
        }
    }

    return true;
}

static void number_divide(Number dividend, u32 divisor, Number quotient, u32 *remainder) {
    u32 intermediate_remainder = 0;

    // Start from the most significant digits of the dividend.
    for (isize i = dividend.size - 1; i >= 0; i -= 1) {
        u64 intermediate_dividend = ((u64)intermediate_remainder << 32) | dividend.data[i];

        // Intermediate quotient fits into a u32.
        //
        // Proof by contradiction. Let's say that it doesn't fit, then this means that:
        // (remainder * 2^32 + next_digit) / divisor >= 2^32
        // next_digit >= 2^32 * (divisor - remainder) >= 2^32
        // Which is a contradiction, because next_digit < 2^32.

        quotient.data[i] = intermediate_dividend / divisor;
        intermediate_remainder = intermediate_dividend % divisor;
    }

    *remainder = intermediate_remainder;
}

static void number_multiply(Number multiplicand, u32 multiplier, Number product, u32 *overflow) {
    u32 intermediate_overflow = 0;

    // Start from the least significant digits of the multiplicand.
    for (isize i = 0; i < multiplicand.size; i += 1) {
        u64 intermediate_product =
            (u64)intermediate_overflow +
            (u64)multiplier * (u64)multiplicand.data[i];

        product.data[i] = intermediate_product & 0xffffffff;
        intermediate_overflow = intermediate_product >> 32;
    }

    *overflow = intermediate_overflow;
}

// Represents a value of: (is_negative ? -1 : 1) * mantissa * (2 ^ exponent)
typedef struct {
    bool is_negative;
    isize exponent;
    Number mantissa;
} Float;

static inline isize float_format(
    Float value,
    char *string, isize string_size,
    FloatFormatParams const *params,
    Arena arena
) {
    isize integer_part_size = isize_max((value.mantissa.size * 32 + value.exponent + 31) / 32, 0);
    isize fractional_part_size = isize_max((0 - value.exponent + 31) / 32, 0);

    u32 *fixed = arena_alloc(&arena, (integer_part_size + fractional_part_size) * sizeof(u32));
    memset(fixed, 0, (integer_part_size + fractional_part_size) * sizeof(u32));

    Number integer_part = {
        .data = fixed + fractional_part_size,
        .size = integer_part_size,
    };
    Number fractional_part = {
        .data = fixed,
        .size = fractional_part_size,
    };

    // Examples of expanding a float into a fixed-point number with 4-bit digits.
    //
    // Input mantissa: 1100 1011 1010
    // 101010111100 * 2^-19 = 0.0000_0001_0101_0111_100     => 1000 0111 0101 0001 0000 .
    // 101010111100 * 2^-7  = 1_0101.0111_100               => 1000 0111 . 0101 0001
    // 101010111100 * 2^0   = 1010_1011_1100                => . 1100 1011 1010
    // 101010111100 * 2^9   = 1_0101_0111_1000_0000_0000    => . 0000 0000 1000 0111 0101 0001
    {
        u32 *fixed_iter = fixed;
        u32 *fixed_end = fixed + integer_part_size + fractional_part_size;

        // Skip the zeros of the integer part, when the exponent is large:
        if (value.exponent >= 32) {
            fixed_iter += value.exponent / 32;
        }

        u32 *mantissa_iter = value.mantissa.data;
        u32 *mantissa_end = value.mantissa.data + value.mantissa.size;

        u32 carry = 0;
        int bit_shift = ((value.exponent % 32) + 32) % 32;

        // Copy mantissa digits starting from the least significant ones:
        while (fixed_iter != fixed_end) {
            if (mantissa_iter < mantissa_end) {
                *fixed_iter = (*mantissa_iter << bit_shift) | carry;
                carry = bit_shift == 0 ? 0 : *mantissa_iter >> (32 - bit_shift);

                mantissa_iter += 1;
            } else {
                *fixed_iter = carry;

                // We ran out of mantissa digits. The rest of the digits are just zeroes.
                // (This can happen when the exponent is small.)
                break;
            }
            fixed_iter += 1;
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

    // These are needed to store the quotients and products when converting to decimal.
    Number integer_part_swap = {
        .data = arena_alloc(&arena, integer_part_size * sizeof(u32)),
        .size = integer_part_size,
    };
    Number fractional_part_swap = {
        .data = arena_alloc(&arena, fractional_part_size * sizeof(u32)),
        .size = fractional_part_size,
    };

    // [decimal mantissa] * (10 ^ decimal_exponent)
    // The order is from the most significant digits to the least significant ones.
    i8 *decimal_mantissa = (i8 *)arena.begin;
    isize decimal_mantissa_size = 0;
    isize decimal_mantissa_capacity = arena.end - arena.begin;
    isize decimal_exponent = 0;

    // Allocate an additional digit in case we overflow when rounding up.
    assert(decimal_mantissa_capacity > 0);
    decimal_mantissa[0] = 0;
    decimal_mantissa_size += 1;

    // Put the integer part into the decimal mantissa.
    {
        i8 *decimal_integer_part_begin = decimal_mantissa + decimal_mantissa_size;

        do {
            u32 remainder;
            number_divide(integer_part, 10, integer_part_swap, &remainder);

            assert(decimal_mantissa_size < decimal_mantissa_capacity);
            decimal_mantissa[decimal_mantissa_size] = remainder;
            decimal_mantissa_size += 1;

            Number swap = integer_part_swap;
            integer_part_swap = integer_part;
            integer_part = swap;
        } while (!number_is_zero(integer_part));

        bytes_reverse(decimal_integer_part_begin, decimal_mantissa + decimal_mantissa_size);
    }

    // Put the fractional part into the decimal mantissa.
    {
        // Truncate leading zeroes for small numbers.
        if (number_is_zero(integer_part)) {
            while (!number_is_zero(fractional_part)) {
                u32 overflow;
                number_multiply(fractional_part, 10, fractional_part_swap, &overflow);

                if (overflow > 0) {
                    break;
                }

                decimal_exponent -= 1;

                Number swap = fractional_part_swap;
                fractional_part_swap = fractional_part;
                fractional_part = swap;
            }
        }

        while (!number_is_zero(fractional_part)) {
            u32 overflow;
            number_multiply(fractional_part, 10, fractional_part_swap, &overflow);

            assert(decimal_mantissa_size < decimal_mantissa_capacity);
            decimal_mantissa[decimal_mantissa_size] = overflow;
            decimal_mantissa_size += 1;
            decimal_exponent -= 1;

            Number swap = fractional_part_swap;
            fractional_part_swap = fractional_part;
            fractional_part = swap;
        }
    }

    // Actually "allocate" the decimal mantissa within the arena.
    arena.begin += decimal_mantissa_size;

    enum {
        ROUND_UP,
        ROUND_DOWN,
        ROUND_TO_EVEN,
    } rounding;

    // The first non-zero digit of the decimal mantissa.
    i8 *decimal_mantissa_begin = decimal_mantissa;
    i8 *decimal_mantissa_end = decimal_mantissa + decimal_mantissa_size;
    while (decimal_mantissa_begin < decimal_mantissa_end && *decimal_mantissa_begin == 0) {
        decimal_mantissa_begin += 1;
    }

    i8 last_significant_digit = 0;
    i8 rounding_digit = 0;
    {
        i8 *decimal_mantissa_iter = decimal_mantissa_begin;

        if (decimal_mantissa_iter < decimal_mantissa_end) {
            isize precision_left = params->precision;

            rounding_digit = *decimal_mantissa_iter;
            decimal_mantissa_iter += 1;

            while (precision_left > 0) {
                last_significant_digit = rounding_digit;

                if (decimal_mantissa_iter < decimal_mantissa_end) {
                    rounding_digit = *decimal_mantissa_iter;
                } else {
                    rounding_digit = 0;
                }

                decimal_mantissa_iter += 1;
                precision_left -= 1;
            }
        }
    }

    if (rounding_digit < 5) {
        rounding = ROUND_DOWN;
    } else if (rounding_digit > 5) {
        rounding = ROUND_UP;
    } else {
        rounding = ROUND_TO_EVEN;

        i8 *decimal_mantissa_iter = decimal_mantissa_begin;
        i8 *decimal_mantissa_end = decimal_mantissa + decimal_mantissa_size;

        // Skip past the rounding digit.
        decimal_mantissa_iter += params->precision + 1;
        while (decimal_mantissa_iter < decimal_mantissa_end) {
            if (*decimal_mantissa_iter > 0) {
                rounding = ROUND_UP;
                break;
            }

            decimal_mantissa_iter += 1;
        }
    }

    if (rounding == ROUND_UP || rounding == ROUND_TO_EVEN && last_significant_digit % 2 == 1) {
        i8 overflow = 1;

        i8 *decimal_mantissa_iter = decimal_mantissa_begin;
        i8 *decimal_mantissa_end = decimal_mantissa + decimal_mantissa_size;

        decimal_mantissa_iter += params->precision - 1;
        while (decimal_mantissa_iter >= decimal_mantissa) {
            i8 intermediate_sum = overflow + *decimal_mantissa_iter;
            *decimal_mantissa_iter = intermediate_sum % 10;
            overflow = intermediate_sum / 10;

            decimal_mantissa_iter -= 1;
        }

        // Recalculate position of the first non-zero mantissa digit, because an overflow could have
        // happened during the rounding.
        decimal_mantissa_begin = decimal_mantissa;
        while (decimal_mantissa_begin < decimal_mantissa_end && *decimal_mantissa_begin == 0) {
            decimal_mantissa_begin += 1;
        }
    }

    i8 *decimal_mantissa_iter = decimal_mantissa_begin;
    isize precision_left = params->precision;

    // Print the first digit and the decimal point:
    if (decimal_mantissa_iter < decimal_mantissa_end) {
        string_writer_push_char(&string_writer, '0' + *decimal_mantissa_iter);
        decimal_mantissa_iter += 1;
        precision_left -= 1;
    } else {
        string_writer_push_char(&string_writer, '0');
    }

    string_writer_push_char(&string_writer, '.');

    if (precision_left > 0) {
        while (precision_left > 0) {
            if (decimal_mantissa_iter < decimal_mantissa_end) {
                string_writer_push_char(&string_writer, '0' + *decimal_mantissa_iter);
                decimal_mantissa_iter += 1;
            } else {
                string_writer_push_char(&string_writer, '0');
            }

            precision_left -= 1;
        }
    } else {
        string_writer_push_char(&string_writer, '0');
    }

    // Print the adjusted exponent:
    {
        string_writer_push_char(&string_writer, 'e');
        string_writer_push_int(
            &string_writer,
            decimal_exponent + isize_max(decimal_mantissa_end - decimal_mantissa_begin, 1) - 1
        );
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

    // Handle infinities and NaNs.
    if (raw_exponent == 255) {
        StringWriter string_writer = {
            .dest = string,
            .dest_size = string_size,
            .chars_written = 0,
        };

        if ((raw_value & mantissa_mask) == 0) {
            if (is_negative) {
                string_writer_push_char(&string_writer, '-');
            }
            string_writer_push_string(&string_writer, SV("inf"));
        } else {
            string_writer_push_string(&string_writer, SV("nan"));
        }
        string_writer_push_char(&string_writer, '\0');

        return string_writer.chars_written;
    }

    isize exponent;
    u32 mantissa;
    if (raw_exponent == 0) {
        // Subnormals and zero.
        exponent = -126 - mantissa_bits;
        mantissa = raw_value & mantissa_mask;

        if (mantissa == 0) {
            exponent = 0;
        }
    } else {
        // Normal floats (with an implicit one).
        exponent = raw_exponent - 127 - mantissa_bits;
        mantissa = (mantissa_mask + 1) | (raw_value & mantissa_mask);
    }

    // Should be enough memory to convert any f32 float?
    u8 arena_buffer[8 * 1024];
    Arena arena = {
        .begin = arena_buffer,
        .end = arena_buffer + 8 * 1024,
    };

    return float_format(
        (Float){is_negative, exponent, (Number){(u32[]){mantissa}, 1}},
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

    // Handle infinities and NaNs.
    if (raw_exponent == 2047) {
        StringWriter string_writer = {
            .dest = string,
            .dest_size = string_size,
            .chars_written = 0,
        };

        if ((raw_value & mantissa_mask) == 0) {
            if (is_negative) {
                string_writer_push_char(&string_writer, '-');
            }
            string_writer_push_string(&string_writer, SV("inf"));
        } else {
            string_writer_push_string(&string_writer, SV("nan"));
        }
        string_writer_push_char(&string_writer, '\0');

        return string_writer.chars_written;
    }

    isize exponent;
    u64 mantissa;
    if (raw_exponent == 0) {
        // Subnormals and zero.
        exponent = -1022 - mantissa_bits;
        mantissa = raw_value & mantissa_mask;

        if (mantissa == 0) {
            exponent = 0;
        }
    } else {
        // Normal floats.
        exponent = raw_exponent - 1023 - mantissa_bits;
        mantissa = (mantissa_mask + 1) | (raw_value & mantissa_mask);
    }

    // Should be enough memory to convert any f64 float?
    u8 arena_buffer[8 * 1024];
    Arena arena = {
        .begin = arena_buffer,
        .end = arena_buffer + 8 * 1024,
    };

    return float_format(
        (Float){is_negative, exponent, (Number){(u32[]){mantissa & 0xffffffff, mantissa >> 32}, 2}},
        string, string_size,
        params,
        arena
    );
}
