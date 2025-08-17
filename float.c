// Algorithm for printing floats:
// https://randomascii.wordpress.com/2012/03/08/float-precisionfrom-zero-to-100-digits-2/
//
// Algorithm for parsing floats:
// https://www.exploringbinary.com/correct-decimal-to-floating-point-using-big-integers/
//
// How many digits are needed for round-trip conversions between decimal and float:
// https://www.exploringbinary.com/number-of-digits-required-for-round-trip-conversions/

#include "float.h"

#include <string.h> // memcpy, memset, strncmp
#include <assert.h> // assert

static inline isize isize_min(isize left, isize right) {
    return left < right ? left : right;
}

static inline isize isize_max(isize left, isize right) {
    return left > right ? left : right;
}

#define ARENA_ALIGNMENT 16

typedef struct {
    u8 *begin;
    u8 *end;
} Arena;

static void *arena_alloc(Arena *arena, isize size) {
    if (size == 0) {
        return NULL;
    }

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

static inline StringView sv_range(char const *begin, char const *end) {
    return (StringView){
        .data = begin,
        .size = end - begin,
    };
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
        *decimal_iter = ("987654321" "0" "123456789")[9 + (integer % 10)];

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
//
// One nice thing about the "little-endian" order is that you can easily realloc the digits array to
// allow for storing larger numbers.
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

#define INFINITY_LITERAL "inf"
#define NAN_LITERAL "nan"

// Represents a value of: (is_negative ? -1 : 1) * mantissa * (2 ^ exponent)
typedef struct {
    bool is_negative;
    isize exponent;
    Number mantissa;
} FloatParts;

static inline isize float_format(
    FloatParts float_parts,
    char *string, isize string_size,
    FloatFormatParams const *params,
    Arena arena
) {
    isize integer_part_size =
        isize_max((float_parts.mantissa.size * 32 + float_parts.exponent + 31) / 32, 0);
    isize fractional_part_size =
        isize_max((0 - float_parts.exponent + 31) / 32, 0);

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
        if (float_parts.exponent >= 32) {
            fixed_iter += float_parts.exponent / 32;
        }

        u32 *mantissa_iter = float_parts.mantissa.data;
        u32 *mantissa_end = float_parts.mantissa.data + float_parts.mantissa.size;

        u32 carry = 0;
        int bit_shift = ((float_parts.exponent % 32) + 32) % 32;

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

        while (!number_is_zero(integer_part)) {
            u32 remainder;
            number_divide(integer_part, 10, integer_part_swap, &remainder);

            assert(decimal_mantissa_size < decimal_mantissa_capacity);
            decimal_mantissa[decimal_mantissa_size] = remainder;
            decimal_mantissa_size += 1;

            Number swap = integer_part_swap;
            integer_part_swap = integer_part;
            integer_part = swap;
        }

        bytes_reverse(decimal_integer_part_begin, decimal_mantissa + decimal_mantissa_size);
    }

    // Put the fractional part into the decimal mantissa.
    {
        // Truncate leading zeroes for small numbers.
        if (number_is_zero(float_parts.mantissa)) {
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

    // Exponential form (precision = 2 digits after the decimal point)
    // 01235 * 10^e => 1.24 * 10^(e + 2)
    // 09999 * 10^e => 1.00 * 10^(e + 3)
    //
    // Regular form (precision = 2 digits after the decimal point)
    // 01235 * 10^(-3) =>      1.24
    // 09999 * 10^(-3) =>     10.00
    // 01235 * 10^2    => 123500.00

    // decimal_mantissa[decimal_point_pos] is the first digit of the fractional part.
    // This might be an out of bounds index.
    isize decimal_point_pos;
    if (params->exponential) {
        decimal_point_pos = 0;

        while (
            decimal_point_pos < decimal_mantissa_size &&
            decimal_mantissa[decimal_point_pos] == 0
        ) {
            decimal_point_pos += 1;
        }

        // Put the decimal point after the first non-zero digit.
        // (Or after 0 if the whole number is 0.)
        if (decimal_point_pos < decimal_mantissa_size) {
            decimal_point_pos += 1;
        }
    } else {
        decimal_point_pos = decimal_mantissa_size + decimal_exponent;
    }

    i8 last_significant_digit = 0;
    i8 rounding_digit = 0;
    {
        isize last_significant_digit_pos = decimal_point_pos + params->precision - 1;
        if (0 <= last_significant_digit_pos && last_significant_digit_pos < decimal_mantissa_size) {
            last_significant_digit = decimal_mantissa[last_significant_digit_pos];
        }

        isize rounding_digit_pos = last_significant_digit_pos + 1;
        if (0 <= rounding_digit_pos && rounding_digit_pos < decimal_mantissa_size) {
            rounding_digit = decimal_mantissa[rounding_digit_pos];
        }
    }

    enum {
        ROUND_UP,
        ROUND_DOWN,
        ROUND_TO_EVEN,
    } rounding;

    if (rounding_digit < 5) {
        rounding = ROUND_DOWN;
    } else if (rounding_digit > 5) {
        rounding = ROUND_UP;
    } else {
        rounding = ROUND_TO_EVEN;

        // Start at right after the rounding digit.
        i8 *decimal_mantissa_iter = decimal_mantissa + decimal_point_pos + params->precision + 1;
        while (decimal_mantissa_iter < decimal_mantissa + decimal_mantissa_size) {
            if (*decimal_mantissa_iter != 0) {
                rounding = ROUND_UP;
                break;
            }

            decimal_mantissa_iter += 1;
        }
    }

    if (rounding == ROUND_UP || rounding == ROUND_TO_EVEN && last_significant_digit % 2 == 1) {
        i8 overflow = 1;

        // Start at the last digit of the fractional part.
        i8 *decimal_mantissa_iter = decimal_mantissa + decimal_point_pos + params->precision - 1;

        while (decimal_mantissa_iter >= decimal_mantissa) {
            i8 intermediate_sum = overflow + *decimal_mantissa_iter;

            *decimal_mantissa_iter = intermediate_sum % 10;
            overflow = intermediate_sum / 10;

            // We stop when there is no more overflow...
            if (overflow == 0) {
                break;
            }

            decimal_mantissa_iter -= 1;
        }

        // ...to adjust the decimal point when printing float in the exponential form:
        if (params->exponential) {
            decimal_point_pos = isize_min(
                decimal_mantissa_iter - decimal_mantissa + 1,
                decimal_point_pos
            );
        }
    }

    StringWriter string_writer = {
        .dest = string,
        .dest_size = string_size,
        .chars_written = 0,
    };

    if (float_parts.is_negative) {
        string_writer_push_char(&string_writer, '-');
    }

    // Print the integer part:
    {
        isize current_decimal_place = 0;

        // Skip the leading zeroes.
        while (
            current_decimal_place < decimal_point_pos &&
            decimal_mantissa[current_decimal_place] == 0
        ) {
            current_decimal_place += 1;
        }

        if (current_decimal_place < decimal_point_pos) {
            for (isize i = current_decimal_place; i < decimal_point_pos; i += 1) {
                i8 next_digit = i < decimal_mantissa_size ? decimal_mantissa[i] : 0;
                string_writer_push_char(&string_writer, '0' + next_digit);
            }
        } else {
            string_writer_push_char(&string_writer, '0');
        }
    }

    string_writer_push_char(&string_writer, '.');

    // Print the fractional part:
    for (isize i = 0; i < params->precision; i += 1) {
        i8 next_digit = 0;
        if (0 <= decimal_point_pos + i && decimal_point_pos + i < decimal_mantissa_size) {
            next_digit = decimal_mantissa[decimal_point_pos + i];
        }

        string_writer_push_char(&string_writer, '0' + next_digit);
    }

    // Print the exponent:
    if (params->exponential) {
        string_writer_push_char(&string_writer, 'e');
        string_writer_push_int(
            &string_writer,
            decimal_exponent + isize_max(decimal_mantissa_size - decimal_point_pos, 0)
        );
    }

    string_writer_push_char(&string_writer, '\0');
    return string_writer.chars_written;
}

isize f32_format(f32 value, char *string, isize string_size, FloatFormatParams const *params) {
    u32 raw_value;
    memcpy(&raw_value, &value, sizeof(raw_value));

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
            string_writer_push_string(&string_writer, SV(INFINITY_LITERAL));
        } else {
            string_writer_push_string(&string_writer, SV(NAN_LITERAL));
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
        (FloatParts){
            is_negative,
            exponent,
            (Number){(u32[]){mantissa}, 1},
        },
        string, string_size,
        params,
        arena
    );
}

isize f64_format(f64 value, char *string, isize string_size, FloatFormatParams const *params) {
    u64 raw_value;
    memcpy(&raw_value, &value, sizeof(raw_value));

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
            string_writer_push_string(&string_writer, SV(INFINITY_LITERAL));
        } else {
            string_writer_push_string(&string_writer, SV(NAN_LITERAL));
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
        (FloatParts){
            is_negative,
            exponent,
            (Number){(u32[]){mantissa & 0xffffffff, mantissa >> 32}, 2},
        },
        string, string_size,
        params,
        arena
    );
}

static inline bool char_is_digit(char value) {
    return '0' <= value && value <= '9';
}

typedef struct {
    StringView special;
    StringView sign;
    StringView integer_part;
    StringView fractional_part;
    StringView exponent;
} FloatLiteralParts;

static bool float_literal_into_parts(
    char const *string,
    isize string_size,
    FloatLiteralParts *parts
) {
    memset(parts, 0, sizeof(FloatLiteralParts));

    char const *string_iter = string;

    char const *string_end;
    if (string_size >= 0) {
        string_end = string + string_size;
    } else {
        string_end = string;
        while (*string_end != '\0') {
            string_end += 1;
        }
    }

    // Sign
    if (string_iter < string_end && *string_iter == '-') {
        parts->sign = sv_range(string_iter, string_iter + 1);
        string_iter += 1;
    }

    // Special values
    if (strncmp(string_iter, INFINITY_LITERAL, string_end - string_iter) == 0) {
        parts->special = sv_range(string_iter, string_end);
        return true;
    }
    if (strncmp(string_iter, NAN_LITERAL, string_end - string_iter) == 0) {
        // Don't allow "-nan" to be a valid float, because this doesn't make any sense.
        if (parts->sign.size != 0) {
            return false;
        }

        parts->special = sv_range(string_iter, string_end);
        return true;
    }

    // Integer part (required)
    if (string_iter < string_end) {
        char const *integer_part_begin = string_iter;

        if (*string_iter == '0') {
            string_iter += 1;
        } else if ('1' <= *string_iter && *string_iter <= '9') {
            string_iter += 1;

            while (string_iter < string_end) {
                if (!char_is_digit(*string_iter)) {
                    break;
                }
                string_iter += 1;
            }
        } else {
            return false;
        }

        parts->integer_part = sv_range(integer_part_begin, string_iter);
    }

    // Fractional part
    if (string_iter < string_end && *string_iter == '.') {
        string_iter += 1;

        char const *fractional_part_begin = string_iter;

        if (string_iter < string_end && char_is_digit(*string_iter)) {
            string_iter += 1;
        } else {
            return false;
        }

        while (string_iter < string_end) {
            if (!char_is_digit(*string_iter)) {
                break;
            }
            string_iter += 1;
        }

        parts->fractional_part = sv_range(fractional_part_begin, string_iter);
    }

    // Exponent
    if (string_iter < string_end && (*string_iter == 'e' || *string_iter == 'E')) {
        string_iter += 1;

        char const *exponent_begin = string_iter;

        if (string_iter < string_end && (*string_iter == '-' || *string_iter == '+')) {
            string_iter += 1;
        }

        if (string_iter < string_end && char_is_digit(*string_iter)) {
            string_iter += 1;
        } else {
            return false;
        }

        while (string_iter < string_end) {
            if (!char_is_digit(*string_iter)) {
                break;
            }
            string_iter += 1;
        }

        parts->exponent = sv_range(exponent_begin, string_iter);
    }

    return string_iter == string_end;
}

bool string_is_float(char const *string, isize string_size) {
    FloatLiteralParts float_parts;
    return float_literal_into_parts(string, string_size, &float_parts);
}

f32 f32_parse(char const *string, isize string_size, FloatLibAllocator *allocator) {
    FloatLiteralParts float_parts;
    bool string_is_float = float_literal_into_parts(string, string_size, &float_parts);
    assert(string_is_float);

    // TODO: Not implemented.
    return 0.0F;
}

f64 f64_parse(char const *string, isize string_size, FloatLibAllocator *allocator) {
    FloatLiteralParts float_parts;
    bool string_is_float = float_literal_into_parts(string, string_size, &float_parts);
    assert(string_is_float);

    // TODO: Not implemented.
    return 0.0;
}
