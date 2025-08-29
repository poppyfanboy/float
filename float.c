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

#define LOG2_10 3.3219280948873626

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
        .size = isize_max(end - begin, 0),
    };
}

static inline bool sv_equals(StringView left, StringView right) {
    if (left.size != right.size) {
        return false;
    }

    return strncmp(left.data, right.data, left.size) == 0;
}

static isize isize_parse(StringView string, bool *overflow, bool *underflow) {
    assert(string.size > 0);

    *overflow = false;
    *underflow = false;
    isize result = 0;

    const char *string_iter = string.data;
    const char *string_end = string.data + string.size;

    int result_sign = 1;
    if (*string_iter == '-') {
        result_sign = -1;
        string_iter += 1;
    }
    if (*string_iter == '+') {
        string_iter += 1;
    }

    if (result_sign > 0) {
        isize const isize_max = ((usize)0 - 1) >> 1;

        while (string_iter < string_end) {
            isize next_digit = *(string_iter++) - '0';

            if (
                result > isize_max / 10 ||
                result == isize_max / 10 && next_digit > isize_max % 10
            ) {
                *overflow = true;
                return 0;
            }

            result = 10 * result + next_digit;
        }
    } else {
        isize const isize_min = -(((usize)0 - 1) >> 1) - 1;

        while (string_iter < string_end) {
            isize next_digit = *(string_iter++) - '0';

            if (
                result < isize_min / 10 ||
                result == isize_min / 10 && -next_digit < isize_min % 10
            ) {
                *underflow = true;
                return 0;
            }

            result = 10 * result - next_digit;
        }
    }

    return result;
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

// Positive shifts are left shifts, because they shift bits into *more* significant places.
void bits_shift(u32 *words, isize word_count, isize shift) {
    if (shift >= 32 || shift <= -32) {
        isize word_shift_amount = isize_min(shift > 0 ? shift / 32 : -(shift / 32), word_count);
        isize word_copy_amount = word_count - word_shift_amount;

        if (shift > 0) {
            memmove(words + word_shift_amount, words, word_copy_amount * sizeof(u32));
            memset(words, 0, word_shift_amount * sizeof(u32));
        } else {
            memmove(words, words + word_shift_amount, word_copy_amount * sizeof(u32));
            memset(words + word_count - word_shift_amount, 0, word_shift_amount * sizeof(u32));
        }
    }

    if (shift % 32 != 0) {
        isize shift_amount = shift > 0 ? shift % 32 : -(shift % 32);

        if (shift > 0) {
            u32 overflow = 0;
            for (isize i = 0; i < word_count; i += 1) {
                u32 next_overflow = words[i] >> (32 - shift_amount);
                words[i] = (words[i] << shift_amount) | overflow;

                overflow = next_overflow;
            }
        } else {
            u32 overflow = 0;
            for (isize i = word_count - 1; i >= 0; i -= 1) {
                u32 next_overflow = words[i] << (32 - shift_amount);
                words[i] = (words[i] >> shift_amount) | overflow;

                overflow = next_overflow;
            }
        }
    }
}

inline void bits_shift_left(u32 *words, isize word_count, isize shift) {
    assert(shift >= 0);
    bits_shift(words, word_count, shift);
}

inline void bits_shift_right(u32 *words, isize word_count, isize shift) {
    assert(shift >= 0);
    bits_shift(words, word_count, -shift);
}

// Fill bit_count bits starting from starting_offset towards the more significant bits.
static inline void bits_fill(
    u32 *words,
    isize starting_offset, isize bit_count,
    int value
) {
    u32 *word_iter = &words[starting_offset / 32];

    if (bit_count > 0) {
        u32 mask = 0xffffffff;
        mask >>= 32 - isize_min(bit_count, 32);
        mask <<= starting_offset % 32;

        if (value == 0) {
            *word_iter &= ~mask;
        } else {
            *word_iter |= mask;
        }
        word_iter += 1;

        isize first_word_bits = isize_min(bit_count, 32 - starting_offset % 32);
        bit_count -= first_word_bits;
    }

    if (bit_count >= 32) {
        u8 fill_value = value == 0 ? 0 : 0xff;
        memset(word_iter, fill_value, (bit_count / 32) * sizeof(u32));

        word_iter += bit_count / 32;
        bit_count %= 32;
    }

    if (bit_count > 0) {
        u32 mask = 0xffffffff;
        mask >>= 32 - bit_count;

        if (value == 0) {
            *word_iter &= ~mask;
        } else {
            *word_iter |= mask;
        }
    }
}

void bits_set(u32 *words, isize starting_offset, isize bit_count) {
    bits_fill(words, starting_offset, bit_count, 1);
}

void bits_clear(u32 *words, isize starting_offset, isize bit_count) {
    bits_fill(words, starting_offset, bit_count, 0);
}

// Puts the result into the lower bits of the u32.
static u32 bits_read(u32 *words, isize starting_offset, int bit_count) {
    assert(0 < bit_count && bit_count <= 32);

    u64 source_data = words[starting_offset / 32];
    if (starting_offset % 32 + bit_count > 32) {
        // A clever trick could be applied here to make the code branchless, but I don't deserve it.
        source_data |= (u64)words[starting_offset / 32 + 1] << 32;
    }

    return (source_data >> starting_offset % 32) & (0xffffffffffffffff >> (64 - bit_count));
}

void bits_copy_nonoverlapping(
    u32 *source_words, isize source_starting_offset,
    u32 *dest_words, isize dest_starting_offset,
    isize bit_count
) {
    u32 *dest_iter = &dest_words[dest_starting_offset / 32];

    if (bit_count > 0) {
        isize first_word_bits = isize_min(bit_count, 32 - dest_starting_offset % 32);

        u32 source_data = bits_read(source_words, source_starting_offset, first_word_bits);

        u32 dest_clear_mask = 0xffffffff;
        dest_clear_mask >>= 32 - isize_min(bit_count, 32);
        dest_clear_mask <<= dest_starting_offset % 32;

        *dest_iter &= ~dest_clear_mask;
        *dest_iter |= source_data << dest_starting_offset % 32;
        dest_iter += 1;

        source_starting_offset += first_word_bits;
        bit_count -= first_word_bits;
    }

    while (bit_count >= 32) {
        *(dest_iter++) = bits_read(source_words, source_starting_offset, 32);

        source_starting_offset += 32;
        bit_count -= 32;
    }

    if (bit_count > 0) {
        u32 source_data = bits_read(source_words, source_starting_offset, bit_count);

        u32 dest_clear_mask = 0xffffffff;
        dest_clear_mask >>= 32 - bit_count;

        *dest_iter &= ~dest_clear_mask;
        *dest_iter |= source_data;
    }
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

static Number number_create_zero(isize size, FloatLibAllocator *allocator) {
    assert(size > 0);

    Number number = {
        .data = allocator->alloc(size * sizeof(u32), allocator->user_data),
        .size = size,
    };
    memset(number.data, 0, number.size * sizeof(u32));

    return number;
}

static void number_destroy(Number number, FloatLibAllocator *allocator) {
    allocator->dealloc(number.data, number.size * sizeof(u32), allocator->user_data);
}

static void number_resize(Number *number, isize new_size, FloatLibAllocator *allocator) {
    assert(new_size > number->size);

    Number new_number = number_create_zero(new_size, allocator);
    memcpy(new_number.data, number->data, number->size * sizeof(u32));

    number_destroy(*number, allocator);
    *number = new_number;
}

static bool number_is_zero(Number number) {
    for (isize i = 0; i < number.size; i += 1) {
        if (number.data[i] != 0) {
            return false;
        }
    }

    return true;
}

static void number_subtract(Number left, Number right, Number result, u32 *final_borrow) {
    assert(result.size >= left.size);
    assert(left.size >= right.size);

    u32 borrow = 0;
    for (isize i = 0; i < right.size; i += 1) {
        if ((u64)left.data[i] >= (u64)right.data[i] + borrow) {
            left.data[i] -= right.data[i];
            left.data[i] -= borrow;
            borrow = 0;
        } else {
            left.data[i] = ((u64)left.data[i] | ((u64)1 << 32)) - (u64)right.data[i] - (u64)borrow;
            borrow = 1;
        }
    }

    for (isize i = right.size; i < left.size; i += 1) {
        if (left.data[i] >= borrow) {
            left.data[i] -= borrow;
            borrow = 0;
            break;
        } else {
            left.data[i] = ((u64)left.data[i] | ((u64)1 << 32)) - borrow;
        }
    }

    if (final_borrow != NULL) {
        *final_borrow = borrow;
    } else {
        assert(borrow == 0);
    }
}

static void number_divide(Number dividend, u32 divisor, Number quotient, u32 *final_remainder) {
    assert(dividend.size == quotient.size);

    u32 remainder = 0;

    // Start from the most significant digits of the dividend.
    for (isize i = dividend.size - 1; i >= 0; i -= 1) {
        u64 intermediate_dividend = ((u64)remainder << 32) | dividend.data[i];

        // Intermediate quotient fits into a u32.
        //
        // Proof by contradiction. Let's say that it doesn't fit, then this means that:
        // (remainder * 2^32 + next_digit) / divisor >= 2^32
        // next_digit >= 2^32 * (divisor - remainder) >= 2^32
        // Which is a contradiction, because next_digit < 2^32.

        quotient.data[i] = intermediate_dividend / divisor;
        remainder = intermediate_dividend % divisor;
    }

    if (final_remainder != NULL) {
        *final_remainder = remainder;
    }
}

static void number_multiply(
    Number multiplicand,
    u32 multiplier,
    Number product,
    u32 *final_overflow
) {
    assert(multiplicand.size <= product.size);

    u32 overflow = 0;

    // Start from the least significant digits of the multiplicand.
    for (isize i = 0; i < multiplicand.size; i += 1) {
        u64 intermediate_product = (u64)overflow + (u64)multiplier * (u64)multiplicand.data[i];

        product.data[i] = intermediate_product & 0xffffffff;
        overflow = intermediate_product >> 32;
    }

    for (isize i = multiplicand.size; i < product.size; i += 1) {
        product.data[i] = overflow;
        overflow = 0;
    }

    if (final_overflow != NULL) {
        *final_overflow = overflow;
    } else {
        assert(overflow == 0);
    }
}

static void number_multiply_long(Number left, Number right, Number product) {
    assert(product.size >= left.size + right.size);

    if (left.size < right.size) {
        Number swap = left;
        left = right;
        right = swap;
    }
    assert(left.size >= right.size);

    for (isize right_index = 0; right_index < right.size; right_index += 1) {
        u32 overflow = 0;

        for (isize left_index = 0; left_index < left.size; left_index += 1) {
            u64 intermediate_product =
                (u64)overflow + (u64)right.data[right_index] * (u64)left.data[left_index];
            u64 intermediate_sum =
                (intermediate_product & 0xffffffff) + (u64)product.data[right_index];

            product.data[right_index + left_index] = intermediate_sum & 0xffffffff;
            overflow = (u32)(intermediate_product >> 32) + (u32)(intermediate_sum >> 32);
        }
        product.data[left.size + right_index] += overflow;
    }
}

static int numbers_compare(Number left, Number right) {
    int result_multiplier = 1;

    if (left.size < right.size) {
        Number swap = left;
        left = right;
        right = swap;
        result_multiplier = -1;
    }
    assert(left.size >= right.size);

    for (isize i = left.size - 1; i >= right.size; i -= 1) {
        if (left.data[i] != 0) {
            // left > right
            return 1 * result_multiplier;
        }
    }

    for (isize i = right.size - 1; i >= 0; i -= 1) {
        if (left.data[i] < right.data[i]) {
            return (-1) * result_multiplier;
        } else if (left.data[i] > right.data[i]) {
            return 1 * result_multiplier;
        }
    }

    return 0;
}

static void number_add(Number left_term, u32 right_term, Number sum, u32 *final_overflow) {
    assert(left_term.size == sum.size);

    u32 overflow = right_term;

    // Start from the least significant digits of the multiplicand.
    for (isize i = 0; i < left_term.size; i += 1) {
        u64 intermediate_sum = (u64)overflow + (u64)left_term.data[i];

        sum.data[i] = intermediate_sum & 0xffffffff;
        overflow = intermediate_sum >> 32;
    }

    if (final_overflow != NULL) {
        *final_overflow = overflow;
    } else {
        assert(overflow == 0);
    }
}

static void number_divide_long(
    Number dividend_input,
    Number divisor_input,
    Number quotient,
    Number remainder,
    FloatLibAllocator *allocator
) {
    assert(quotient.size >= dividend_input.size);
    assert(remainder.size >= divisor_input.size);

    if (dividend_input.size < divisor_input.size) {
        memset(quotient.data, 0, quotient.size * sizeof(u32));
        // left.size < right.size <= remainder.size, so we're not going to overflow the remainder.
        memcpy(remainder.data, dividend_input.data, dividend_input.size * sizeof(u32));
        return;
    }

    // This is needed so that we could scale both right and interemediate dividend by at most 2^31
    // and not overflow anything.
    while (divisor_input.size > 0 && divisor_input.data[divisor_input.size - 1] == 0) {
        divisor_input.size -= 1;
    }
    assert(divisor_input.size > 0);

    // Normalization step: make sure that the most significant digit of the divisor is >= 2^31.
    Number initial_dividend = number_create_zero(dividend_input.size + 1, allocator);
    memcpy(initial_dividend.data, dividend_input.data, dividend_input.size * sizeof(u32));
    Number divisor = number_create_zero(divisor_input.size, allocator);
    memcpy(divisor.data, divisor_input.data, divisor_input.size * sizeof(u32));

    isize leading_zeroes = __builtin_clz(divisor.data[divisor.size - 1]);
    bits_shift_left(initial_dividend.data, initial_dividend.size, leading_zeroes);
    bits_shift_left(divisor.data, divisor.size, leading_zeroes);

    isize quotient_index = dividend_input.size - divisor_input.size;

    // "dividend" is the intermediate thing which repeatedly gets divided by the divisor to get the
    // quotient digits.
    Number dividend = number_create_zero(divisor.size + 1, allocator);
    memcpy(dividend.data, initial_dividend.data + quotient_index, (divisor.size + 1) * sizeof(u32));

    while (quotient_index >= 0) {
        // This is an upper estimate which is greater than the real quotient by at most 2.
        u32 quotient_estimate;
        {
            u64 unclamped_estimate = (
                (u64)dividend.data[dividend.size - 1] << 32 | (u64)dividend.data[dividend.size - 2]
            ) / (u64)divisor.data[divisor.size - 1];

            u32 const u32_max = 0xffffffff;
            if (unclamped_estimate > (u64)u32_max) {
                quotient_estimate = u32_max;
            } else {
                quotient_estimate = (u32)unclamped_estimate;
            }
        }

        Number product = number_create_zero(divisor.size + 1, allocator);

        isize estimate_trial_count = 0;
        while (true) {
            memset(product.data, 0, product.size * sizeof(u32));
            number_multiply(divisor, quotient_estimate, product, NULL);

            // If the product is greater than the dividend then the estimate is obviously way to
            // large.
            if (numbers_compare(dividend, product) < 0) {
                quotient_estimate -= 1;
            } else {
                break;
            }
            estimate_trial_count += 1;
        }
        // The initial estimate is at most 2 too large compared to the actual quotient.
        assert(estimate_trial_count <= 3);

        number_destroy(product, allocator);

        number_subtract(dividend, product, dividend, NULL);
        assert(numbers_compare(dividend, divisor) < 0);

        quotient.data[quotient_index] = quotient_estimate;
        quotient_index -= 1;

        if (quotient_index >= 0) {
            bits_shift_left(dividend.data, dividend.size, 32);
            dividend.data[0] = initial_dividend.data[quotient_index];
        }
    }

    // Shift right to account for the initial normalization (the remainder DOES change when you
    // multiply both dividend and divisor by the same number, so we must scale it back to get the
    // actual remainder back).
    bits_shift_right(dividend.data, dividend.size, leading_zeroes);

    memset(remainder.data, 0, remainder.size * sizeof(u32));
    memcpy(remainder.data, dividend.data, remainder.size * sizeof(u32));

    number_destroy(initial_dividend, allocator);
    number_destroy(divisor, allocator);
    number_destroy(dividend, allocator);
}

static isize number_leading_zeroes(Number number) {
    isize leading_zeroes = 0;
    for (isize i = number.size - 1; i >= 0; i -= 1) {
        if (number.data[i] == 0) {
            leading_zeroes += 32;
        } else {
            leading_zeroes += __builtin_clz(number.data[i]);
            break;
        }
    }

    return leading_zeroes;
}

static inline bool number_is_even(Number number) {
    assert(number.size > 0);

    return number.data[0] % 2 == 0;
}

#ifndef NDEBUG

#include <stdio.h>  // printf

static Number number_parse(StringView string, FloatLibAllocator *allocator) {
    isize bit_count;
    {
        f64 bit_count_estimate = string.size * LOG2_10;
        u64 raw;
        memcpy(&raw, &bit_count_estimate, 8);
        raw += 1;
        memcpy(&bit_count_estimate, &raw, 8);

        bit_count = (isize)bit_count_estimate + 1;
    }

    Number number = number_create_zero((bit_count + 31) / 32, allocator);

    for (isize i = 0; i < string.size; i += 1) {
        assert('0' <= string.data[i] && string.data[i] <= '9');

        u32 overflow = 0;
        number_multiply(number, 10, number, &overflow);
        assert(overflow == 0);
        number_add(number, string.data[i] - '0', number, &overflow);
        assert(overflow == 0);
    }

    return number;
}

static void number_debug_print(Number input_number, FloatLibAllocator *allocator) {
    Number number = number_create_zero(input_number.size, allocator);
    memcpy(number.data, input_number.data, input_number.size * sizeof(u32));

    struct {
        char *data;
        isize size;
        isize capacity;
    } number_string;
    number_string.data = allocator->alloc(16, allocator->user_data);
    number_string.capacity = 16;
    number_string.size = 0;

    if (number_is_zero(number)) {
        number_string.data[0] = '0';
        number_string.size = 1;
    } else {
        while (!number_is_zero(number)) {
            u32 remainder;
            number_divide(number, 10, number, &remainder);

            if (number_string.size == number_string.capacity) {
                isize new_capacity = number_string.capacity * 2;
                char *new_data = allocator->alloc(new_capacity, allocator->user_data);

                memcpy(new_data, number_string.data, number_string.size);
                allocator->dealloc(
                    number_string.data,
                    number_string.capacity,
                    allocator->user_data
                );

                number_string.data = new_data;
                number_string.capacity = new_capacity;
            }

            number_string.data[number_string.size++] = remainder + '0';
        }
    }

    bytes_reverse(number_string.data, number_string.data + number_string.size);

    printf("%.*s\n", (int)number_string.size, number_string.data);

    allocator->dealloc(number_string.data, number_string.size, allocator->user_data);
    number_destroy(number, allocator);
}

#endif

#define INFINITY_LITERAL "inf"
#define NAN_LITERAL "nan"

typedef enum {
    FLOAT_KIND_REGULAR,
    FLOAT_KIND_INFINITY,
    FLOAT_KIND_NAN,
} FloatKind;

// Represents a value of: (is_negative ? -1 : 1) * mantissa * (2 ^ exponent)
typedef struct {
    FloatKind kind;

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
            number_divide(integer_part, 10, integer_part, &remainder);

            assert(decimal_mantissa_size < decimal_mantissa_capacity);
            decimal_mantissa[decimal_mantissa_size] = remainder;
            decimal_mantissa_size += 1;
        }

        bytes_reverse(decimal_integer_part_begin, decimal_mantissa + decimal_mantissa_size);
    }

    // Put the fractional part into the decimal mantissa.
    {
        // Truncate leading zeroes for small numbers.
        if (number_is_zero(float_parts.mantissa)) {
            while (!number_is_zero(fractional_part)) {
                u32 overflow;
                number_multiply(fractional_part, 10, fractional_part, &overflow);

                if (overflow > 0) {
                    break;
                }

                decimal_exponent -= 1;
            }
        }

        while (!number_is_zero(fractional_part)) {
            u32 overflow;
            number_multiply(fractional_part, 10, fractional_part, &overflow);

            assert(decimal_mantissa_size < decimal_mantissa_capacity);
            decimal_mantissa[decimal_mantissa_size] = overflow;
            decimal_mantissa_size += 1;
            decimal_exponent -= 1;
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

    // Print the special values:
    if (float_parts.kind != FLOAT_KIND_REGULAR) {
        if (float_parts.kind == FLOAT_KIND_INFINITY) {
            string_writer_push_string(&string_writer, SV(INFINITY_LITERAL));
        }
        if (float_parts.kind == FLOAT_KIND_NAN) {
            string_writer_push_string(&string_writer, SV(NAN_LITERAL));
        }

        string_writer_push_char(&string_writer, '\0');
        return string_writer.chars_written;
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
    FloatParts float_parts = {0};

    // Should be enough memory to convert any f32 float?
    u8 arena_buffer[8 * 1024];
    Arena arena = {
        .begin = arena_buffer,
        .end = arena_buffer + 8 * 1024,
    };

    u32 raw_value;
    memcpy(&raw_value, &value, sizeof(raw_value));

    bool is_negative = (raw_value & 0x80000000) != 0;
    raw_value &= 0x7fffffff;
    float_parts.is_negative = is_negative;

    // Mantissa size without an implicit 1.
    int const mantissa_bits = 23;
    u32 const mantissa_mask = 0x007fffff;

    isize raw_exponent = (isize)(raw_value >> mantissa_bits);

    // Handle infinities and NaNs.
    if (raw_exponent == 255) {
        if ((raw_value & mantissa_mask) == 0) {
            float_parts.kind = FLOAT_KIND_INFINITY;
        } else {
            float_parts.kind = FLOAT_KIND_NAN;
        }
    } else {
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

        float_parts.exponent = exponent;

        float_parts.mantissa.data = arena_alloc(&arena, 1 * sizeof(u32));
        float_parts.mantissa.data[0] = mantissa;
        float_parts.mantissa.size = 1;
    }

    return float_format(float_parts, string, string_size, params, arena);
}

isize f64_format(f64 value, char *string, isize string_size, FloatFormatParams const *params) {
    FloatParts float_parts = {0};

    // Should be enough memory to convert any f64 float?
    u8 arena_buffer[8 * 1024];
    Arena arena = {
        .begin = arena_buffer,
        .end = arena_buffer + 8 * 1024,
    };

    u64 raw_value;
    memcpy(&raw_value, &value, sizeof(raw_value));

    bool is_negative = (raw_value & 0x8000000000000000) != 0;
    raw_value &= 0x7fffffffffffffff;
    float_parts.is_negative = is_negative;

    // Mantissa size without an implicit 1.
    int const mantissa_bits = 52;
    u64 const mantissa_mask = 0xfffffffffffff;

    isize raw_exponent = (isize)(raw_value >> mantissa_bits);

    // Handle infinities and NaNs.
    if (raw_exponent == 2047) {
        if ((raw_value & mantissa_mask) == 0) {
            float_parts.kind = FLOAT_KIND_INFINITY;
        } else {
            float_parts.kind = FLOAT_KIND_NAN;
        }
    } else {
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

        float_parts.exponent = exponent;

        float_parts.mantissa.data = arena_alloc(&arena, 2 * sizeof(u32));
        float_parts.mantissa.data[0] = mantissa & 0xffffffff;
        float_parts.mantissa.data[1] = mantissa >> 32;
        float_parts.mantissa.size = 2;
    }

    return float_format(float_parts, string, string_size, params, arena);
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
    if (string_iter < string_end && (*string_iter == '-' || *string_iter == '+')) {
        parts->sign = sv_range(string_iter, string_iter + 1);
        string_iter += 1;
    }

    // Special values
    if (sv_equals(sv_range(string_iter, string_end), SV(INFINITY_LITERAL))) {
        parts->special = sv_range(string_iter, string_end);
        return true;
    }
    if (sv_equals(sv_range(string_iter, string_end), SV(NAN_LITERAL))) {
        // Don't allow "-nan" or "+nan" to be a valid float, because this doesn't make any sense.
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
    } else {
        return false;
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

void float_parse(
    char const *string, isize string_size,
    isize max_mantissa_bits,
    FloatParts *float_parts,
    FloatLibAllocator *allocator
) {
    assert(max_mantissa_bits > 0);
    assert(float_parts->mantissa.data != NULL);
    assert(float_parts->mantissa.size * 32 >= max_mantissa_bits);

    float_parts->kind = FLOAT_KIND_REGULAR;
    float_parts->is_negative = false;
    float_parts->exponent = 0;

    FloatLiteralParts float_string_parts;
    bool string_is_float = float_literal_into_parts(string, string_size, &float_string_parts);
    assert(string_is_float);

    if (sv_equals(float_string_parts.special, SV(NAN_LITERAL))) {
        float_parts->kind = FLOAT_KIND_NAN;
        return;
    }

    if (sv_equals(float_string_parts.special, SV(INFINITY_LITERAL))) {
        float_parts->kind = FLOAT_KIND_INFINITY;
        if (float_string_parts.sign.size > 0 && float_string_parts.sign.data[0] == '-') {
            float_parts->is_negative = true;
        }
        return;
    }

    bool decimal_exponent_underflow = false;
    bool decimal_exponent_overflow = false;
    isize decimal_exponent = 0;
    if (float_string_parts.exponent.size > 0) {
        decimal_exponent = isize_parse(
            float_string_parts.exponent,
            &decimal_exponent_overflow,
            &decimal_exponent_underflow
        );
    }
    if (decimal_exponent_overflow) {
        float_parts->kind = FLOAT_KIND_INFINITY;
        if (float_string_parts.sign.size > 0 && float_string_parts.sign.data[0] == '-') {
            float_parts->is_negative = true;
        }
        return;
    }

    Number mantissa = number_create_zero(1, allocator);

    char const *integer_iter = float_string_parts.integer_part.data;
    char const *integer_part_end = integer_iter + float_string_parts.integer_part.size;

    char const *fractional_iter = float_string_parts.fractional_part.data;
    char const *fractional_part_end = fractional_iter + float_string_parts.fractional_part.size;

    while (!(integer_iter == integer_part_end && fractional_iter == fractional_part_end)) {
        char next_digit_char;
        if (integer_iter != integer_part_end) {
            next_digit_char = *(integer_iter++);
        } else {
            next_digit_char = *(fractional_iter++);
            decimal_exponent -= 1;
        }

        {
            u32 overflow;
            number_multiply(mantissa, 10, mantissa, &overflow);
            if (overflow != 0) {
                isize overflow_place = mantissa.size;
                number_resize(&mantissa, 2 * mantissa.size, allocator);
                mantissa.data[overflow_place] = overflow;
            }
        }

        {
            u32 overflow;
            number_add(mantissa, next_digit_char - '0', mantissa, &overflow);
            if (overflow != 0) {
                isize overflow_place = mantissa.size;
                number_resize(&mantissa, 2 * mantissa.size, allocator);
                mantissa.data[overflow_place] = overflow;
            }
        }
    }

    for (isize i = 0; i < decimal_exponent; i += 1) {
        u32 overflow;
        number_multiply(mantissa, 10, mantissa, &overflow);
        if (overflow != 0) {
            isize overflow_place = mantissa.size;
            number_resize(&mantissa, 2 * mantissa.size, allocator);
            mantissa.data[overflow_place] = overflow;
        }
    }

    if (number_is_zero(mantissa)) {
        memset(float_parts->mantissa.data, 0, float_parts->mantissa.size * sizeof(u32));
        if (float_string_parts.sign.size > 0 && float_string_parts.sign.data[0] == '-') {
            float_parts->is_negative = true;
        }
        return;
    }

    if (decimal_exponent < 0) {
        // 2^(max_mantissa_bits - 1)
        Number power_of_two = number_create_zero(((max_mantissa_bits - 1) + 31) / 32, allocator);
        power_of_two.data[(max_mantissa_bits - 1) / 32] = 1 << ((max_mantissa_bits - 1) % 32);

        // 10^(-decimal_exponent)
        Number decimal_exponent_number = number_create_zero(
            ((-decimal_exponent) * LOG2_10 + 31.0) / 32.0,
            allocator
        );
        decimal_exponent_number.data[0] = 1;

        for (isize i = 0; i < -decimal_exponent; i += 1) {
            u32 overflow;
            number_multiply(decimal_exponent_number, 10, decimal_exponent_number, &overflow);
            if (overflow != 0) {
                isize overflow_place = mantissa.size;
                isize new_size = 2 * decimal_exponent_number.size;
                number_resize(&decimal_exponent_number, new_size, allocator);
                decimal_exponent_number.data[overflow_place] = overflow;
            }
        }

        Number product = number_create_zero(
            power_of_two.size + decimal_exponent_number.size,
            allocator
        );
        number_multiply_long(power_of_two, decimal_exponent_number, product);

        isize shift = isize_max(
            (32 * product.size - number_leading_zeroes(product)) -
                (32 * mantissa.size - number_leading_zeroes(mantissa)),
            0
        );
        // +1 to account for the potential additional shift in case we undershoot.
        number_resize(&mantissa, mantissa.size + (shift + 31 + 1) / 32, allocator);

        bits_shift_left(mantissa.data, mantissa.size, shift);
        if (numbers_compare(mantissa, product) < 0) {
            bits_shift_left(mantissa.data, mantissa.size, 1);
        }

        number_destroy(decimal_exponent_number, allocator);
        number_destroy(power_of_two, allocator);
        number_destroy(product, allocator);
        assert(false && "Not implemented");
    }

    int rounding_bit = 0;
    isize mantissa_bits = 32 * mantissa.size - number_leading_zeroes(mantissa);

    bits_copy_nonoverlapping(
        mantissa.data, isize_max(mantissa_bits - max_mantissa_bits, 0),
        float_parts->mantissa.data, isize_max(max_mantissa_bits - mantissa_bits, 0),
        isize_min(max_mantissa_bits, mantissa_bits)
    );
    bits_clear(
        mantissa.data,
        isize_max(mantissa_bits - max_mantissa_bits, 0),
        isize_min(max_mantissa_bits, mantissa_bits)
    );

    isize rounding_bit_offset = isize_max(mantissa_bits - max_mantissa_bits, 0) - 1;
    u32 rounding_bit_mask = (u32)1 << (rounding_bit_offset % 32);
    rounding_bit = (mantissa.data[rounding_bit_offset / 32] & rounding_bit_mask) == 0 ? 0 : 1;
    mantissa.data[rounding_bit_offset / 32] &= ~rounding_bit_mask;

    int sticky_bit = 0;
    for (isize i = 0; i < mantissa.size; i += 1) {
        if (mantissa.data[i] != 0) {
            sticky_bit = 1;
            break;
        }
    }

    float_parts->exponent = mantissa_bits - 1;

    if (
        rounding_bit == 1 && sticky_bit == 1 ||
        rounding_bit == 1 && sticky_bit == 0 && !number_is_even(float_parts->mantissa)
    ) {
        u32 rounded_mantissa_overflow = 0;

        number_add(float_parts->mantissa, 1, float_parts->mantissa, &rounded_mantissa_overflow);

        if (rounded_mantissa_overflow == 0) {
            isize result_mantissa_bits =
                32 * float_parts->mantissa.size - number_leading_zeroes(float_parts->mantissa);

            if (result_mantissa_bits > max_mantissa_bits) {
                bits_shift_right(float_parts->mantissa.data, float_parts->mantissa.size, 1);
                float_parts->exponent += 1;
            }
        } else {
            assert(rounded_mantissa_overflow == 1);

            bits_shift_right(float_parts->mantissa.data, float_parts->mantissa.size, 1);
            float_parts->mantissa.data[float_parts->mantissa.size - 1] |= 0x80000000;
            float_parts->exponent += 1;
        }
    }

    allocator->dealloc(mantissa.data, mantissa.size * sizeof(u32), allocator->user_data);
}

f32 f32_parse(char const *string, isize string_size, FloatLibAllocator *allocator) {
    // 23 bit is IEEE754 single-precision float mantissa size without an implicit one.
    isize ieee_mantissa_bits = 23;

    u32 mantissa_buffer[1] = {0};
    FloatParts float_parts = {
        .mantissa = {.data = mantissa_buffer, .size = 1},
    };

    // (ieee_mantissa_bits + 1) to include an implicit one.
    float_parse(string, string_size, ieee_mantissa_bits + 1, &float_parts, allocator);

    if (float_parts.kind == FLOAT_KIND_NAN) {
        return 0.0F / 0.0F;
    }
    if (float_parts.kind == FLOAT_KIND_INFINITY) {
        if (float_parts.is_negative) {
            return -1.0F / 0.0F;
        } else {
            return 1.0F / 0.0F;
        }
    }

    u32 raw_mantissa = mantissa_buffer[0];
    if (raw_mantissa == 0) {
        return 0.0F;
    }
    u32 implicit_one_mask = ~((u32)1 << (32 - __builtin_clz(raw_mantissa) - 1));
    raw_mantissa = raw_mantissa & implicit_one_mask;

    // FIXME: Handle epxonent underflow here.
    if (float_parts.exponent > 127) {
        if (float_parts.is_negative) {
            return -1.0F / 0.0F;
        } else {
            return 1.0F / 0.0F;
        }
    }
    u32 raw_exponent = 127 + float_parts.exponent;

    u32 raw_sign = 0;
    if (float_parts.is_negative) {
        raw_sign = 0x80000000;
    }

    u32 raw_value = raw_sign | raw_exponent << ieee_mantissa_bits | raw_mantissa;
    f32 value;
    memcpy(&value, &raw_value, sizeof(u32));
    return value;
}

f64 f64_parse(char const *string, isize string_size, FloatLibAllocator *allocator) {
    // 52 bit is IEEE754 double-precision float mantissa size without an implicit one.
    isize ieee_mantissa_bits = 52;

    u32 mantissa_buffer[2] = {0};
    FloatParts float_parts = {
        .mantissa = {.data = mantissa_buffer, .size = 2},
    };

    // (ieee_mantissa_bits + 1) to include an implicit one.
    float_parse(string, string_size, ieee_mantissa_bits + 1, &float_parts, allocator);

    if (float_parts.kind == FLOAT_KIND_NAN) {
        return 0.0 / 0.0;
    }
    if (float_parts.kind == FLOAT_KIND_INFINITY) {
        if (float_parts.is_negative) {
            return -1.0 / 0.0;
        } else {
            return 1.0 / 0.0;
        }
    }

    u64 raw_mantissa = (u64)mantissa_buffer[1] << 32 | (u64)mantissa_buffer[0];
    if (raw_mantissa == 0) {
        return 0.0;
    }
    u64 implicit_one_mask = ~((u64)1 << (64 - __builtin_clzll(raw_mantissa) - 1));
    raw_mantissa = raw_mantissa & implicit_one_mask;

    // FIXME: Handle epxonent underflow here.
    if (float_parts.exponent > 1023) {
        if (float_parts.is_negative) {
            return -1.0 / 0.0;
        } else {
            return 1.0 / 0.0;
        }
    }
    u64 raw_exponent = 1023 + float_parts.exponent;

    u64 raw_sign = 0;
    if (float_parts.is_negative) {
        raw_sign = 0x8000000000000000;
    }

    u64 raw_value = raw_sign | raw_exponent << ieee_mantissa_bits | raw_mantissa;
    f64 value;
    memcpy(&value, &raw_value, sizeof(u64));
    return value;
}
