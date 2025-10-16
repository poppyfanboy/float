// Algorithm for printing floats:
// https://randomascii.wordpress.com/2012/03/08/float-precisionfrom-zero-to-100-digits-2/
//
// Algorithm for parsing floats:
// https://www.exploringbinary.com/correct-decimal-to-floating-point-using-big-integers/
//
// How many digits are needed for round-trip conversions between decimal and float:
// https://www.exploringbinary.com/number-of-digits-required-for-round-trip-conversions/

#include "float.h"

#include <stddef.h> // NULL
#include <string.h> // memcpy, memmove, memset, strncmp, strlen
#include <assert.h> // assert

#define LOG2_10 3.32192809488736234787

static inline isize isize_min(isize left, isize right) {
    return left < right ? left : right;
}

static inline isize isize_max(isize left, isize right) {
    return left > right ? left : right;
}

static int u32_leading_zeroes_impl(u32 value) {
    int leading_zeroes = 0;

    if ((value & 0xffff0000) == 0) {
        leading_zeroes += 16;
        value <<= 16;
    }
    if ((value & 0xff000000) == 0) {
        leading_zeroes += 8;
        value <<= 8;
    }
    if ((value & 0xf0000000) == 0) {
        leading_zeroes += 4;
        value <<= 4;
    }
    if ((value & 0xc0000000) == 0) {
        leading_zeroes += 2;
        value <<= 2;
    }
    if ((value & 0x80000000) == 0) {
        leading_zeroes += 1;
    }

    return leading_zeroes;
}

static int u64_leading_zeroes_impl(u64 value) {
    int leading_zeroes = 0;

    if ((value & 0xffffffff00000000) == 0) {
        leading_zeroes += 32;
        value <<= 32;
    }
    if ((value & 0xffff000000000000) == 0) {
        leading_zeroes += 16;
        value <<= 16;
    }
    if ((value & 0xff00000000000000) == 0) {
        leading_zeroes += 8;
        value <<= 8;
    }
    if ((value & 0xf000000000000000) == 0) {
        leading_zeroes += 4;
        value <<= 4;
    }
    if ((value & 0xc000000000000000) == 0) {
        leading_zeroes += 2;
        value <<= 2;
    }
    if ((value & 0x8000000000000000) == 0) {
        leading_zeroes += 1;
    }

    return leading_zeroes;
}

#if defined(__GNUC__) || defined(__GNUG__)
    #define u32_leading_zeroes __builtin_clz
    #define u64_leading_zeroes __builtin_clzll
#elif defined(_MSC_VER)
    #define u32_leading_zeroes __lzcnt
    #define u64_leading_zeroes __lzcnt64
#else
    #define u32_leading_zeroes u32_leading_zeroes_impl
    #define u64_leading_zeroes u64_leading_zeroes_impl
#endif

static inline void *alloc(FloatLibAllocator *allocator, isize size) {
    return allocator->alloc(size, allocator->user_data);
}

static inline void dealloc(FloatLibAllocator *allocator, void *ptr, isize size) {
    allocator->dealloc(ptr, size, allocator->user_data);
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
    assert(arena->end - arena->begin >= size + padding);

    void *ptr = arena->begin + padding;
    arena->begin += padding + size;

    return ptr;
}

static void bytes_reverse(void *begin, void *end) {
    u8 *forward_iter = begin;
    u8 *backward_iter = (u8 *)end - 1;

    while (forward_iter < backward_iter) {
        u8 swap = *forward_iter;
        *forward_iter = *backward_iter;
        *backward_iter = swap;

        forward_iter += 1;
        backward_iter -= 1;
    }
}

typedef struct {
    char const *data;
    isize size;
} StringView;

#define SV(string_literal)                  \
    (StringView){                           \
        .data = string_literal,             \
        .size = sizeof(string_literal) - 1, \
    }

static inline StringView sv_range(char const *begin, char const *end) {
    return (StringView){
        .data = begin,
        .size = end - begin,
    };
}

static inline bool sv_equals(StringView left, StringView right) {
    if (left.size != right.size) {
        return false;
    }

    return strncmp(left.data, right.data, left.size) == 0;
}

// The whole string must be a valid integer representation. Empty strings are not allowed.
static isize isize_parse(StringView string, bool *overflow, bool *underflow) {
    *overflow = false;
    *underflow = false;
    isize result = 0;

    const char *string_iter = string.data;
    const char *string_end = string.data + string.size;

    int result_sign = 1;
    if (*string_iter == '-') {
        result_sign = -1;
        string_iter += 1;
    } else if (*string_iter == '+') {
        string_iter += 1;
    }

    if (result_sign > 0) {
        isize const ISIZE_MAX = ((usize)0 - 1) >> 1;

        while (string_iter < string_end) {
            isize next_digit = *(string_iter++) - '0';

            if (
                result > ISIZE_MAX / 10 ||
                result == ISIZE_MAX / 10 && next_digit > ISIZE_MAX % 10
            ) {
                *overflow = true;
                return 0;
            }

            result = 10 * result + next_digit;
        }
    } else {
        isize const ISIZE_MIN = -(isize)(((usize)0 - 1) >> 1) - 1;

        while (string_iter < string_end) {
            isize next_digit = *(string_iter++) - '0';

            if (
                result < ISIZE_MIN / 10 ||
                result == ISIZE_MIN / 10 && -next_digit < ISIZE_MIN % 10
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

static void string_writer_push_char(StringWriter *writer, char character) {
    if (writer->dest != NULL) {
        assert(writer->chars_written + 1 <= writer->dest_size);
        writer->dest[writer->chars_written] = character;
    }

    writer->chars_written += 1;
}

static void string_writer_push_string(StringWriter *writer, StringView string) {
    if (writer->dest != NULL) {
        assert(writer->chars_written + string.size <= writer->dest_size);
        memcpy(writer->dest + writer->chars_written, string.data, string.size);
    }

    writer->chars_written += string.size;
}

static void string_writer_push_int(StringWriter *writer, isize integer) {
    if (integer < 0) {
        string_writer_push_char(writer, '-');
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

    if (writer->dest != NULL) {
        assert(writer->chars_written + decimal_size <= writer->dest_size);

        // Write the decimal number in reverse.
        for (isize i = 0; i < decimal_size; i += 1) {
            decimal_iter -= 1;
            writer->dest[writer->chars_written + i] = *decimal_iter;
        }
    }

    writer->chars_written += decimal_size;
}

// Positive shifts are left shifts, because they shift bits into *more* significant places.
static void bits_shift(u32 *words, isize word_count, isize shift) {
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

static inline void bits_shift_left(u32 *words, isize word_count, isize shift) {
    assert(shift >= 0);
    bits_shift(words, word_count, shift);
}

static inline void bits_shift_right(u32 *words, isize word_count, isize shift) {
    assert(shift >= 0);
    bits_shift(words, word_count, -shift);
}

// Fill bit_count bits starting from starting_offset towards the more significant bits.
static inline void bits_fill(u32 *words, isize starting_offset, isize bit_count, int value) {
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

static void bits_set(u32 *words, isize starting_offset, isize bit_count) {
    bits_fill(words, starting_offset, bit_count, 1);
}

static void bits_clear(u32 *words, isize starting_offset, isize bit_count) {
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

static void bits_copy_nonoverlapping(
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
// Zero-sized words are not allowed.
//
// One nice thing about the "little-endian" order is that you can easily realloc the digits array to
// allow for storing larger numbers.
typedef struct {
    u32 *words;
    isize word_count;
} Number;

// Given the maximum amount of decimal digits in a number, calculates an upper estimate of how many
// "words" are needed to store this number in its binary form.
//
// For example, the largest number representable with 10 decimal digits is "9999999999", which is
// "00000010 01010100 00001011 11100011 11111111" in binary. Which means that we will need 2 32-bit
// "words" to store this number.
static isize max_number_words_for_decimal_digits(isize max_decimal_digits) {
    // bit_count = floor(log2(10^max_decimal_digits)) + 1
    f64 bit_count_estimate = max_decimal_digits * LOG2_10;

    // Take the next float, so that we get an upper estimate?
    {
        u64 raw;
        memcpy(&raw, &bit_count_estimate, sizeof(f64));
        raw += 1;
        memcpy(&bit_count_estimate, &raw, sizeof(f64));
    }

    isize bit_count = (isize)bit_count_estimate + 1;
    return (bit_count + 31) / 32;
}

static Number number_create_zero(isize word_count, FloatLibAllocator *allocator) {
    assert(word_count > 0);

    Number number = {
        .words = alloc(allocator, word_count * sizeof(u32)),
        .word_count = word_count,
    };
    memset(number.words, 0, number.word_count * sizeof(u32));

    return number;
}

static void number_destroy(Number number, FloatLibAllocator *allocator) {
    dealloc(allocator, number.words, number.word_count * sizeof(u32));
}

static void number_resize(Number *number, isize new_size, FloatLibAllocator *allocator) {
    assert(new_size > number->word_count);

    Number new_number = number_create_zero(new_size, allocator);
    memcpy(new_number.words, number->words, number->word_count * sizeof(u32));

    number_destroy(*number, allocator);
    *number = new_number;
}

static bool number_is_zero(Number number) {
    for (isize i = 0; i < number.word_count; i += 1) {
        if (number.words[i] != 0) {
            return false;
        }
    }

    return true;
}

static int numbers_compare(Number left, Number right);

// https://en.wikipedia.org/wiki/Subtraction
// The number being subtracted is the *subtrahend*, while the number it is subtracted from is the
// *minuend*. The result is the *difference*.
static void number_subtract_long(
    Number minuend,
    Number subtrahend,
    Number difference,
    u32 *final_borrow
) {
    assert(difference.word_count >= minuend.word_count);    // worst case is subtracting 0
    assert(numbers_compare(minuend, subtrahend) >= 0);

    u32 borrow = 0;

    for (isize i = 0; i < minuend.word_count; i += 1) {
        u64 intermediate_subtrahend = i < subtrahend.word_count ? subtrahend.words[i] : 0;

        if (minuend.words[i] >= intermediate_subtrahend + borrow) {
            u64 intermediate_minuend = minuend.words[i];

            difference.words[i] = intermediate_minuend - intermediate_subtrahend - borrow;
            borrow = 0;
        } else {
            u64 intermediate_minuend = (u64)minuend.words[i] | ((u64)1 << 32);

            difference.words[i] = intermediate_minuend - intermediate_subtrahend - borrow;
            borrow = 1;
        }
    }

    for (isize i = minuend.word_count; i < difference.word_count; i += 1) {
        difference.words[i] = 0;
    }

    if (final_borrow != NULL) {
        *final_borrow = borrow;
    } else {
        assert(borrow == 0);
    }
}

static void number_divide(Number dividend, u32 divisor, Number quotient, u32 *final_remainder) {
    assert(quotient.word_count >= dividend.word_count); // worst case is dividing by 1

    // Be careful here, you can't just memset the whole quotient to zero in the beginning of the
    // function, because both dividend and quotient could be the same Number.
    for (isize i = quotient.word_count - 1; i >= dividend.word_count; i -= 1) {
        quotient.words[i] = 0;
    }

    u32 remainder = 0;

    // Start from the most significant digits of the dividend.
    for (isize i = dividend.word_count - 1; i >= 0; i -= 1) {
        u64 intermediate_dividend = ((u64)remainder << 32) | dividend.words[i];

        // Intermediate quotient fits into a u32.
        //
        // Proof by contradiction. Let's say that it doesn't fit, then this means that:
        // (remainder * 2^32 + next_digit) / divisor >= 2^32
        // next_digit >= 2^32 * (divisor - remainder) >= 2^32
        // Which is a contradiction, because next_digit < 2^32.

        quotient.words[i] = intermediate_dividend / divisor;
        remainder = intermediate_dividend % divisor;
    }

    if (final_remainder != NULL) {
        *final_remainder = remainder;
    } else {
        // When doing integer division you don't necessarily care about the remainder.
    }
}

static void number_multiply(Number multiplicand, u32 multiplier, Number product, u32 *final_carry) {
    assert(product.word_count >= multiplicand.word_count);  // worst case is multiplying by 1

    u32 carry = 0;

    // Start from the least significant digits of the multiplicand.
    for (isize i = 0; i < product.word_count; i += 1) {
        u64 intermediate_multiplicand = i < multiplicand.word_count ? multiplicand.words[i] : 0;
        u64 intermediate_product = intermediate_multiplicand * multiplier + carry;

        product.words[i] = intermediate_product & 0xffffffff;
        carry = intermediate_product >> 32;
    }

    if (final_carry != NULL) {
        *final_carry = carry;
    } else {
        assert(carry == 0);
    }
}

static void number_multiply_long(Number left, Number right, Number product) {
    assert(product.word_count >= left.word_count + right.word_count);

    assert(product.words != left.words && product.words != right.words);
    memset(product.words, 0, product.word_count * sizeof(u32));

    if (left.word_count < right.word_count) {
        Number swap = left;
        left = right;
        right = swap;
    }

    for (isize right_index = 0; right_index < right.word_count; right_index += 1) {
        u32 carry = 0;

        for (isize left_index = 0; left_index < left.word_count; left_index += 1) {
            u64 intermediate_product =
                (u64)left.words[left_index] * right.words[right_index] + carry;
            u64 intermediate_sum =
                (intermediate_product & 0xffffffff) + (u64)product.words[right_index];

            product.words[right_index + left_index] = intermediate_sum & 0xffffffff;
            carry = (intermediate_product >> 32) + (intermediate_sum >> 32);
        }

        product.words[left.word_count + right_index] += carry;
    }
}

static int numbers_compare(Number left, Number right) {
    int result_multiplier = 1;
    if (left.word_count < right.word_count) {
        Number swap = left;
        left = right;
        right = swap;
        result_multiplier = -1;
    }

    for (isize i = left.word_count - 1; i >= right.word_count; i -= 1) {
        if (left.words[i] != 0) {
            // left > right
            return 1 * result_multiplier;
        }
    }

    for (isize i = right.word_count - 1; i >= 0; i -= 1) {
        if (left.words[i] < right.words[i]) {
            return (-1) * result_multiplier;
        } else if (left.words[i] > right.words[i]) {
            return 1 * result_multiplier;
        }
    }

    return 0;
}

static void number_add(Number left, u32 right, Number sum, u32 *final_carry) {
    assert(sum.word_count >= left.word_count);  // worst case is adding 0

    u32 carry = right;

    // Start from the least significant digits of the multiplicand.
    for (isize i = 0; i < sum.word_count; i += 1) {
        u64 intermediate_sum = (i < left.word_count ? (u64)left.words[i] : 0) + carry;

        sum.words[i] = intermediate_sum & 0xffffffff;
        carry = intermediate_sum >> 32;
    }

    if (final_carry != NULL) {
        *final_carry = carry;
    } else {
        assert(carry == 0);
    }
}

static void number_divide_long(
    Number dividend_input,
    Number divisor_input,
    Number quotient,
    Number remainder,
    FloatLibAllocator *allocator
) {
    assert(quotient.word_count >= dividend_input.word_count);
    assert(remainder.word_count >= divisor_input.word_count);

    if (numbers_compare(dividend_input, divisor_input) < 0) {
        memset(quotient.words, 0, quotient.word_count * sizeof(u32));

        // dividend size < divisor size <= remainder size, so the result will fit into remainder.
        memset(remainder.words, 0, remainder.word_count * sizeof(u32));
        memcpy(remainder.words, dividend_input.words, remainder.word_count * sizeof(u32));

        return;
    }

    // Normalization step: make sure that the most significant digit of the divisor is >= 2^31.

    // Skip the leading zeroes in the divisor.
    while (divisor_input.word_count > 0 && divisor_input.words[divisor_input.word_count - 1] == 0) {
        divisor_input.word_count -= 1;
    }
    assert(divisor_input.word_count > 0);

    // Thanks to getting rid of the leading zeroes in the divisor, we're going to need at most one
    // additional digit for the normalized dividend.
    Number dividend_normalized = number_create_zero(dividend_input.word_count + 1, allocator);
    memcpy(
        dividend_normalized.words,
        dividend_input.words,
        dividend_input.word_count * sizeof(u32)
    );

    Number divisor_normalized = number_create_zero(divisor_input.word_count, allocator);
    memcpy(
        divisor_normalized.words,
        divisor_input.words,
        divisor_input.word_count * sizeof(u32)
    );

    isize normalization_shift =
        u32_leading_zeroes(divisor_normalized.words[divisor_normalized.word_count - 1]);

    bits_shift_left(dividend_normalized.words, dividend_normalized.word_count, normalization_shift);
    bits_shift_left(divisor_normalized.words, divisor_normalized.word_count, normalization_shift);

    // Generate quotient digits starting from the most significant ones.
    isize quotient_index = dividend_normalized.word_count - divisor_normalized.word_count;

    // Dividing an (M+N)-digit number by an N-digit number comes down to a series of divisions of
    // (N+1)-digit numbers by the N-digit number.
    Number dividend = number_create_zero(divisor_normalized.word_count + 1, allocator);
    memcpy(
        dividend.words,
        dividend_normalized.words + quotient_index,
        (dividend_normalized.word_count - quotient_index) * sizeof(u32)
    );

    Number divisor_times_estimate = number_create_zero(dividend_normalized.word_count, allocator);

    while (quotient_index >= 0) {
        // This is an upper estimate which is greater than the real quotient by at most 2.
        // See Knuth (Book 2 - Chapter 4.3.1) for the details.
        u32 quotient_estimate;
        {
            // Take two most significant digits from the dividend...
            u64 unclamped_estimate =
                (u64)dividend.words[dividend.word_count - 1] << 32 |
                (u64)dividend.words[dividend.word_count - 2];
            // ...and divide them by the most significant digit from the divisor.
            unclamped_estimate /= divisor_normalized.words[divisor_normalized.word_count - 1];

            u32 const u32_max = 0xffffffff;
            if (unclamped_estimate > u32_max) {
                quotient_estimate = u32_max;
            } else {
                quotient_estimate = unclamped_estimate;
            }
        }

        for (int trial = 0; trial < 3; trial += 1) {
            number_multiply(divisor_normalized, quotient_estimate, divisor_times_estimate, NULL);

            // divisor * quotient estimate > dividend means that the estimate is too large.
            if (numbers_compare(divisor_times_estimate, dividend) > 0) {
                // The initial estimate is at most 2 too large compared to the actual quotient.
                assert(trial != 2);

                quotient_estimate -= 1;
            } else {
                break;
            }
        }

        number_subtract_long(dividend, divisor_times_estimate, dividend, NULL);
        assert(numbers_compare(dividend, divisor_normalized) < 0);

        quotient.words[quotient_index] = quotient_estimate;
        quotient_index -= 1;

        // We don't need to pull the next digit from the dividend at the last step.
        if (quotient_index >= 0) {
            bits_shift_left(dividend.words, dividend.word_count, 32);
            dividend.words[0] = dividend_normalized.words[quotient_index];
        }
    }

    // Shift right to account for the initial normalization (the remainder *does* change when you
    // multiply both dividend and divisor by the same number, so we must scale it back to get the
    // actual remainder back).
    bits_shift_right(dividend.words, dividend.word_count, normalization_shift);

    memset(remainder.words, 0, remainder.word_count * sizeof(u32));
    memcpy(
        remainder.words,
        dividend.words,
        isize_min(remainder.word_count, dividend.word_count) * sizeof(u32)
    );

    number_destroy(divisor_times_estimate, allocator);
    number_destroy(dividend, allocator);
    number_destroy(divisor_normalized, allocator);
    number_destroy(dividend_normalized, allocator);
}

static isize number_leading_zeroes(Number number) {
    isize leading_zeroes = 0;
    for (isize i = number.word_count - 1; i >= 0; i -= 1) {
        if (number.words[i] == 0) {
            leading_zeroes += 32;
        } else {
            leading_zeroes += u32_leading_zeroes(number.words[i]);
            break;
        }
    }

    return leading_zeroes;
}

static inline bool number_is_even(Number number) {
    assert(number.word_count > 0);

    return number.words[0] % 2 == 0;
}

#ifndef NDEBUG

#include <stdio.h>  // printf

static Number number_parse(StringView string, FloatLibAllocator *allocator) {
    Number number = number_create_zero(max_number_words_for_decimal_digits(string.size), allocator);

    for (isize i = 0; i < string.size; i += 1) {
        assert('0' <= string.data[i] && string.data[i] <= '9');

        number_multiply(number, 10, number, NULL);
        number_add(number, string.data[i] - '0', number, NULL);
    }

    return number;
}

static void number_debug_print(Number input_number, FloatLibAllocator *allocator) {
    Number number = number_create_zero(input_number.word_count, allocator);
    memcpy(number.words, input_number.words, input_number.word_count * sizeof(u32));

    typedef struct {
        char *data;
        isize size;
        isize capacity;
    } String;

    String number_string = {
        .data = alloc(allocator, 16),
        .size = 0,
        .capacity = 16,
    };

    if (number_is_zero(number)) {
        number_string.data[number_string.size++] = '0';
    }

    while (!number_is_zero(number)) {
        u32 remainder;
        number_divide(number, 10, number, &remainder);

        if (number_string.size == number_string.capacity) {
            isize new_capacity = number_string.capacity * 2;
            char *new_data = alloc(allocator, new_capacity);

            memcpy(new_data, number_string.data, number_string.size);
            dealloc(allocator, number_string.data, number_string.capacity);

            number_string.data = new_data;
            number_string.capacity = new_capacity;
        }

        number_string.data[number_string.size++] = remainder + '0';
    }
    bytes_reverse(number_string.data, number_string.data + number_string.size);

    printf("%.*s", (int)number_string.size, number_string.data);

    dealloc(allocator, number_string.data, number_string.size);
    number_destroy(number, allocator);
}

#endif // NDEBUG

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
        isize_max((float_parts.mantissa.word_count * 32 + float_parts.exponent + 31) / 32, 0);
    isize fractional_part_size =
        isize_max((0 - float_parts.exponent + 31) / 32, 0);

    u32 *fixed = arena_alloc(&arena, (integer_part_size + fractional_part_size) * sizeof(u32));
    memset(fixed, 0, (integer_part_size + fractional_part_size) * sizeof(u32));

    Number integer_part = {
        .words = fixed + fractional_part_size,
        .word_count = integer_part_size,
    };
    Number fractional_part = {
        .words = fixed,
        .word_count = fractional_part_size,
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

        // Skip the zeroes of the integer part, when the exponent is large:
        if (float_parts.exponent >= 32) {
            fixed_iter += float_parts.exponent / 32;
        }

        bits_copy_nonoverlapping(
            float_parts.mantissa.words, 0,
            fixed_iter, ((float_parts.exponent % 32) + 32) % 32,
            float_parts.mantissa.word_count * 32
        );
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
    i8 *decimal_integer_part_begin = decimal_mantissa + decimal_mantissa_size;

    while (!number_is_zero(integer_part)) {
        u32 remainder;
        number_divide(integer_part, 10, integer_part, &remainder);

        assert(decimal_mantissa_size < decimal_mantissa_capacity);
        decimal_mantissa[decimal_mantissa_size] = remainder;
        decimal_mantissa_size += 1;
    }

    bytes_reverse(decimal_integer_part_begin, decimal_mantissa + decimal_mantissa_size);

    // Put the fractional part into the decimal mantissa.
    while (!number_is_zero(fractional_part)) {
        u32 carry;
        number_multiply(fractional_part, 10, fractional_part, &carry);

        assert(decimal_mantissa_size < decimal_mantissa_capacity);
        decimal_mantissa[decimal_mantissa_size] = carry;
        decimal_mantissa_size += 1;
        decimal_exponent -= 1;
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
        i8 carry = 1;

        // Start at the last digit of the fractional part.
        i8 *decimal_mantissa_iter = decimal_mantissa + decimal_point_pos + params->precision - 1;

        while (decimal_mantissa_iter >= decimal_mantissa) {
            i8 intermediate_sum = carry + *decimal_mantissa_iter;

            *decimal_mantissa_iter = intermediate_sum % 10;
            carry = intermediate_sum / 10;

            // We stop when there is no more carry...
            if (carry == 0) {
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

        float_parts.mantissa.words = arena_alloc(&arena, 1 * sizeof(u32));
        float_parts.mantissa.words[0] = mantissa;
        float_parts.mantissa.word_count = 1;
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

        float_parts.mantissa.words = arena_alloc(&arena, 2 * sizeof(u32));
        float_parts.mantissa.words[0] = mantissa & 0xffffffff;
        float_parts.mantissa.words[1] = mantissa >> 32;
        float_parts.mantissa.word_count = 2;
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
        string_end = string + strlen(string);
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

bool string_represents_float(char const *string, isize string_size) {
    FloatLiteralParts float_parts;
    return float_literal_into_parts(string, string_size, &float_parts);
}

void float_parse(
    char const *string, isize string_size,
    isize max_mantissa_bits, isize min_exponent,
    FloatParts *float_parts,
    FloatLibAllocator *allocator
) {
    assert(max_mantissa_bits > 0);
    assert(float_parts->mantissa.word_count * 32 >= max_mantissa_bits);

    float_parts->kind = FLOAT_KIND_REGULAR;
    float_parts->is_negative = false;
    float_parts->exponent = 0;

    FloatLiteralParts float_string_parts;
    bool string_is_float = float_literal_into_parts(string, string_size, &float_string_parts);
    assert(string_is_float);

    if (float_string_parts.sign.size > 0 && float_string_parts.sign.data[0] == '-') {
        float_parts->is_negative = true;
    }

    if (sv_equals(float_string_parts.special, SV(NAN_LITERAL))) {
        float_parts->kind = FLOAT_KIND_NAN;
        return;
    }

    if (sv_equals(float_string_parts.special, SV(INFINITY_LITERAL))) {
        float_parts->kind = FLOAT_KIND_INFINITY;
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
        return;
    }
    if (decimal_exponent_underflow) {
        memset(float_parts->mantissa.words, 0, float_parts->mantissa.word_count * sizeof(u32));
        float_parts->exponent = 0;
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
            u32 carry;
            number_multiply(mantissa, 10, mantissa, &carry);
            if (carry != 0) {
                isize carry_place = mantissa.word_count;
                number_resize(&mantissa, 2 * mantissa.word_count, allocator);
                mantissa.words[carry_place] = carry;
            }
        }
        {
            u32 carry;
            number_add(mantissa, next_digit_char - '0', mantissa, &carry);
            if (carry != 0) {
                isize carry_place = mantissa.word_count;
                number_resize(&mantissa, 2 * mantissa.word_count, allocator);
                mantissa.words[carry_place] = carry;
            }
        }
    }

    for (isize i = 0; i < decimal_exponent; i += 1) {
        u32 carry;
        number_multiply(mantissa, 10, mantissa, &carry);
        if (carry != 0) {
            isize carry_place = mantissa.word_count;
            number_resize(&mantissa, 2 * mantissa.word_count, allocator);
            mantissa.words[carry_place] = carry;
        }
    }

    if (number_is_zero(mantissa)) {
        memset(float_parts->mantissa.words, 0, float_parts->mantissa.word_count * sizeof(u32));
        if (float_string_parts.sign.size > 0 && float_string_parts.sign.data[0] == '-') {
            float_parts->is_negative = true;
        }
        return;
    }

    if (decimal_exponent < 0) {
        // At this point we have a rational number mantissa / 10^(-decimal_exponent) which
        // represents the input number. We need to scale it up to at least the value of min_mantissa
        // to get enough binary digits to fill up the max_mantissa_bits worth of bits.

        // 10^(-decimal_exponent)
        Number denominator = number_create_zero(
            max_number_words_for_decimal_digits(-decimal_exponent),
            allocator
        );
        denominator.words[0] = 1;
        for (isize i = 0; i < -decimal_exponent; i += 1) {
            number_multiply(denominator, 10, denominator, NULL);
        }

        // 2^(max_mantissa_bits - 1)
        Number min_mantissa = number_create_zero(((max_mantissa_bits - 1) + 31) / 32, allocator);
        min_mantissa.words[(max_mantissa_bits - 1) / 32] = 1 << ((max_mantissa_bits - 1) % 32);

        Number min_numerator = number_create_zero(
            min_mantissa.word_count + denominator.word_count,
            allocator
        );
        number_multiply_long(min_mantissa, denominator, min_numerator);

        isize scaling_shift =
            (32 * min_numerator.word_count - number_leading_zeroes(min_numerator)) -
            (32 * mantissa.word_count - number_leading_zeroes(mantissa));
        scaling_shift = isize_min(isize_max(scaling_shift, 0), -min_exponent);

        // +1 to account for the potential additional shift in case we undershoot.
        number_resize(&mantissa, mantissa.word_count + (scaling_shift + 31 + 1) / 32, allocator);

        bits_shift_left(mantissa.words, mantissa.word_count, scaling_shift);
        if (numbers_compare(mantissa, min_numerator) < 0 && scaling_shift < -min_exponent) {
            bits_shift_left(mantissa.words, mantissa.word_count, 1);
            scaling_shift += 1;
        }

        // After scaling mantissa up into the 2^(max_mantissa_bits - 1)..2^max_mantissa_bits what is
        // left is to divide the scaled mantissa by the 10^(-decimal_exponent).

        // +1, so that we don't overflow in case of rounding up.
        Number quotient = number_create_zero(mantissa.word_count + 1, allocator);
        Number remainder = number_create_zero(denominator.word_count, allocator);
        number_divide_long(mantissa, denominator, quotient, remainder, allocator);

        Number half_denominator = denominator;
        number_divide(denominator, 2, half_denominator, NULL);

        int rounding_direction = numbers_compare(remainder, half_denominator);
        if (rounding_direction > 0 || rounding_direction == 0 && !number_is_even(quotient)) {
            number_add(quotient, 1, quotient, NULL);

            if (32 * quotient.word_count - number_leading_zeroes(quotient) > max_mantissa_bits) {
                bits_shift_right(quotient.words, quotient.word_count, 1);
                scaling_shift -= 1;
            }
        }

        bits_copy_nonoverlapping(
            quotient.words, 0,
            float_parts->mantissa.words, 0,
            max_mantissa_bits
        );
        float_parts->exponent = -scaling_shift;

        number_destroy(remainder, allocator);
        number_destroy(quotient, allocator);
        number_destroy(min_numerator, allocator);
        number_destroy(min_mantissa, allocator);
        number_destroy(denominator, allocator);
        number_destroy(mantissa, allocator);
        return;
    }

    int rounding_bit = 0;
    isize mantissa_bits = 32 * mantissa.word_count - number_leading_zeroes(mantissa);

    bits_copy_nonoverlapping(
        mantissa.words, isize_max(mantissa_bits - max_mantissa_bits, 0),
        float_parts->mantissa.words, isize_max(max_mantissa_bits - mantissa_bits, 0),
        isize_min(max_mantissa_bits, mantissa_bits)
    );
    bits_clear(
        mantissa.words,
        isize_max(mantissa_bits - max_mantissa_bits, 0),
        isize_min(max_mantissa_bits, mantissa_bits)
    );

    isize rounding_bit_offset = isize_max(mantissa_bits - max_mantissa_bits, 0) - 1;
    u32 rounding_bit_mask = (u32)1 << (rounding_bit_offset % 32);
    rounding_bit = (mantissa.words[rounding_bit_offset / 32] & rounding_bit_mask) == 0 ? 0 : 1;
    mantissa.words[rounding_bit_offset / 32] &= ~rounding_bit_mask;

    int sticky_bit = 0;
    for (isize i = 0; i < mantissa.word_count; i += 1) {
        if (mantissa.words[i] != 0) {
            sticky_bit = 1;
            break;
        }
    }

    float_parts->exponent = mantissa_bits - max_mantissa_bits;

    if (
        rounding_bit == 1 && sticky_bit == 1 ||
        rounding_bit == 1 && sticky_bit == 0 && !number_is_even(float_parts->mantissa)
    ) {
        u32 carry = 0;

        number_add(float_parts->mantissa, 1, float_parts->mantissa, &carry);

        if (carry == 0) {
            isize rounded_mantissa_bits =
                32 * float_parts->mantissa.word_count -
                number_leading_zeroes(float_parts->mantissa);

            if (rounded_mantissa_bits > max_mantissa_bits) {
                bits_shift_right(float_parts->mantissa.words, float_parts->mantissa.word_count, 1);
                float_parts->exponent += 1;
            }
        } else {
            // This might happen when max_mantissa_bits is a multiple of 32 and we got an overflow.
            assert(carry == 1);
            bits_shift_right(float_parts->mantissa.words, float_parts->mantissa.word_count, 1);
            float_parts->mantissa.words[float_parts->mantissa.word_count - 1] |= 0x80000000;
            float_parts->exponent += 1;
        }
    }

    number_destroy(mantissa, allocator);
}

f32 f32_parse(char const *string, isize string_size, FloatLibAllocator *allocator) {

    isize const min_exponent = -126;
    isize const max_exponent = 127;

    // 23 bit is IEEE 754 single-precision float mantissa size without an implicit one.
    isize ieee_mantissa_bits = 23;

    u32 mantissa_buffer[1] = {0};
    FloatParts float_parts = {
        .mantissa = {.words = mantissa_buffer, .word_count = 1},
    };

    // (ieee_mantissa_bits + 1) to include an implicit one.
    float_parse(
        string, string_size,
        ieee_mantissa_bits + 1, min_exponent - ieee_mantissa_bits,
        &float_parts,
        allocator
    );

    if (float_parts.kind == FLOAT_KIND_NAN) {
        f32 nan;
        memcpy(&nan, &(u32){0x7f800001}, sizeof(u32));
        return nan;
    }
    if (float_parts.kind == FLOAT_KIND_INFINITY) {
        f32 infinity;
        memcpy(&infinity, &(u32){0x7f800000}, sizeof(u32));

        if (float_parts.is_negative) {
            return -infinity;
        } else {
            return infinity;
        }
    }

    u32 raw_mantissa = mantissa_buffer[0];
    if (raw_mantissa == 0) {
        return 0.0F;
    }

    u32 const raw_exponent_bias = 127;
    u32 raw_exponent;

    // This puts the binary point right after the leftmost set bit.
    isize exponent = float_parts.exponent + ieee_mantissa_bits;

    if (exponent > max_exponent) {
        raw_mantissa = 0;
        raw_exponent = raw_exponent_bias + max_exponent + 1;
    } else {
        if (32 - u32_leading_zeroes(raw_mantissa) > ieee_mantissa_bits) {
            u32 implicit_one_mask = ~((u32)1 << (32 - u32_leading_zeroes(raw_mantissa) - 1));
            raw_mantissa = raw_mantissa & implicit_one_mask;
            raw_exponent = raw_exponent_bias + exponent;
        } else {
            raw_exponent = raw_exponent_bias + exponent - 1;
        }
    }

    assert(32 - u32_leading_zeroes(raw_mantissa) <= ieee_mantissa_bits);

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
    isize const min_exponent = -1022;
    isize const max_exponent = 1023;

    // 52 bit is IEEE 754 double-precision float mantissa size without an implicit one.
    isize ieee_mantissa_bits = 52;

    u32 mantissa_buffer[2] = {0};
    FloatParts float_parts = {
        .mantissa = {.words = mantissa_buffer, .word_count = 2},
    };

    // (ieee_mantissa_bits + 1) to include an implicit one.
    float_parse(
        string,
        string_size,
        ieee_mantissa_bits + 1, min_exponent - ieee_mantissa_bits,
        &float_parts,
        allocator
    );

    if (float_parts.kind == FLOAT_KIND_NAN) {
        f64 nan;
        memcpy(&nan, &(u64){0x7ff0000000000001}, sizeof(u64));
        return nan;
    }

    if (float_parts.kind == FLOAT_KIND_INFINITY) {
        f64 infinity;
        memcpy(&infinity, &(u64){0x7ff0000000000000}, sizeof(u64));
        if (float_parts.is_negative) {
            return -infinity;
        } else {
            return infinity;
        }
    }

    u64 raw_mantissa = (u64)mantissa_buffer[1] << 32 | (u64)mantissa_buffer[0];
    if (raw_mantissa == 0) {
        return 0.0;
    }

    u64 const raw_exponent_bias = 1023;
    u64 raw_exponent;

    // This puts the binary point right after the leftmost set bit.
    isize exponent = float_parts.exponent + ieee_mantissa_bits;

    if (exponent > max_exponent) {
        raw_mantissa = 0;
        raw_exponent = raw_exponent_bias + max_exponent + 1;
    } else {
        if (64 - u64_leading_zeroes(raw_mantissa) > ieee_mantissa_bits) {
            u64 implicit_one_mask = ~((u64)1 << (64 - u64_leading_zeroes(raw_mantissa) - 1));
            raw_mantissa = raw_mantissa & implicit_one_mask;
            raw_exponent = raw_exponent_bias + exponent;
        } else {
            raw_exponent = raw_exponent_bias + exponent - 1;
        }
    }

    assert(64 - u64_leading_zeroes(raw_mantissa) <= ieee_mantissa_bits);

    u64 raw_sign = 0;
    if (float_parts.is_negative) {
        raw_sign = 0x8000000000000000;
    }

    u64 raw_value = raw_sign | raw_exponent << ieee_mantissa_bits | raw_mantissa;
    f64 value;
    memcpy(&value, &raw_value, sizeof(u64));
    return value;
}
