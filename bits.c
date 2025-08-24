#include <stddef.h>     // NULL
#include <assert.h>     // assert
#include <stdio.h>      // printf
#include <string.h>     // strlen, memcpy, memcmp, memmove, memset
#include <ctype.h>      // isspace
#include <stdbool.h>    // true, false, bool
#include <stdlib.h>     // malloc, free

// Redefinition of typedefs is a C11 feature.
// This is the official™ guard, which is used across different headers to protect u8 and friends.
// (Or just add a #define before including this header, if you already have short names defined.)
#ifndef SHORT_NAMES_FOR_PRIMITIVE_TYPES_WERE_DEFINED
    #define SHORT_NAMES_FOR_PRIMITIVE_TYPES_WERE_DEFINED

    #include <stdint.h>
    #include <stddef.h>

    typedef int8_t i8;
    typedef uint8_t u8;
    typedef uint16_t u16;
    typedef int16_t i16;
    typedef uint32_t u32;
    typedef int32_t i32;
    typedef uint64_t u64;
    typedef int64_t i64;

    typedef uintptr_t uptr;
    typedef size_t usize;
    typedef ptrdiff_t isize;

    typedef float f32;
    typedef double f64;
#endif

#define ARRAY_SIZE(array) ((isize)sizeof(array) / (isize)sizeof((array)[0]))

static inline isize isize_min(isize left, isize right) {
    return left < right ? left : right;
}

isize bits_to_string(
    u32 const *restrict words, isize word_count,
    char *restrict dest_buffer, isize dest_buffer_size
) {
    isize const min_dest_buffer_size =
        32 * word_count +   // zeroes and ones representing "words"
        word_count - 1  +   // whitespaces between "words"
        1;                  // null-terminator at the end

    if (dest_buffer == NULL) {
        return min_dest_buffer_size;
    } else {
        assert(dest_buffer_size >= min_dest_buffer_size);
    }

    char *dest_buffer_iter = dest_buffer;
    for (isize word_index = word_count - 1; word_index >= 0; word_index -= 1) {
        u32 current_word = words[word_index];

        for (int offset = 31; offset >= 0; offset -= 1) {
            *(dest_buffer_iter++) = (current_word >> offset & 1) == 0 ? '0' : '1';
        }
        *(dest_buffer_iter++) = ' ';
    }
    *(dest_buffer_iter++) = '\0';

    return dest_buffer_iter - dest_buffer;
}

isize bits_from_string(
    char const *restrict string, isize string_size,
    u32 *restrict dest_words, isize dest_word_count
) {
    if (string_size < 0) {
        string_size = strlen(string);
    }

    isize parsed_word_count = 0;
    {
        char const *string_iter = string;
        char const *string_end = string + string_size;

        while (string_iter != string_end) {
            while (string_iter != string_end && isspace(*string_iter)) {
                string_iter += 1;
            }

            if (string_iter != string_end) {
                parsed_word_count += 1;
            }

            char const *string_word_begin = string_iter;
            while (
                string_iter != string_end &&
                !isspace(*string_iter) &&
                string_iter - string_word_begin < 32
            ) {
                string_iter += 1;
            }
        }
    }

    if (dest_words == NULL) {
        return parsed_word_count;
    } else {
        // Chars at low addresses go into the high addresses of the words array.
        //
        // Because of that the results will look different (comparing them starting from low
        // addresses) depending on the size of the words array.
        assert(dest_word_count == parsed_word_count);
    }

    char const *string_iter = string;
    char const *string_end = string + string_size;

    for (isize word_index = dest_word_count - 1; word_index >= 0; word_index -= 1) {
        while (string_iter != string_end && isspace(*string_iter)) {
            string_iter += 1;
        }
        if (string_iter == string_end) {
            break;
        }

        char const *string_word_begin = string_iter;

        u32 next_word = 0;
        while (
            string_iter != string_end &&
            !isspace(*string_iter) &&
            string_iter - string_word_begin < 32
        ) {
            next_word = (next_word << 1) | (*string_iter == '0' ? 0 : 1);
            string_iter += 1;
        }
        dest_words[word_index] = next_word;
    }

    return dest_word_count;
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
    bits_shift(words, word_count, shift);
}

inline void bits_shift_right(u32 *words, isize word_count, isize shift) {
    // NOTE: OCD
    isize const isize_min = -(isize)((usize)(-1) >> 1) - 1;
    if (shift == isize_min) {
        bits_shift(words, word_count, -1);
        shift += 1;
    }

    bits_shift(words, word_count, -shift);
}

// Fill bit_count bits starting from starting_offset towards the more significant bits.
void bits_fill(u32 *words, isize word_count, isize starting_offset, isize bit_count, int value) {
    assert((starting_offset + bit_count) / 32 <= word_count);

    if (bit_count > 0) {
        u32 *first_word = &words[starting_offset / 32];

        u32 mask = 0xffffffff;
        mask >>= 32 - isize_min(bit_count, 32);
        mask <<= starting_offset % 32;

        if (value == 0) {
            *first_word &= ~mask;
        } else {
            *first_word |= mask;
        }

        isize first_word_bits = 32 - starting_offset % 32;
        bit_count -= first_word_bits;
        starting_offset += first_word_bits;
    }

    assert(bit_count == 0 || starting_offset % 32 == 0);
    if (bit_count / 32 > 0) {
        u8 fill_value = value == 0 ? 0 : 0xff;
        memset(words + starting_offset / 32, fill_value, (bit_count / 32) * sizeof(u32));

        isize middle_bits = (bit_count / 32) * 32;
        bit_count -= middle_bits;
        starting_offset += middle_bits;
    }

    assert(bit_count == 0 || starting_offset % 32 == 0 && bit_count < 32);
    if (bit_count > 0) {
        u32 *last_word = &words[starting_offset / 32];

        u32 mask = 0xffffffff;
        mask >>= 32 - bit_count;

        if (value == 0) {
            *last_word &= ~mask;
        } else {
            *last_word |= mask;
        }
    }
}

inline void bits_set(u32 *words, isize word_count, isize starting_offset, isize bit_count) {
    bits_fill(words, word_count, starting_offset, bit_count, 1);
}

inline void bits_clear(u32 *words, isize word_count, isize starting_offset, isize bit_count) {
    bits_fill(words, word_count, starting_offset, bit_count, 0);
}

// Tests

int test_count = 0;
int tests_passed = 0;

void bits_shift_test(isize shift, char const *input_string, char const *expected_string) {
    test_count += 1;

    isize input_word_count = bits_from_string(input_string, -1, NULL, 0);
    u32 *input_words = malloc(input_word_count * sizeof(u32));
    bits_from_string(input_string, -1, input_words, input_word_count);
    isize input_string_formatted_size = bits_to_string(input_words, input_word_count, NULL, 0);
    char *input_string_formatted = malloc(input_string_formatted_size);
    bits_to_string(input_words, input_word_count, input_string_formatted, input_string_formatted_size);

    isize expected_word_count = bits_from_string(expected_string, -1, NULL, 0);
    u32 *expected_words = malloc(expected_word_count * sizeof(u32));
    bits_from_string(expected_string, -1, expected_words, expected_word_count);
    isize expected_string_formatted_size = bits_to_string(expected_words, expected_word_count, NULL, 0);
    char *expected_string_formatted = malloc(expected_string_formatted_size);
    bits_to_string(expected_words, expected_word_count, expected_string_formatted, expected_string_formatted_size);

    u32 *actual_words = malloc(input_word_count * sizeof(u32));
    memcpy(actual_words, input_words, input_word_count * sizeof(u32));
    bits_shift(actual_words, input_word_count, shift);
    isize actual_string_size = bits_to_string(actual_words, input_word_count, NULL, 0);
    char *actual_string = malloc(actual_string_size);
    bits_to_string(actual_words, input_word_count, actual_string, actual_string_size);

    if (memcmp(expected_words, actual_words, expected_word_count * sizeof(u32)) == 0) {
        tests_passed += 1;
    } else {
        printf("Test #%d failed\n", test_count);
        printf("in:\t%s\n", input_string_formatted);
        printf("shift:\t%td\n", shift);
        printf("Expected:\n\t%s\n", expected_string_formatted);
        printf("Actual:\n\t%s\n\n", actual_string);
    }

    free(input_words);
    free(actual_words);
    free(expected_words);

    free(input_string_formatted);
    free(expected_string_formatted);
    free(actual_string);
}

// "function" is either bits_clear or bits_set.
void bits_fill_test(
    isize starting_offset,
    isize bit_count,
    char const *input_string,
    char const *expected_string,
    void(*function)(u32 *, isize, isize, isize)
) {
    test_count += 1;

    isize input_word_count = bits_from_string(input_string, -1, NULL, 0);
    u32 *input_words = malloc(input_word_count * sizeof(u32));
    bits_from_string(input_string, -1, input_words, input_word_count);
    isize input_string_formatted_size = bits_to_string(input_words, input_word_count, NULL, 0);
    char *input_string_formatted = malloc(input_string_formatted_size);
    bits_to_string(input_words, input_word_count, input_string_formatted, input_string_formatted_size);

    isize expected_word_count = bits_from_string(expected_string, -1, NULL, 0);
    u32 *expected_words = malloc(expected_word_count * sizeof(u32));
    bits_from_string(expected_string, -1, expected_words, expected_word_count);
    isize expected_string_formatted_size = bits_to_string(expected_words, expected_word_count, NULL, 0);
    char *expected_string_formatted = malloc(expected_string_formatted_size);
    bits_to_string(expected_words, expected_word_count, expected_string_formatted, expected_string_formatted_size);

    u32 *actual_words = malloc(input_word_count * sizeof(u32));
    memcpy(actual_words, input_words, input_word_count * sizeof(u32));
    function(actual_words, input_word_count, starting_offset, bit_count);
    isize actual_string_size = bits_to_string(actual_words, input_word_count, NULL, 0);
    char *actual_string = malloc(actual_string_size);
    bits_to_string(actual_words, input_word_count, actual_string, actual_string_size);

    if (memcmp(expected_words, actual_words, expected_word_count * sizeof(u32)) == 0) {
        tests_passed += 1;
    } else {
        printf("Test #%d failed\n", test_count);
        printf("in:\t%s\n", input_string_formatted);
        printf("offset:\t%td\n", starting_offset);
        printf("bits:\t%td\n", bit_count);
        printf("Expected:\n\t%s\n", expected_string_formatted);
        printf("Actual:\n\t%s\n\n", actual_string);
    }

    free(input_words);
    free(actual_words);
    free(expected_words);

    free(input_string_formatted);
    free(expected_string_formatted);
    free(actual_string);
}

void bits_clear_test(
    isize starting_offset,
    isize bit_count,
    char const *input_string,
    char const *expected_string
) {
    bits_fill_test(starting_offset, bit_count, input_string, expected_string, bits_clear);
}

void bits_set_test(
    isize starting_offset,
    isize bit_count,
    char const *input_string,
    char const *expected_string
) {
    bits_fill_test(starting_offset, bit_count, input_string, expected_string, bits_set);
}

int main(void) {
    // bits_shift tests

    // Shifts by zero bits:
    bits_shift_test(0,
        "0",
        "0"
    );
    bits_shift_test(0,
        "00101100000000111000000110100101",
        "00101100000000111000000110100101"
    );

    // Shifts by multiples of 32:
    bits_shift_test(32,
        "00101001111111111111100100101001 10011111111111110000000001110010",
        "10011111111111110000000001110010 00000000000000000000000000000000"
    );
    bits_shift_test(-32,
        "00101001111111111111100100101001 10011111111111110000000001110010",
        "00000000000000000000000000000000 00101001111111111111100100101001"
    );
    bits_shift_test(96,
        "10100101010110011111010010101010 10101010111111111111001001000110 00101001111111111111100100101001 10011111111111110000000001110010",
        "10011111111111110000000001110010 00000000000000000000000000000000 00000000000000000000000000000000 00000000000000000000000000000000"
    );
    bits_shift_test(-96,
        "10100101010110011111010010101010 10101010111111111111001001000110 00101001111111111111100100101001 10011111111111110000000001110010",
        "00000000000000000000000000000000 00000000000000000000000000000000 00000000000000000000000000000000 10100101010110011111010010101010"
    );

    // Shifts by non-multiples of 32 which are less than 32:
    bits_shift_test(19,
        "0000000000000000000000000000000001010001010000100111111100100101",
        "0000000000000010100010100001001111111001001010000000000000000000"
    );
    bits_shift_test(-19,
        "0000000000000010100010100001001111111001001010000000000000000000",
        "0000000000000000000000000000000001010001010000100111111100100101"
    );
    bits_shift_test(13,
        "0101101010110110100001101101010110100111111111010010100001110001",
        "1101000011011010101101001111111110100101000011100010000000000000"
    );
    bits_shift_test(-13,
        "0101101010110110100001101101010110100111111111010010100001110001",
        "0000000000000010110101011011010000110110101011010011111111101001"
    );

    // Shifts by non-multiples of 32 which are greater than 32:
    bits_shift_test(85,
        "01011010101101101000011011010101101001111111110100101000011100011010011101000101010100001111010101111111110111111110001010101010",
        "00011110101011111111101111111100010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    );
    bits_shift_test(-85,
        "01011010101101101000011011010101101001111111110100101000011100011010011101000101010100001111010101111111110111111110001010101010",
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000101101010110110100001101101010110100111111"
    );
    bits_shift_test(75,
        "01011010101101101000011011010101101001111111110100101000011100011010011101000101010100001111010101111111110111111110001010101010",
        "00101010100001111010101111111110111111110001010101010000000000000000000000000000000000000000000000000000000000000000000000000000"
    );
    bits_shift_test(-75,
        "01011010101101101000011011010101101001111111110100101000011100011010011101000101010100001111010101111111110111111110001010101010",
        "00000000000000000000000000000000000000000000000000000000000000000000000000001011010101101101000011011010101101001111111110100101"
    );

    // Shifting everything out:
    bits_shift_test(128,
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    );
    bits_shift_test(-128,
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    );
    bits_shift_test(9223372036854775807ll,
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    );
    bits_shift_test(-9223372036854775807ll - 1ll,
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    );

    // bits_clear/bits_set tests

    // Setting zero bits:
    bits_set_test(0, 0,
        "00000000000000000000000000000000",
        "00000000000000000000000000000000"
    );

    // Setting whole words at aligned offsets:
    bits_set_test(0, 32,
        "00000000000000000000000000000000",
        "11111111111111111111111111111111"
    );
    bits_set_test(32, 96,
        "00000000000000000000000000000000 00000000000000000000000000000000 00000000000000000000000000000000 00000000000000000000000000000000",
        "11111111111111111111111111111111 11111111111111111111111111111111 11111111111111111111111111111111 00000000000000000000000000000000"
    );

    // Setting bits starting at aligned offsets:
    bits_set_test(0, 14,
        "00000000000000000000000000000000",
        "00000000000000000011111111111111"
    );
    bits_set_test(32, 24,
        "00000000000000000000000000000000 00000000000000000000000000000000 00000000000000000000000000000000 00000000000000000000000000000000",
        "00000000000000000000000000000000 00000000000000000000000000000000 00000000111111111111111111111111 00000000000000000000000000000000"
    );
    bits_set_test(32, 77,
        "00000000000000000000000000000000 00000000000000000000000000000000 00000000000000000000000000000000 00000000000000000000000000000000",
        "00000000000000000001111111111111 11111111111111111111111111111111 11111111111111111111111111111111 00000000000000000000000000000000"
    );

    // Setting bits starting at unaligned offsets:
    bits_set_test(12, 17,
        "00000000000000000000000000000000",
        "00011111111111111111000000000000"
    );
    bits_set_test(25, 8,
        "0000000000000000000000000000000000000000000000000000000000000000",
        "0000000000000000000000000000000111111110000000000000000000000000"
    );
    bits_set_test(31, 35,
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
        "00000000000000000000000000000000000000000000000000000000000000111111111111111111111111111111111110000000000000000000000000000000"
    );
    bits_set_test(12, 95,
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
        "00000000000000000000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000000000000"
    );
    bits_clear_test(24, 100,
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111",
        "11110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111111111111111111111111"
    );

    printf("%d/%d tests passed.\n", tests_passed, test_count);

    return 0;
}
