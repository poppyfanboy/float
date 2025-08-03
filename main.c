#include "float.h"

#include <stdio.h>
#include <stdlib.h>

int main(void) {
    f32 f32_input_value = 123.7654321F;
    {
        isize buffer_size = f32_format(f32_input_value, NULL, 0, &(FloatFormatParams){.precision = 0});
        char *buffer = malloc(buffer_size);
        f32_format(f32_input_value, buffer, buffer_size, &(FloatFormatParams){.precision = 0});

        printf("f32_input_value = %s\n", buffer);
    }

    printf("\n");

    f64 f64_input_value = 1e100;
    {
        isize buffer_size = f64_format(f64_input_value, NULL, 0, &(FloatFormatParams){.precision = 0});
        char *buffer = malloc(buffer_size);
        f64_format(f64_input_value, buffer, buffer_size, &(FloatFormatParams){.precision = 0});

        printf("f64_input_value = %s\n", buffer);
    }

    return 0;
}
