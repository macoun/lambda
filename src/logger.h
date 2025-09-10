#ifndef logger_h
#define logger_h

#include <stdio.h>

#define COLOR_RED "\x1b[31m"
#define COLOR_GREEN "\x1b[32m"
#define COLOR_YELLOW "\x1b[33m"
#define COLOR_BLUE "\x1b[34m"
#define COLOR_MAGENTA "\x1b[35m"
#define COLOR_CYAN "\x1b[36m"
#define COLOR_BRIGHT_WHITE "\x1b[97m"
#define COLOR_RESET "\x1b[0m"

#if COLORED
#define eprintf(color, format, ...) \
    fprintf(stderr, color format COLOR_RESET "\n", ##__VA_ARGS__)
#else
#define eprintf(color, format, ...) \
    fprintf(stderr, format "\n", ##__VA_ARGS__)
#endif

#define error(...) eprintf(COLOR_RED, __VA_ARGS__)
#define info(...) eprintf(COLOR_GREEN, __VA_ARGS__)
#define debug(...) eprintf(COLOR_CYAN, __VA_ARGS__)
#define warn(...) eprintf(COLOR_YELLOW, __VA_ARGS__)
#define logexpr(s, exp)  \
    do                   \
    {                    \
        info("%s: ", s); \
        print_exp(exp);  \
        printf("\n");    \
    } while (0)

#endif /* logger_h */
