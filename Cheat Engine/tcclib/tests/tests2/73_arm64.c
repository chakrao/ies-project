// This program is designed to test some arm64-specific things, such as the
// calling convention, but should give the same results on any architecture.

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

struct s1 { char x[1]; } s1 = { "0" };
struct s2 { char x[2]; } s2 = { "12" };
struct s3 {