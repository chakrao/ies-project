/*
 *  Test 128-bit floating-point arithmetic on arm64:
 *  build with two different compilers and compare the output.
 *
 *  Copyright (c) 2015 Edmund Grimley Evans
 *
 * Copying and distribution of this file, with or without modification,
 * are permitted in any medium without royalty provided the copyright
 * notice and this notice are preserved.  This file is offered as-is,
 * without any warranty.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define check(x) ((x) ? (void)0 : check_fail(#x, __FILE__, __LINE__))

void check_fail(const char *assertion, const char *file, unsigned int line)
{
    printf("%s:%d: Check (%s) failed.", file, line, assertion);
    exit(1);
}

typedef struct {
    unsigned long long x0, x1;
} u128_t;

float copy_fi(uint32_t x)
{
    float f;
    memcpy(&f, &x, 4);
    return f;
}

double copy_di(uint64_t x)
{
    double f;
    memcpy(&f, &x, 8);
    return f;
}

long double copy_ldi(u128_t x)
{
    long double f;
    memcpy(&f, &x, 16);
    return f;
}

uint32_t copy_if(float f)
{
    uint32_t x;
    memcpy(&x, &f, 4);
    return x;
}

uint64_t copy_id(double f)
{
    uint64_t x;
    memcpy(&x, &f, 8);
    return x;
}

u128_t copy_ild(long double f)
{
    u128_t x;
    memcpy(&x, &f, 16);
    return x;
}

long double make(int sgn, int exp, uint64_t high, uint64_t low)
{
    u128_t x = { low,
                 (0x0000ffffffffffff & high) |
                 (0x7fff000000000000 & (uint64_t)exp << 48) |
                 (0x8000000000000000 & (uint64_t)sgn << 63) };
    return copy_ldi(x);
}

void cmp(long double a, long double b)
{
    u128_t ax = copy_ild(a);
    u128_t bx = copy_ild(b);
    int eq = (a == b);
    int ne = (a != b);
    int lt = (a < b);
    int le = (a <= b);
    int gt = (a > b);
    int ge = (a >= b);

    check(eq == 0 || eq == 1);
    check(lt == 0 || lt == 1);
    check(gt == 0 || gt == 1);
    check(ne == !eq && le == (lt | eq) && ge == (gt | eq));
    check(eq + lt + gt < 2);

    printf("cmp %016llx%016llx %016llx%016llx %d %d %d\n",
           ax.x1, ax.x0, bx.x1, bx.x0, lt, eq, gt);
}

void cmps(void)
{
    int i, j;

    for (i = 0; i < 2; i++)
        for (j = 0; j < 2; j++)
            cmp(make(i, 0, 0, 0), make(j, 0, 0, 0));

    for (i = 0; i < 2; i++) {
        for (j = 0; j < 64; j++) {
            long double f1 = make(i, 32767, (uint64_t)1 << j, 0);
            long double f2 = make(i, 32767, 0, (uint64_t)1 << j);
            cmp(f1, 0);
            cmp(f2, 0);
            cmp(0, f1);
            cmp(0, f2);
        }
    }

    for (i = 0; i < 6; i++)
        for (j = 0; j < 6; j++)
            cmp(make(i & 1, i >> 1, 0, 0),
                make(j & 1, j >> 1, 0, 0));

    for (i = 0; i < 2; i++) {
        for (j = 0; j < 2; j++) {
            int a, b;
            for (a = 0; a < 2; a++) {
                for (b = 0; b < 2; b++) {
                    cmp(make(i, j, a, b), make(i, j, 0, 0));
                    cmp(make(i, j, 0, 0), make(i, j, a, b));
                }
            }
        }
    }
}

void xop(const char *name, long double a, long double b, long double c)
{
    u128_t ax = copy_ild(a);
    u128_t bx = copy_ild(b);
    u128_t cx = copy_ild(c);
    printf("%s %016llx%016llx %016llx%016llx %016llx%016llx\n",
           name, ax.x1, ax.x0, bx.x1, bx.x0, cx.x1, cx.x0);
}

void fadd(long double a, long double b)
{
    xop("add", a, b, a + b);
}

void fsub(long double a, long double b)
{
    xop("sub", a, b, a - b);
}

void fmul(long double a, long double b)
{
    xop("mul", a, b, a * b);
}

void fdiv(long double a, long double b)
{
    xop("div", a, b, a / b);
}

void nanz(void)
{
    // Check NaNs:
    {
        long double x[7];
        int i, j, n = 0;
        x[n++] = make(0, 32000, 0x95132b76effc, 0xd79035214b4f8d53);
        x[n++] = make(1, 32001, 0xbe71d7a51587, 0x30601c6815d6c3ac);
        x[n++] = make(0, 32767, 0, 1);
        x[n++] = make(0, 32767, (uint64_t)1 << 46, 0);
        x[n++] = make(1, 32767, (uint64_t)1 << 47, 0);
        x[n++] = make(1, 32767, 0x7596c7099ad5, 0xe25fed2c58f73fc9);
        x[n++] = make(0, 32767, 0x835d143360f9, 0x5e315efb35630666);
        check(n == sizeof(x) / sizeof(*x));
        for (i = 0; i <