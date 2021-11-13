#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define NB_ITS 1000000
//#define NB_ITS 1
#define TAB_SIZE 100

int tab[TAB_SIZE];
int ret_sum;
char tab3[256];

int test1(void)
{
    int i, sum = 0;
    for(i=0;i<TAB_SIZE;i++) {
        sum += tab[i];
    }
    return sum;
}

/* error */
int test2(void)
{
    int i, sum = 0;
    for(i