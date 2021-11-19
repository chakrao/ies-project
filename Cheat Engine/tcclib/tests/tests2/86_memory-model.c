#include <stdio.h>

int
main()
{
#if defined(__LLP64__)
	if (sizeof(short) == 2
	    && sizeof(int) == 4
	    && sizeof(long int) == 4
	    && sizeof(long long int) == 8
	    && sizeof(void*) == 8) {
		(void)printf("Ok\n");
	} else {
		(void)printf("KO __LLP64__\n");
	}
#elif defined(__LP64_