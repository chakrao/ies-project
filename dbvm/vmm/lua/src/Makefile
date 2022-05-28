ASM = yasm
LD = ld
CC = gcc
#CPATH = ./include
AFLAGS = -gDWARF2 -f elf64 -DSERIALPORT=${SERIALPORT} -DDISPLAYDEBUG=${DISPLAYDEBUG} #-DJTAG
CFLAGS = -O0 -W -Wall  -DSUPPORT_64BIT_OFFSET -DDARKBYTESFUCKEDUPCROSSCOMPILER -fno-builtin-floor -fno-builtin-ceil -fno-builtin-strspn -fno-builtin-memcpy -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-strcat -fno-builtin-malloc -fno-builtin-snprintf -fno-builtin-memset -fno-stack-protector -Wimplicit-fallthrough=0 -g
LDFLAGS = -static -T lua.lnk -Map lua.map 
LDFLAGS2 = -Map lua.map
C2FLAGS = -Wconversion
#

all:
	@${CC} ${CFLAGS} lapi.c -c -o ../lapi.o
	@${CC} ${CFLAGS} lobject.c -c -o ../lobject.o
	@${CC} ${CFLAGS} ldo.c -c -o ../ldo.o
	@${CC} ${CFLAGS} lgc.c -c -o ../lgc.o
	@${CC} ${CFLAGS} lstate.c -c -o ../lstate.o
	@${CC} ${CFLAGS} ldebug.c -c -o ../ldebug.o
	@${CC} ${CFLAGS} ltm.c -c -o .