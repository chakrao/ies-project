TOP = ../..
include $(TOP)/Makefile
SRC = $(TOPSRC)/tests/tests2
VPATH = $(SRC)

TESTS = $(patsubst %.c,%.test,\
    $(sort $(notdir $(wildcard $(SRC)/??_*.c)))\
    $(sort $(notdir $(wildcard $(SRC)/???_*.c))))

# some tests do not pass on all platforms, remove them for now
SKIP = 34_array_assignment.test # array assignment is not in C standard
ifeq ($(CONFIG_arm_eabi),yes) # not ARM soft-float
 SKIP += 22_floating_point.test
endif
ifdef CONFIG_OSX
 SKIP += 40_stdio.test 42_function_pointer.test
endif
ifeq ($(ARCH),x86_64)
 SKIP += 73_arm64.test
endif
ifeq (,$(filter i386,$(ARCH)))
 SKIP += 98_al_ax_extend.test 99_fastcall.test
endif
ifeq (,$(filter i386 x86_64,$(ARCH)))
 SKIP += 85_asm-outside-function.test # x86 asm
endif
ifeq ($(CONFIG_backtrace),no)
 SKIP += 112_backtrace.test
 SKIP += 113_btdll.test
 CONFIG_bcheck = no # no bcheck without backtrace
endif
ifeq ($(CONFIG_bcheck),no)
 SKIP += 114_bound_signal.test
 SKIP += 115_bound_setjmp.test
 SKIP += 116_bound_setjmp2.test
 SKIP += 117_builtins.test
endif
ifeq ($(CONFIG_dll),no)
 SKIP += 113_btdll.t