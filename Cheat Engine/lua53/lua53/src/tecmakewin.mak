#-------------------------------------------------------------------------#
#- Tecmake  (Windows Version)                                            -#
#- Generic Makefile to build applications and libraries at TeCGraf       -#
#- The user makefile usually has the name "config.mak".                  -#
#-------------------------------------------------------------------------#

#---------------------------------#
# Tecmake Version
VERSION = 4.13


#---------------------------------#
# First target
.PHONY: build
build: tecmake


#---------------------------------#
# Location of this file
TECMAKE  = $(TECMAKE_HOME)/tecmakewin.mak


#---------------------------------#
# System Variables Definitions

# If tecmake.bat is not used,
# then at least define main system variables.

WIN32UNAMES = vc12 vc11 vc10 vc9 vc8 vc7 vc6 owc1 bc55 bc56 bc6 gcc3 gcc4 mingw3 mingw4 dllw4 dllg4 dll dll7 dll8 dll9 dll10 dll11 dll12
WIN64UNAMES = vc12_64 vc11_64 vc10_64 vc9_64 vc8_64 dll8_64 dll9_64 dll10_64 dll11_64 dll12_64 gcc4_64 mingw4_64 dllw4_64 dllg4_64

ifdef TEC_UNAME
  ifneq ($(findstring $(TEC_UNAME), $(WIN32UNAMES)), )
    TEC_WIN32 = Yes
  else
    ifneq ($(findstring $(TEC_UNAME), $(WIN64UNAMES)), )
      TEC_WIN64 = Yes
    endif
  endif
endif

ifdef TEC_WIN64
  TEC_SYSNAME=Win64
  TEC_SYSARCH=x64
  # This is not working, because make from Cygwin returns x86 even when running in AMD64.
  ifeq ($(PROCESSOR_ARCHITECTURE), x86)
    # Define this if compiling for 64-bits in a 32bits environment
    #USE_X86_CL64=XXX
  endif
else
  TEC_SYSNAME=Win32
  TEC_SYSARCH=x86
endif


#---------------------------------#
# System Info
.PHONY: sysinfo
sysinfo:
	@echo ''; echo 'Tecmake: System Info'
	@echo 'TEC_SYSNAME = $(TEC_SYSNAME)'
	@echo 'TEC_SYSARCH = $(TEC_SYSARCH)'
	@echo 'TEC_UNAME = $(TEC_UNAME)'
	@echo 'TEC_CC = $(TEC_CC)'; echo ''


#---------------------------------#
# Known platforms

UNAMES = $(WIN32UNAMES) $(WIN64UNAMES)


#---------------------------------#
# Directories Definitions

PROJDIR = ..
SRCDIR  = .
OBJROOT = $(PROJDIR)/obj


# ---------------------------------------------------------
# Byte Order and Word Size

ifneq ($(findstring x86, $(TEC_SYSARCH)), )
   TEC_BYTEORDER = TEC