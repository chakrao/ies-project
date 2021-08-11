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
   TEC_BYTEORDER = TEC_LITTLEENDIAN
else
ifeq ($(TEC_SYSARCH), arm)
   TEC_BYTEORDER = TEC_LITTLEENDIAN
else
   TEC_BYTEORDER = TEC_BIGENDIAN
endif
endif

ifeq ($(TEC_SYSARCH), x64)
  TEC_BYTEORDER = TEC_LITTLEENDIAN
  TEC_WORDSIZE = TEC_64
else
  TEC_WORDSIZE = TEC_32
endif

# Itanium Exception
ifeq ($(TEC_SYSARCH), ia64)
  TEC_BYTEORDER = TEC_LITTLEENDIAN
  TEC_WORDSIZE = TEC_64
endif


#---------------------------------#
# Tools

SHELL      = bash
SLASH      = slash_parser

# Packed Lua script
LUAPRE = "$(TECMAKE_PATH)"/luapre.lua


#---------------------------------#
# Defaults
APPTYPE = windows
INCLUDES =
LIBS =
LIB =


#---------------------------------#
# User Configuration File

MAKENAME = config.mak

ifdef MF
  MAKENAME = $(MF).mak
endif

###################
include $(MAKENAME)
###################


#---------------------------------#
# Definitions of public variables

ifdef LIBNAME
  TARGETNAME = $(LIBNAME)
  MAKETYPE = LIB
else
  TARGETNAME = $(APPNAME)
  MAKETYPE = APP
endif

ifndef TARGETNAME
  $(error LIBNAME nor APPNAME defined in $(MAKENAME))
endif

PROJNAME ?= $(TARGETNAME)

ifneq ($(PROJNAME), $(TARGETNAME))
  OBJROOT := $(OBJROOT)/$(TARGETNAME)
endif

ifneq ($(findstring dll, $(TEC_UNAME)), )
  ifneq ($(MAKETYPE), APP)
    MAKETYPE = DLL
    DEF_FILE ?= $(TARGETNAME).def
    DEF_FILE := $(SRCDIR)/$(DEF_FILE)
  endif
endif

DEPEND := $(TARGETNAME).wdep

ifdef DEPENDDIR
  DEPEND := $(DEPENDDIR)/$(TARGETNAME).dep.$(TEC_UNAME)
endif


# ---------------------------------------------------------
# LO, LOH and LH folders

SRCLUADIR ?= $(SRCDIR)
ifdef NO_LUAOBJECT
  LHDIR  ?= $(SRCLUADIR)
else
  LOHDIR ?= $(SRCLUADIR)
endif

ifdef USE_LOH_SUBDIR
  ifeq ($(TEC_BYTEORDER), TEC_BIGENDIAN)
    ifeq ($(TEC_WORDSIZE), TEC_64)
      LOH_SUBDIR ?= be64
    else
      LOH_SUBDIR ?= be32
    endif
  else
    ifeq ($(TEC_WORDSIZE), TEC_64)
      # longs in 64-bits Windows are 32 bits!!!
      LOH_SUBDIR ?= le64w
    else
      LOH_SUBDIR ?= le32
    endif
  endif
  
  LOHDIR := $(LOHDIR)/$(LOH_SUBDIR)
  INCLUDES += $(LOHDIR)
else
  ifeq ($(TEC_BYTEORDER), TEC_BIGENDIAN)
    ifeq ($(TEC_WORDSIZE), TEC_64)
      LO_SUFFIX ?= _be64
    else
      LO_SUFFIX ?= _be32
    endif
  else
    ifeq ($(TEC_WORDSIZE), TEC_64)
      # longs in 64-bits Windows are 32 bits!!!
      LO_SUFFIX ?= _le64w
    else
      LO_SUFFIX ?=
    endif
  endif
endif

ifdef USE_LH_SUBDIR
  INCLUDES += $(LHDIR)
endif


#---------------------------------#
# Main Rule - Build Everything that it is necessary

.PHONY: tecmake
ifeq ($(MAKETYPE), APP)
  tecmake: print-start system-check directories application scripts
else
  ifeq ($(MAKETYPE), DLL)
    tecmake: print-start system-check directories dynamic-lib
  else
    tecmake: print-start system-check directories static-lib
  endif
endif

.PHONY: print-start
print-start:
	@echo ''; echo 'Tecmake: Starting [ $(TARGETNAME):$(TEC_UNAME) ]'


#---------------------------------#
# Definitions of public variables

ifeq ($(MAKETYPE), APP)
  TARGETROOT ?= $(PROJDIR)/bin
else
  TARGETROOT ?= $(PROJDIR)/lib
endif

ifeq ($(MAKETYPE), APP)
  TEC_UNAME_DIR ?= $(TEC_SYSNAME)
else
  TEC_UNAME_DIR ?= $(TEC_UNAME)
endif

ifdef DBG
  OPT:=
  ifdef DBG_DIR
    TEC_UNAME_DIR := $(TEC_UNAME_DIR)d
  endif
endif

ifdef LUAMOD_DIR
  ifdef USE_LUA53
    LUAMODSFX = 53
  endif
  ifdef USE_LUA52
    LUAMODSFX = 52
  endif
  ifdef USE_LUA51
    LUAMODSFX = 51
  endif
  TEC_UNAME_DIR := $(TEC_UNAME_DIR)/Lua$(LUAMODSFX)
endif

OBJDIR := $(OBJROOT)/$(TEC_UNAME_DIR)
TARGETDIR := $(TARGETROOT)/$(TEC_UNAME_DIR)

TARGETEXE := $(TARGETDIR)/$(TARGETNAME).exe
TARGETDLL := $(TARGETDIR)/$(TARGETNAME).dll
TARGETLIB := $(TARGETDIR)/$(TARGETNAME).lib
ifneq ($(findstring gcc, $(TEC_UNAME)), )
  TARGETLIB := $(TARGETDIR)/lib$(TARGETNAME).a
endif
ifneq ($(findstring mingw, $(TEC_UNAME)), )
  TARGETLIB := $(TARGETDIR)/lib$(TARGETNAME).a
endif
ifneq ($(findstring dllg, $(TEC_UNAME)), )
  TARGETLIB := $(TARGETDIR)/lib$(TARGETNAME).dll.a
endif
ifneq ($(findstring dllw, $(TEC_UNAME)), )
  TARGETLIB := $(TARGETDIR)/lib$(TARGETNAME).a
endif

ifdef NO_ECHO
  ECHO:=@
endif

#---------------------------------#
# Platform/Compiler dependend parameters

STDDEFS = -DTEC_UNAME=$(TEC_UNAME) -DTEC_SYSNAME=$(TEC_SYSNAME) -D$(TEC_BYTEORDER) -D$(TEC_WORDSIZE) -DWIN32
STDLIB  = kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 uuid oleaut32 ole32 comctl32

#Compilers
VC6 ?= x:/lng/vc6
VC7 ?= x:/lng/vc7
VC8 ?= x:/lng/vc8
VC9 ?= x:/lng/vc9
VC10 ?= x:/lng/vc10
VC11 ?= x:/lng/vc11
VC12 ?= x:/lng/vc12
OWC1 ?= x:/lng/owc1
BC55 ?= x:/lng/bc55
BC56 ?= x:/lng/cbuilderx
BC6  ?= x:/lng/bc6
MINGW3 ?= x:/lng/mingw3
MINGW4 ?= x:/lng/mingw4
MINGW4_64 ?= x:/lng/mingw4_64
# The default location is in the PATH
#GCC3 ?= x:/lng/gc