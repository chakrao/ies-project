/*
 * linuxport.h
 *
 *  Created on: Nov 5, 2022
 *      Author: eric
 */

#ifndef MONODATACOLLECTOR_LINUXPORT_H_
#define MONODATACOLLECTOR_LINUXPORT_H_



#include <cstdint>
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <pthread.h>
#include <stdlib.h>
#include <strings.h>
#include <stdio.h>
#include <dlfcn.h>

#include <errno.h>



typedef int BOOL;
typedef intptr_t HANDLE;


typedef uint8_t BYTE;
typedef uint16_t WORD;
typedef uint32_t DWORD, *LPDWORD;
typedef uint32_t UINT32;
typedef uint64_t QWORD;
typedef int64_t INT64;
typedef uint64_t UINT64, *PUINT64;


typedef void* PVOID;
typedef void* LPVOID, *LPOVERLAPPED;


typedef intptr_t HINSTANCE;
typedef size_t SIZE_T;

typedef uintptr_t UINT_PTR, *PUINT_PTR;

typedef pthread_mutex_t CRITICAL_SECTION;

//__