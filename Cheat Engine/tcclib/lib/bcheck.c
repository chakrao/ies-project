/*
 *  Tiny C Memory and bounds checker
 * 
 *  Copyright (c) 2002 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <setjmp.h>

#if !defined(__FreeBSD__) \
 && !defined(__FreeBSD_kernel__) \
 && !defined(__DragonFly__) \
 && !defined(__OpenBSD__) \
 && !defined(__APPLE__) \
 && !defined(__NetBSD__)
#include <malloc.h>
#endif

#if !defined(_WIN32)
#include <unistd.h>
#include <sys/syscall.h>
#endif

#define BOUND_DEBUG             (1)
#define BOUND_STATISTIC         (1)

#if BOUND_DEBUG
 #define dprintf(a...)         if (print_calls) fprintf(a)
#else
 #define dprintf(a...)
#endif

#ifdef __attribute__
  /* an __attribute__ macro is defined in the system headers */
  #undef __attribute__ 
#endif
#define FASTCALL __attribute__((regparm(3)))

#ifdef _WIN32
# define DLL_EXPORT __declspec(dllexport)
#else
# define DLL_EXPORT
#endif

#if defined(__FreeBSD__) \
 || defined(__FreeBSD_kernel__) \
 || defined(__DragonFly__) \
 || defined(__OpenBSD__) \
 || defined(__NetBSD__) \
 || defined(__dietlibc__)

#include <sys/mman.h>
#define INIT_SEM()
#define EXIT_SEM()
#define WAIT_SEM()
#define POST_SEM()
#define TRY_SEM()
#define HAVE_MEMALIGN          (0)
#define MALLOC_REDIR           (0)
#define HAVE_PTHREAD_CREATE    (0)
#define HAVE_CTYPE             (0)
#define HAVE_ERRNO             (0)
#define HAVE_SIGNAL            (0)
#define HAVE_SIGACTION         (0)
#define HAVE_FORK              (0)
#define HAVE_TLS_FUNC          (0)
#define HAVE_TLS_VAR           (0)

#elif defined(_WIN32)

#include <windows.h>
#include <signal.h>
static CRITICAL_SECTION bounds_sem;
#define INIT_SEM()             InitializeCriticalSection(&bounds_sem)
#define EXIT_SEM()             DeleteCriticalSection(&bounds_sem)
#define WAIT_SEM()             EnterCriticalSection(&bounds_sem)
#define POST_SEM()             LeaveCriticalSection(&bounds_sem)
#define TRY_SEM()              TryEnterCriticalSection(&bounds_sem)
#define HAVE_MEMALIGN          (0)
#define MALLOC_REDIR           (0)
#define HAVE_PTHREAD_CREATE    (0)
#define HAVE_CTYPE             (0)
#define HAVE_ERRNO             (0)
#define HAVE_SIGNAL            (1)
#define HAVE_SIGACTION         (0)
#define HAVE_FORK              (0)
#define HAVE_TLS_FUNC          (1)
#define HAVE_TLS_VAR           (0)

#else

#define __USE_GNU              /* get RTLD_NEXT */
#include <sys/mman.h>
#include <ctype.h>
#include <pthread.h>
#include <dlfcn.h>
#include <errno.h>
#include <signal.h>
#ifdef __APPLE__
#include <dispatch/dispatch.h>
static dispatch_semaphore_t bounds_sem;
#define INIT_SEM()             bounds_sem = dispatch_semaphore_create(1)
#define EXIT_SEM()             dispatch_release(*(dispatch_object_t*)&bounds_sem)
#define WAIT_SEM()             if (use_sem) dispatch_semaphore_wait(bounds_sem, DISPATCH_TIME_FOREVER)
#define POST_SEM()             if (use_sem) dispatch_semaphore_signal(bounds_sem)
#define TRY_SEM()              if (use_sem) dispatch_semaphore_wait(bounds_sem, DISPATCH_TIME_NOW)
#elif 0
#include <semaphore.h>
static sem_t bounds_sem;
#define INIT_SEM()             sem_init (&bounds_sem, 0, 1)
#define EXIT_SEM()             sem_destroy (&bounds_sem)
#define WAIT_SEM()             if (use_sem) while (sem_wait (&bounds_sem) < 0 \
                                                   && errno == EINTR)
#define POST_SEM()             if (use_sem) sem_post (&bounds_sem)
#define TRY_SEM()              if (use_sem) while (sem_trywait (&bounds_sem) < 0 \
                                                   && errno == EINTR)
#elif 0
static pthread_mutex_t bounds_mtx;
#define INIT_SEM()             pthread_mutex_init (&bounds_mtx, NULL)
#define EXIT_SEM()             pthread_mutex_destroy (&bounds_mtx)
#define WAIT_SEM()             if (use_sem) pthread_mutex_lock (&bounds_mtx)
#define POST_SEM()             if (use_sem) pthread_mutex_unlock (&bounds_mtx)
#define TRY_SEM()              if (use_sem) pthread_mutex_trylock (&bounds_mtx)
#else
static pthread_spinlock_t bounds_spin;
/* about 25% faster then semaphore. */
#define INIT_SEM()             pthread_spin_init (&bounds_spin, 0)
#define EXIT_SEM()             pthread_spin_destroy (&bounds_spin)
#define WAIT_SEM()             if (use_sem) pthread_spin_lock (&bounds_spin)
#define POST_SEM()             if (use_sem) pthread_spin_unlock (&bounds_spin)
#define TRY_SEM()              if (use_sem) pthread_spin_trylock (&bounds_spin)
#endif
#define HAVE_MEMALIGN          (1)
#define MALLOC_REDIR           (1)
#define HAVE_PTHREAD_CREATE    (1)
#define HAVE_CTYPE             (1)
#define HAVE_ERRNO             (1)
#define HAVE_SIGNAL            (1)
#define HAVE_SIGACTION         (1)
#define HAVE_FORK              (1)
#if !defined(__APPLE__) && defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))
#define HAVE_TLS_FUNC          (0)
#define HAVE_TLS_VAR           (1)
#else
#define HAVE_TLS_FUNC          (1)
#define HAVE_TLS_VAR           (0)
#endif
#ifdef TCC_MUSL
# undef HAVE_CTYPE
#endif
#endif

#if MALLOC_REDIR
static void *(*malloc_redir) (size_t);
static void *(*calloc_redir) (size_t, size_t);
static void (*free_redir) (void *);
static void *(*realloc_redir) (void *, size_t);
static unsigned int pool_index;
static unsigned char __attribute__((aligned(16))) initial_pool[256];
#endif
#if HAVE_MEMALIGN
static void *(*memalign_redir) (size_t, size_t);
#endif
#if HAVE_PTHREAD_CREATE
static int (*pthread_create_redir) (pthread_t *thread,
                                    const pthread_attr_t *attr,
                                    void *(*start_routine)(void *), void *arg);
#endif
#if HAVE_SIGNAL
typedef void (*bound_sig)(int);
static bound_sig (*signal_redir) (int signum, bound_sig handler);
#endif
#if HAVE_SIGACTION
static int (*sigaction_redir) (int signum, const struct sigaction *act,
                               struct sigaction *oldact);
#endif
#if HAVE_FORK
static int (*fork_redir) (void);
#endif

#define TCC_TYPE_NONE           (0)
#define TCC_TYPE_MALLOC         (1)
#define TCC_TYPE_CALLOC         (2)
#define TCC_TYPE_REALLOC        (3)
#define TCC_TYPE_MEMALIGN       (4)
#define TCC_TYPE_STRDUP         (5)

/* this pointer is generated when bound check is incorrect */
#define INVALID_POINTER ((void *)(-2))

typedef struct tree_node Tree;
struct tree_node {
    Tree * left, * right;
    size_t start;
    size_t size;
    unsigned char type;
    unsigned char is_invalid; /* true if pointers outside region are invalid */
};

typedef struct alloca_list_struct {
    size_t fp;
    void *p;
    size_t size;
    struct alloca_list_struct *next;
} alloca_list_type;

#if defined(_WIN32)
#define BOUND_TID_TYPE   DWORD
#define BOUND_GET_TID    GetCurrentThreadId()
#elif defined(__OpenBSD__)
#define BOUND_TID_TYPE   pid_t
#define BOUND_GET_TID    syscall (SYS_getthrid)
#elif defined(__FreeBSD__) || defined(__NetBSD__)
#define BOUND_TID_TYPE   pid_t
#define BOUND_GET_TID    0
#elif defined(__i386__) || defined(__x86_64__) || defined(__arm__) || defined(__aarch64__) || defined(__riscv)
#define BOUND_TID_TYPE   pid_t
#define BOUND_GET_TID    syscall (SYS_gettid)
#else
#define BOUND_TID_TYPE   int
#define BOUND_GET_TID    0
#endif

typedef struct jmp_list_struct {
    void *penv;
    size_t fp;
    size_t end_fp;
    BOUND_TID_TYPE tid;
    struct jmp_list_struct *next;
} jmp_list_type;

#define BOUND_STATISTIC_SPLAY   (0)
static Tree * splay (size_t addr, Tree *t);
static Tree * splay_end (size_t addr, Tree *t);
static Tree * splay_insert(size_t addr, size_t size, Tree * t);
static Tree * splay_delete(size_t addr, Tree *t);
void splay_printtree(Tree * t, int d);

/* external interface */
void __bounds_checking (int no_check);
void __bound_never_fatal (int no_check);
DLL_EXPORT void * __bound_ptr_add(void *p, size_t offset);
DLL_EXPORT void * __bound_ptr_indir1(void *p, size_t offset);
DLL_EXPORT void * __bound_ptr_indir2(void *p, size_t offset);
DLL_EXPORT void * __bound_ptr_indir4(void *p, size_t offset);
DLL_EXPORT void * __bound_ptr_indir8(void *p, size_t offset);
DLL_EXPORT void * __bound_ptr_indir12(void *p, size_t offset);
DLL_EXPORT void * __bound_ptr_indir16(void *p, size_t offset);
DLL_EXPORT void FASTCALL __bound_local_new(void *p1);
DLL_EXPORT void FASTCALL __bound_local_delete(void *p1);
void __bound_init(size_t *, int);
void __bound_main_arg(int argc, char **argv, char **envp);
void __bound_exit(void);
#if !defined(_WIN32)
void *__bound_mmap (void *start, size_t size, int prot, int flags, int fd,
                    off_t offset);
int __bound_munmap (void *start, size_t size);
DLL_EXPORT void __bound_siglongjmp(jmp_buf env, int val);
#endif
DLL_EXPORT void __bound_new_region(void *p, size_t size);
DLL_EXPORT void __bound_setjmp(jmp_buf env);
DLL_EXPORT void __bound_longjmp(jmp_buf env, int val);
DLL_EXPORT void *__bound_memcpy(void *dst, const void *src, size_t size);
DLL_EXPORT int __bound_memcmp(const void *s1, const void *s2, size_t size);
DLL_EXPORT void *__bound_memmove(void *dst, const void *src, size_t size);
DLL_EXPORT void *__bound_memset(void *dst, int c, size_t size);
DLL_EXPORT int __bound_strlen(const char *s);
DLL_EXPORT char *__bound_strcpy(char *dst, const char *src);
DLL_EXPORT char *__bound_strncpy(char *dst, const char *src, size_t n);
DLL_EXPORT int __bound_strcmp(const char *s1, const char *s2);
DLL_EXPORT int __bound_strncmp(const char *s1, const char *s2, size_t n);
DLL_EXPORT char *__bound_strcat(char *dest, const char *src);
DLL_EXPORT char *__bound_strchr(const char *string, int ch);
DLL_EXPORT char *__bound_strdup(const char *s);

#if defined(__arm__) && defined(__ARM_EABI__)
DLL_EXPORT void *__bound___aeabi_memcpy(void *dst, const void *src, size_t size);
DLL_EXPORT void *__bound___aeabi_memmove(void *dst, const void *src, size_t size);
DLL_EXPORT void *__bound___aeabi_memmove4(void *dst, const void *src, size_t size);
DLL_EXPORT void *__bound___aeabi_memmove8(void *dst, const void *src, size_t size);
DLL_EXPORT void *__bound___aeabi_memset(void *dst, int c, size_t size);
DLL_EXPORT void *__aeabi_memcpy(void *dst, const void *src, size_t size);
DLL_EXPORT void *__aeabi_memmove(void *dst, const void *src, size_t size);
DLL_EXPORT void *__aeabi_memmove4(void *dst, const void *src, size_t size);
DLL_EXPORT void *__aeabi_memmove8(void *dst, const void *src, size_t size);
DLL_EXPORT void *__aeabi_memset(void *dst, int c, size_t size);
#endif

#if MALLOC_REDIR
#define BOUND_MALLOC(a)          malloc_redir(a)
#define BOUND_MEMALIGN(a,b)      memalign_redir(a,b)
#define BOUND_FREE(a)            free_redir(a)
#define BOUND_REALLOC(a,b)       realloc_redir(a,b)
#define BOUND_CALLOC(a,b)        calloc_redir(a,b)
#else
#define BOUND_MALLOC(a)          malloc(a)
#define BOUND_MEMALIGN(a,b)      memalign(a,b)
#define BOUND_FREE(a)            free(a)
#define BOUND_REALLOC(a,b)       realloc(a,b)
#define BOUND_CALLOC(a,b)        calloc(a,b)
DLL_EXPORT void *__bound_malloc(size_t size, const void *caller);
DLL_EXPORT void *__bound_memalign(size_t size, size_t align, const void *caller);
DLL_EXPORT void __bound_free(void *ptr, const void *caller);
DLL_EXPORT void *__bound_realloc(void *ptr, size_t size, const void *caller);
DLL_EXPORT void *__bound_calloc(size_t nmemb, size_t size);
#endif

#define FREE_REUSE_SIZE (100)
static unsigned int free_reuse_index;
static void *free_reuse_list[FREE_REUSE_SIZE];

static Tree *tree = NULL;
#define TREE_REUSE      (1)
#if TREE_REUSE
static Tree *tree_free_list;
#endif
static alloca_list_type *alloca_list;
static jmp_list_type *jmp_list;

static unsigned char inited;
static unsigned char print_warn_ptr_add;
static unsigned char print_calls;
static unsigned char print_heap;
static unsigned char print_statistic;
static unsigned char no_strdup;
static unsigned char use_sem;
static int never_fatal;
#if HAVE_TLS_FUNC
#if defined(_WIN32)
static int no_checking = 0;
static DWORD no_checking_key;
#define NO_CHECKING_CHECK() if (!p) {                                         \
                                  p = (int *) LocalAlloc(LPTR, sizeof(int));  \
                                  if (!p) bound_alloc_error("tls malloc");    \
                                  *p = 0;                                     \
                                  TlsSetValue(no_checking_key, p);            \
                            }
#define NO_CHECKING_GET()   ({ int *p = TlsGetValue(no_checking_key);         \
                               NO_CHECKING_CHECK();                           \
                               *p;                                            \
                            })
#define NO_CHECKING_SET(v)  { int *p = TlsGetValue(no_checking_key);          \
                              NO_CHECKING_CHECK();                            \
                              *p = v;                                         \
                            }
#else
static int no_checking = 0;
static pthread_key_t no_checking_key;
#define NO_CHECKING_CHECK() if (!p) {                                         \
                                  p = (int *) BOUND_MALLOC(sizeof(int));      \
                                  if (!p) bound_alloc_error("tls malloc");    \
                                  *p = 0;                                     \
                                  pthread_setspecific(no_checking_key, p);    \
                            }
#define NO_CHECKING_GET()   ({ int *p = pthread_getspecific(no_checking_key); \
                               NO_CHECKING_CHECK();                           \
                               *p;                                            \
                            })
#define NO_CHECKING_SET(v)  { int *p = pthread_getspecific(no_checking_key);  \
                              NO_CHECKING_CHECK();                            \
                              *p = v;           