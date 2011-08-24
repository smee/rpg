#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/mman.h>
#include <R.h>
#include <Rinternals.h>

#if defined(__MACH__) || defined(__APPLE__)
  #define MAP_ANONYMOUS MAP_ANON
#elif defined(linux) || defined(__linux)
  // nothing to do
#else // Unsupported OS
  #error "Unsupported OS."
#endif


typedef struct X86CodeBuffer {
  size_t bufSize;
  uint8_t *buf;
} X86CodeBuffer;


static R_INLINE X86CodeBuffer *makeX86CodeBuffer(const size_t bufSize) {
  X86CodeBuffer *x86CodeBuffer = Calloc(1, X86CodeBuffer);
  x86CodeBuffer->bufSize = bufSize;
  x86CodeBuffer->buf = mmap(NULL, bufSize, PROT_EXEC | PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  return x86CodeBuffer;
}

static R_INLINE void freeX86CodeBuffer(X86CodeBuffer *x86CodeBuffer) {
  //Rprintf("freeX86CodeBuffer\n");
  munmap(x86CodeBuffer->buf, x86CodeBuffer->bufSize);
  Free(x86CodeBuffer);
}

static R_INLINE void writeProtectX86CodeBuffer(X86CodeBuffer *x86CodeBuffer) {
  if (mprotect(x86CodeBuffer->buf, x86CodeBuffer->bufSize, PROT_EXEC | PROT_READ) < 0) {
    fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
  }
}

void finalizeX86Code(SEXP x86Code) {
  //Rprintf("finalizeX86Code\n");
  freeX86CodeBuffer(R_ExternalPtrAddr(x86Code));
  R_ClearExternalPtr(x86Code);
}

static R_INLINE SEXP makeX86Code(X86CodeBuffer *x86CodeBuffer) {
  SEXP x86Code = PROTECT(R_MakeExternalPtr(x86CodeBuffer, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(x86Code, finalizeX86Code, TRUE);
  UNPROTECT(1);
  return x86Code;
}

SEXP simpleX86CodeGen() {
  // allocate a readable and executable memory buffer...
  size_t bufSize = 1000;
  X86CodeBuffer *code = makeX86CodeBuffer(bufSize);
  
  // directly generate x86 code into the memory buffer...
  code->buf[0] = 0xb8;
  uint32_t u32 = 42;
  memcpy(code->buf + 1, &u32, 4);
  code->buf[5] = 0xc3;
  
  // write-protect the memory buffer...
  writeProtectX86CodeBuffer(code);
  
  return makeX86Code(code);
}

SEXP executeX86Code(SEXP x86Code) {
  // create a function pointer to the code memory buffer
  X86CodeBuffer *code = R_ExternalPtrAddr(x86Code);
  int (*codeFun)(void) = (int (*)(void)) code->buf;
  
  // call the generated code
  int result = codeFun();

  // return the result as an R integer
  SEXP rResult = allocVector(INTSXP, 1);
  INTEGER(rResult)[0] = result;
  
  return rResult;
}
