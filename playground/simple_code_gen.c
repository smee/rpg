#include <stdio.h>
#include <stdint.h>
//#include <xmmintrin.h>
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
#if !defined(__amd64__)
  #error "Unsupported architecture (only x86-64 is supported)."
#endif

// ---------------------------------------------------------------- X86CodeBuffer
typedef enum X86OpCode {
  RET = 0xC390, // RET then NOP
  MOVSD = 0x0510,
  MULSD = 0x0559
} X86OpCode;

typedef enum X86Register {
  XMM0 = 0x0FF2
} X86Register;

typedef struct X86CodeBuffer {
  size_t codeSize, dataSize;
  uint16_t *code;
  double *data; // TODO maybe use __m128 to assure alignment to 16 byte boundaries
  uint16_t *cp; // code pointer
  double *dp; // data pointer
} X86CodeBuffer;

static R_INLINE X86CodeBuffer *makeX86CodeBuffer(const size_t codeSize, const size_t dataSize) {
  X86CodeBuffer *x86CodeBuffer = Calloc(1, X86CodeBuffer);
  x86CodeBuffer->codeSize = codeSize;
  x86CodeBuffer->code = mmap(NULL, codeSize, PROT_EXEC | PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  x86CodeBuffer->cp = x86CodeBuffer->code;
  x86CodeBuffer->dataSize = dataSize;
  x86CodeBuffer->data = Calloc(dataSize, double);
  x86CodeBuffer->dp = x86CodeBuffer->data;
  return x86CodeBuffer;
}

static R_INLINE void *emitLoadMulDouble(X86CodeBuffer *x86CodeBuffer, double constant) {
  *x86CodeBuffer->dp =  constant;
  
  *x86CodeBuffer->cp++ = XMM0;
  //*x86CodeBuffer->cp++ = MOVSD;
  *x86CodeBuffer->cp++ = MULSD;
  
  // in x86-64, the address is relative to the address of the next instruction (RIP-relative addressing mode)...
  uint8_t *cpu8 = (uint8_t*) x86CodeBuffer->cp;
  uint8_t *dpu8 = (uint8_t*) x86CodeBuffer->dp;
  *((uint32_t*) x86CodeBuffer->cp) = (uint32_t) (dpu8 - (cpu8 + 4));
  x86CodeBuffer->cp = x86CodeBuffer->cp + 2;
  
  x86CodeBuffer->dp++;
}

static R_INLINE void *emitReturn(X86CodeBuffer *x86CodeBuffer) {
  *x86CodeBuffer->cp++ = RET;
}

static R_INLINE void freeX86CodeBuffer(X86CodeBuffer *x86CodeBuffer) {
  //Rprintf("freeX86CodeBuffer\n");
  munmap(x86CodeBuffer->code, x86CodeBuffer->codeSize);
  x86CodeBuffer->code = NULL;
  free(x86CodeBuffer->data);
  x86CodeBuffer->data = NULL;
  free(x86CodeBuffer);
  x86CodeBuffer = NULL;
}

static R_INLINE void writeProtectX86CodeBuffer(X86CodeBuffer *x86CodeBuffer) {
  if (mprotect(x86CodeBuffer->code, x86CodeBuffer->codeSize, PROT_EXEC | PROT_READ) < 0) {
    fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
  }
}


// ---------------------------------------------------------------------- x86Code
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

SEXP x86CodeGen(SEXP doubleConstant) {
  // allocate a readable and executable memory buffer...
  size_t codeSize = 16;
  size_t dataSize = 16;
  X86CodeBuffer *code = makeX86CodeBuffer(codeSize, dataSize);
  
  // directly generate x86 code into the memory buffer...
  double constant = REAL(doubleConstant)[0]; // extract constant
  emitLoadMulDouble(code, constant);
  emitReturn(code);
  
  // write-protect the memory buffer...
  writeProtectX86CodeBuffer(code);
  
  return makeX86Code(code);
}

SEXP executeX86Code(SEXP x86Code, SEXP argument) {
  // create a function pointer to the code memory buffer
  X86CodeBuffer *code = R_ExternalPtrAddr(x86Code);
  double (*codeFun)(double) = (double (*)(double)) code->code;
  
  // call the generated code
  double result = codeFun(REAL(argument)[0]);
  
  // return the result as an R integer
  SEXP rResult = allocVector(REALSXP, 1);
  REAL(rResult)[0] = result;
  
  return rResult;
}
