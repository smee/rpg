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


SEXP simpleCodeGen() {
  // allocate a readable and executable memory buffer...
  size_t bufSize = 1000;
  uint8_t *buf = mmap(NULL, bufSize, PROT_EXEC | PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  // directly generate x86 code into the memory buffer...
  buf[0] = 0xb8;
  uint32_t u32 = 42;
  memcpy(buf + 1, &u32, 4);
  buf[5] = 0xc3;

  // write-protect the memory buffer...
  if (mprotect(buf, bufSize, PROT_EXEC | PROT_READ) < 0) {
    fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
    return R_NilValue;
  }

  // create a function pointer to the memory buffer 
  int (*code)(void) = (int (*)(void)) buf;

  // call the generated code
  Rprintf("generated code returned: %d\n", code());

  // free the memory buffer
  munmap(buf, bufSize);

  return R_NilValue;
}
