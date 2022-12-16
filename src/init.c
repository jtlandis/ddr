
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP hello_world(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"hello_world", (DL_FUNC) &hello_world, 1},
  {NULL, NULL, 0}
};

void R_init_myLib(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}