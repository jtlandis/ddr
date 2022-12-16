#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>



// Recursively walk through method table to perform iterated dispatch
SEXP method_rec(SEXP table, SEXP signature, R_xlen_t signature_itr) {
  //Rprintf("New Call\n");
  if (signature_itr >= Rf_xlength(signature)) {
    //Rprintf("signature_itr is longer than signatures\n");
    return R_NilValue;
  }
  //Rprintf("current signature_itr <%ld>\n", signature_itr);
  SEXP classes = VECTOR_ELT(signature, signature_itr);
  //Rprintf("current classes length <%ld>\n", Rf_xlength(classes));
  for (R_xlen_t i = 0; i < Rf_xlength(classes); ++i) {
    SEXP klass = Rf_install(CHAR(STRING_ELT(classes, i)));
    //Rprintf("<%s> <%ld>", CHAR(STRING_ELT(classes, i)), i);
    SEXP val = Rf_findVarInFrame(table, klass);
    
    if (TYPEOF(val) == ENVSXP) {
      val = method_rec(val, signature, signature_itr + 1);
    }
    if (TYPEOF(val) == CLOSXP) {
      return val;
    }
  }
  
  // ANY fallback
  SEXP val = Rf_findVarInFrame(table, Rf_install("ANY"));
  if (TYPEOF(val) == ENVSXP) {
    val = method_rec(val, signature, signature_itr + 1);
  }
  if (TYPEOF(val) == CLOSXP) {
    return val;
  }
  //Rprintf("Returning NULL");
  return R_NilValue;
}

/*
__attribute__ ((noreturn))
  void R7_method_lookup_error(SEXP generic, SEXP signature) {
    static SEXP R7_method_lookup_error_fun = NULL;
    SEXP ns = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("R7"));
    
    if (R7_method_lookup_error_fun == NULL) {
      R7_method_lookup_error_fun = Rf_findVarInFrame(ns, Rf_install("method_lookup_error"));
    }
    SEXP name = Rf_getAttrib(generic, Rf_install("name"));
    SEXP args = Rf_getAttrib(generic, Rf_install("dispatch_args"));
    SEXP R7_method_lookup_error_call = PROTECT(Rf_lang4(R7_method_lookup_error_fun, name, args, signature));
    Rf_eval(R7_method_lookup_error_call, ns);
    
    while(1);
  }
*/

SEXP method_(SEXP table, SEXP signature, SEXP error_) {

  SEXP m = method_rec(table, signature, 0);
  
  int error = Rf_asInteger(error_);
  if (error && m == R_NilValue) {
    Rf_error("Could no find method");
  }
  return m;
}

/*SEXP R7_obj_dispatch(SEXP object) {
  SEXP ns = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("R7"));
  
  static SEXP obj_dispatch_fun = NULL;
  if (obj_dispatch_fun == NULL) {
    obj_dispatch_fun = Rf_findVarInFrame(ns, Rf_install("obj_dispatch"));
  }
  
  SEXP obj_dispatch_call = PROTECT(Rf_lang2(obj_dispatch_fun, object));
  SEXP res = Rf_eval(obj_dispatch_call, ns);
  UNPROTECT(1);
  
  return res;
}*/
/*
SEXP R7_object_() {
  SEXP obj = PROTECT(Rf_allocSExp(S4SXP));
  Rf_classgets(obj, Rf_mkString("R7_object"));
  UNPROTECT(1);
  
  return obj;
}*/
/*
SEXP showArgs(SEXP args)
{
  //args = CDR(args); 
for(int i = 0; args != R_NilValue; i++, args = CDR(args)) {
  const char *name =
    Rf_isNull(TAG(args)) ? "" : CHAR(PRINTNAME(TAG(args)));
  SEXP el = CAR(args);
  if (Rf_length(el) == 0) {
    Rprintf("[%d] ‘%s’ R type, length 0\n", i+1, name);
    continue;
  }
  switch(TYPEOF(el)) {
  case REALSXP:
    Rprintf("[%d] ‘%s’ %f\n", i+1, name, REAL(el)[0]);
    break;
  case LGLSXP:
  case INTSXP:
    Rprintf("[%d] ‘%s’ %d\n", i+1, name, INTEGER(el)[0]);
    break;
  case CPLXSXP:
  {
    Rcomplex cpl = COMPLEX(el)[0];
    Rprintf("[%d] ‘%s’ %f + %fi\n", i+1, name, cpl.r, cpl.i);
  }
    break;
  case STRSXP:
    Rprintf("[%d] ‘%s’ %s\n", i+1, name,
            CHAR(STRING_ELT(el, 0)));
    break;
  default:
    Rprintf("[%d] ‘%s’ R type\n", i+1, name);
  }
}
return R_NilValue;
}*/

void report_log_(SEXP vec, char *context) {
  R_xlen_t n = Rf_xlength(vec);
  printf("report_log_ called. context : %s , length <%ld>\n", context, n);
  if (n > 0) {
    SEXP el = STRING_ELT(vec, 0);
    printf("vector: %s", CHAR(el));
    for (int i = 1; i < n; ++i) {
      el = STRING_ELT(vec, i);
      printf(", %s", CHAR(el));
    }
    printf("\n");
  }
  
}

SEXP get_class(SEXP obj) {
  return Rf_getAttrib(obj, Rf_install("class"));
}

SEXP obj_dispatch(SEXP object, SEXP fun, SEXP env) {
 /*SEXP ns = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("R7"));
 
 static SEXP obj_dispatch_fun = NULL;
 if (obj_dispatch_fun == NULL) {
 obj_dispatch_fun = Rf_findVarInFrame(ns, Rf_install("obj_dispatch"));
 }*/
 
 SEXP obj_dispatch_call = PROTECT(Rf_lang2(fun, object));
 SEXP res = Rf_eval(obj_dispatch_call, env);
 UNPROTECT(1);
 
 return res;
}

SEXP method_call_(SEXP generic, SEXP envir, SEXP genenv, SEXP class_fun) {
  int n_protect = 0;
  
  // Get the number of arguments to the generic
  SEXP formals = FORMALS(generic);
  //showArgs(formals);
  R_xlen_t n_args = Rf_xlength(formals);
  // And how many are used for dispatch
  SEXP dispatch_args = Rf_getAttrib(genenv, Rf_install(".signature"));
  //report_log_(dispatch_args, "Finding Signature");
  R_xlen_t n_dispatch = Rf_xlength(dispatch_args);
  //printf("args length is <%ld>\n", n_dispatch);
  // Allocate a list to store the classes for the arguments
  SEXP dispatch_classes = PROTECT(Rf_allocVector(VECSXP, n_dispatch));
  ++n_protect;
  
  // Allocate a pairlist to hold the arguments for when we call the method
  SEXP mcall = PROTECT(Rf_lcons(R_NilValue, R_NilValue));
  ++n_protect;
  SEXP mcall_tail = mcall;
  //printf("Starting to parse formals\n");
  // For each of the arguments to the generic
  for (R_xlen_t i = 0; i < n_args; ++i) {
    //printf("index <%ld>\n", i);
    // Find its name and look up its value (a promise)
    SEXP name = TAG(formals);
    //printf("current arg '%s'\n", Rf_isNull(name) ? "" : CHAR(PRINTNAME(name)));
    SEXP arg = Rf_findVar(name, envir);
    //report_log_(name);
    if (i < n_dispatch) {
      if (PRCODE(arg) != R_MissingArg) {
        // Evaluate the original promise so we can look up its class
        SEXP val = PROTECT(Rf_eval(arg, R_EmptyEnv));
        
        if (!Rf_inherits(val, "mod_dispatch_super")) {
          // Update the value of the promise to avoid evaluating it
          // again in the method body
          SET_PRVALUE(arg, val);
          
          // Then add to arguments of method call
          SETCDR(mcall_tail, Rf_cons(arg, R_NilValue));
          
          // Determine class string to use for method look up
          //printf("Determine class string\n");
          SET_VECTOR_ELT(dispatch_classes, i, obj_dispatch(val, class_fun, envir));
          //report_log_(VECTOR_ELT(dispatch_classes, i), "Verifying class string");
        } else {
          // If it's a superclass, we get the stored value and dispatch class
          SEXP true_val = VECTOR_ELT(val, 0);
          SET_PRVALUE(arg, true_val);
          SETCDR(mcall_tail, Rf_cons(arg, R_NilValue));
          SET_VECTOR_ELT(dispatch_classes, i, VECTOR_ELT(val, 1));
          //report_log_(VECTOR_ELT(dispatch_classes, i), "Super class verifying string");
        }
        UNPROTECT(1);
      } else {
        SETCDR(mcall_tail, Rf_cons(name, R_NilValue));
        SET_VECTOR_ELT(dispatch_classes, i, Rf_mkString("MISSING"));
      }
    } else {
      // other arguments not used for dispatch
      SEXP arg_wrap = Rf_cons(name, R_NilValue);
      SET_TAG(arg_wrap, name);
      SETCDR(mcall_tail, arg_wrap);
    }
    
    mcall_tail = CDR(mcall_tail);
    formals = CDR(formals);
  }
  
  // Now that we have all the classes, we can look up what method to call
  SEXP m = method_(genenv, dispatch_classes, Rf_ScalarLogical(1));

  SETCAR(mcall, m);
  UNPROTECT(n_protect);
  return mcall;
}
