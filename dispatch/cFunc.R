

box::use(src = ./src)


.on_load <- function(ns) {
  
  for (i in 1:nrow(src$src_info)) {
    lib <- src$src_info$libname[i]
    funcs <- src$src_info$func[[i]]
    ddl <- dyn.load(lib)
    for (f in funcs)
      assign(f, eval(substitute(ddl$.name, list(.name = f))), envir = ns)

  }
  ns
}

.on_unload <- function(ns) {
  
  for (i in 1:nrow(src$src_info)) {
    dyn.unload(src$src_info$libname[i])
  }
}

#' dispatch_call
#'
#' forumlate the correct call based on methods registered through
#' The dispatch module
#' 
#' @param generic The generic function
#' @param envir Where the dispatch call is to be evaluated
#' @param genenv The generic environement where all methods are registered
#' @export
dispatch_call <- function(generic, envir, genenv) {
  .Call(method_call_, generic, envir, genenv, obj_class)
}

obj_class <- function(obj) {
  obj_type <- if(isS4(obj))
    "S4"
  else if (is.object(obj))
    "S3"
  else 
    "base"
  
  switch(obj_type,
         S4 = {
           self <- class(obj)
           extends <- unname(methods::extends(self, fullInfo = T))
           extends <- Filter(function(x) methods::is(x, "SClassExtension"), extends)
           classes <- lapply(extends, function(x) methods::getClass(x@superClass))
           classes <- Filter(function(x) {
             !x@virtual || 
               (x@virtual &&
                  methods::extends(x, "oldClass") &&
                  x@className != "oldClass") 
             }, classes)
           
           c(self, vapply(classes, function(x) x@className, character(1)))
         },
         S3 = class(obj),
         base = switch(typeof(obj),
                       closure = "function",
                       special = "function",
                       builtin = "function",
                       language = "call",
                       typeof(obj))
         )
}

