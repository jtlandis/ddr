

# Parser <- R6::R6Class(
#   "Parser",
#   public = list(
#     initialize = function(usage, args)
#   )
# )
#
#
# qual <- function(...) {
#   out <- rlang::list2(...)
#   class(out) <- c("qual", class(out))
#   out
# }
#
# field <- function(x = character()) {
#   stopifnot("field object must be a character"=is.character(x))
#   class(x) <- c("field", class(x))
#   x
# }
#
# new_qual <- function(`_class`) {
#   force(`_class`)
#   function(...) {
#     out <- qual(...)
#     class(out) <- c(`_class`, class(out))
#     out
#   }
# }
#
# opt <- new_qual("opt")
#
# req <- new_qual("req")
#
# multi <- new_qual("multi")
#
# exclsv <- new_qual("exclsv")
#
# new_field <- function(`_class`) {
#   force(`_class`)
#   function(x=character()) {
#     out <- field(x)
#     class(out) <- c(`_class`, class(out))
#     out
#   }
# }
#
# cmd <- new_field("cmd")
# flg <- new_field("flg")
# arg <- new_field("arg")

#
# dispatch <- function() {
#   #browser()
#   .call <- sys.call(-1)
#   .args <- lapply(.call[-1], identity)
#   .fun <- sys.function(-1)
#   gen_name <- attr(.fun, ".name")
#   genenv <- environment(.fun)[[".__dispatch__."]][[gen_name]]
#   if (is.null(genenv)) {
#     stop(sprintf(
#       "`dispatch` called in %s(), but method environment was not found. Did you construct this function with `new_generic()`?",
#       deparse1(.call[[1]])
#     ))
#   }
#   sig_args <- attr(genenv,".signature")
#   .frame <- sys.frame(-1)
#   .arg_nms <- names(.args)
#   if (!is.null(.arg_nms)) {
#     arg_indx <- match(sig_args, .arg_nms, nomatch = 0)
#     if (any(.lgl <- arg_indx==0)) {
#       arg_indx[.lgl] <- max(arg_indx) + seq_len(sum(.lgl))
#     }
#     sig_indx <- arg_indx[1:length(sig_args)]
#   } else {
#     sig_indx <- 1:length(sig_args)
#   }
#   sig_evaled <- eval_signature(.args[sig_indx], .frame)
#   .call[order(sig_indx) + 1] <- sig_evaled[[1]]
#   # if (!is.null(.arg_nms)) {
#   #   arg_indx <- match(.arg_nms, sig_args)
#   #   .m <- match(.arg_nms, sig_args, nomatch = 0L)
#   #   sig <- vector("list", length(sig_args))
#   #   sig[.m] <- .args[.m>0]
#   #   if (any(.lgl <- vapply(sig, is.null, FUN.VALUE = logical(1)))) {
#   #     sig[.lgl] <- .args[.m==0L][1:(sum(.lgl))]
#   #     xtra_args <- .args[.m==0L][-(1:(sum(.lgl)))]
#   #   } else {
#   #     xtra_args <- .args[.m==0L]
#   #   }
#   #
#   # } else {
#   #   .i <- 1:length(sig_args)
#   #   sig <- .args[1:length(sig_args)]
#   #   xtra_args <- .args[-.i]
#   # }
#   # sig_evaled <- eval_signature(sig, .frame)
#   .call[[1]] <- find_method(genenv, sig_args, sig_evaled[[2]], gen_name)
#   # new_call <- c(
#   #   list(find_method(genenv, sig_args , sig_evaled[[2]], gen_name)),
#   #   sig_evaled[[1]],
#   #   xtra_args
#   # )
#   # mode(new_call) <- "call"
#   eval(.call, envir = .frame)
# }

box::use(./cFunc[dispatch_call])

#' @export
dispatch <- function() {
  .call <- sys.call(-1)
  .fun <- sys.function(-1)
  gen_name <- attr(.fun, ".name")
  genenv <- environment(.fun)[[".__dispatch__."]][[gen_name]]
  if (is.null(genenv)) {
    stop(sprintf(
      "`dispatch` called in %s(), but method environment was not found. Did you construct this function with `new_generic()`?",
      deparse1(.call[[1]])
    ))
  }
  .frame <- sys.frame(-1)
  new_call <- dispatch_call(.fun, .frame, genenv)
  eval(new_call, .frame)
}


# eval_signature <- function(sig, frame) {
#   sig_evaled <- lapply(sig, eval, envir = frame)
#   sig_class <- lapply(sig_evaled, class)
#   is_super <- vapply(sig_evaled, inherits, what = "mod_dispatch_super", FUN.VALUE = logical(1))
#   if (any(is_super)) {
#     sig_evaled[is_super] <- lapply(sig_evaled, `[[`, 1)
#     sig_class[is_super] <- lapply(sig_evaled, `[[`, 2)
#   }
#
#   list(
#     sig_evaled,
#     sig_class
#   )
# }

#' @export
super <- function(from, to) {
  out <- list(
    from,
    class(to)
  )
  class(out) <- "mod_dispatch_super"
  out
}

#' @export
new_generic <- function(name, dispatch_args, fun) {

  defenv <- parent.frame()
  dspenv <- defenv[[".__dispatch__."]]
  if (is.null(dspenv)) {
    dspenv <- defenv[[".__dispatch__."]] <- new.env(parent = emptyenv())
  }
  genenv <- dspenv[[name]]
  if (is.null(genenv)) {
    genenv <- dspenv[[name]] <- new.env(parent=emptyenv())
  }
  if (is.null(fun)) {
    args <- c(dispatch_args, "...")
    args <- stats::setNames(lapply(args, function(i) quote(expr = )), args)
    defenv$.dispatch <- dispatch
    f <- function() {}
    formals(f) <- args
    body(f) <- quote(.dispatch())
    environment(f) <- defenv
    fun <- f
  }
  attr(genenv, ".signature") <- dispatch_args
  attr(fun, ".name") <- name
  return(fun)
}


#' @export
method <- function(generic, signature){
  gen_name <- attr(generic, ".name")
  genenv <- environment(generic)[[".__dispatch__."]][[gen_name]]
  disp_args <- attr(genenv, ".signature")

  if ((n <- length(disp_args))==1) {
    signature_classes <- list(class(signature))
  } else {
    signature_classes <- lapply(signature, class)
  }
  find_method(genenv, disp_args, signature_classes, gen_name)
}

search_table <- function(table, signature_class, i, depth) {

  # search along classes in the
  # signature
  arg_class <- signature_class[[i]]
  for (class_name in arg_class) {
    #Try and find class in current table
    tbl <- table[[class_name]]
    #If `class_name` is not found - try next
    if (is.null(tbl)) next
    # If it is found and this is the last
    # class - return the method
    # otherwise try next class
    if (i == depth) {
      return(tbl)
    } else {
      #Recalling either returns a method
      # or NULL from an error tryCatch

      tbl <- Recall(tbl, signature_class, i + 1L, depth)
      # if it is NULL -
      # no method in next arg for current
      # class_name. continue loop to next
      # class_name
      if (!is.null(tbl)) return(tbl)
    }
  }

  # Check fallback
  tbl <- table[["ANY"]]
  if (!is.null(tbl)) {
    if ( i == depth) {
      return(tbl)
    } else {
      tbl <- Recall(tbl, signature_class, i + 1L, depth)
      if (!is.null(tbl)) return(tbl)
    }
  }
  #Code reaching this point can only be NULL
  NULL

}

find_method <- function(genenv, disp_args, signature_classes, gen_name) {
  p_tbl <- genenv

  # search_ <- function(p_tbl, signature_class, i) {
    #
    # search along classes in the
    # signature
  #   arg_class <- signature_class[[i]]
  #   for (class_name in arg_class) {
  #     #Try and find class in current table
  #     tbl <- p_tbl[[class_name]]
  #     #If `class_name` is not found - try next
  #     if (is.null(tbl)) next
  #     # If it is found and this is the last
  #     # class - return the method
  #     # otherwise try next class
  #     if (i == length(signature_class)) {
  #       return(tbl)
  #     } else {
  #       #Recalling either returns a method
  #       # or NULL from an error tryCatch
  #       tbl <- tryCatch(
  #         Recall(tbl, signature_classes, i + 1L),
  #         error = function(cnd) {
  #           NULL
  #         }
  #       )
  #       # if it is NULL -
  #       # no method in next arg for current
  #       # class_name. continue loop to next
  #       # class_name
  #       if (!is.null(tbl)) return(tbl)
  #     }
  #   }
  #   #Code reaching this point can only be NULL
  #   stop(sprintf("Could not find method `%s` for arg `%s` of class <%s>",
  #                gen_name,
  #                disp_args[i], paste0(arg_class, collapse = "/")), call. = F)
  #
  # }

  method <- search_table(p_tbl, signature_classes, 1L, length(signature_classes))

  if (is.null(method)) {
    stop(sprintf("Could not find method %s(%s)",
                 gen_name,
                 paste0("<",vapply(
                   signature_classes, `[[`,
                   1, FUN.VALUE =  character(1)),
                   ">", collapse = ", ")), call. = F)
  }

  method

}

sanitize <- function(signature) {
  if (is.list(signature)) {
    lapply(
      signature,
      function(x) {
        y <- x %in% "numeric"
        if (any(y)) {
          x <- as.list(x)
          x[y] <- list(c("integer","double"))
          unlist(x)
        } else {
          x
        }
      }
    )
  } else if (is.character(signature)) {
    y <- signature %in% "numeric"
    if (any(y)) {
      x <- as.list(signature)
      x[y] <- list(c("integer", "double"))
      x
    } else {
      signature
    }
  }
}

expand_signature <- function(signature) {
  stopifnot('`signature` must be a list or character vector'=!is.object(signature))
  signature <- sanitize(signature)
  if (is.list(signature)) {
    stopifnot("all list elements must be character vectors"=all(vapply(signature, is.character, logical(1))))
    do.call(expand_combo, signature)
  } else if(is.character(signature)) {
    list(signature)
  }
}

expand_combo <- function(...) {
  args <- list(...)
  # signature length
  n <- length(args)
  # length of each char vec
  nargs <- vapply(args, length, integer(1))

  nout <- prod(nargs)

  out <- lapply(1:nout, function(x, n) character(n), n = n)

  nrep <- nout
  for (i in seq_len(n)) {
    vec <- args[[i]]
    nrep <- nrep/nargs[i]
    vec_rep <- rep_len(rep(vec, each = nrep), nout)
    for (j in seq_len(nout)) {
      out[[j]][i] <- vec_rep[j]
    }
  }
  out
}

#' @export
`method<-` <- function(generic, signature, value) {
  gen_name <- attr(generic, ".name")
  defenv <- environment(generic)

  dspenv <- defenv[[".__dispatch__."]]
  if (is.null(dspenv)) {
    dspenv <- defenv[[".__dispatch__."]] <- new.env(parent = emptyenv())
  }
  genenv <- dspenv[[gen_name]]
  if (is.null(genenv)) {
    eval(substitute(genenv <- dspenv$.name <- new.env(parent = emptyenv()),
                    list(.name = sym_gen)))
  }

  disp_args <- attr(genenv, ".signature")
  expanded_class <- expand_signature(signature)
  n <- length(disp_args)
  i_seq <- seq_len(n)
  for (j in seq_along(expanded_class)) {
    p_tbl <- genenv
    signature_class <- expanded_class[[j]]
    for (i in i_seq) {
      class_name <- signature_class[[i]]
      if (i != n) {
        tbl <- p_tbl[[class_name]]
        if (is.null(tbl)) {
          p_tbl[[class_name]] <- tbl <- new.env(parent = emptyenv())
        }
        p_tbl <- tbl
      } else {
        if (!is.null(p_tbl[[class_name]])) {
          message(sprintf(
            "Overwritting method %s(%s)",
            gen_name,
            paste0("<",signature_class, ">", collapse = ", ")
          ))
        }
        p_tbl[[class_name]] <- value
      }
    }
  }

  invisible(generic)

}



