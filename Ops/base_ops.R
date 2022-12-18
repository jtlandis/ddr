
box::use(../dispatch[...])

Ops_classes <- c("logical", "integer", "double", "complex")

#' @export
`+` <- new_generic("+", c("e1", "e2"), function(e1, e2) dispatch())

method(`+`, list(Ops_classes, Ops_classes)) <- function(e1, e2) {
  .Primitive("+")(e1, e2)
}

method(`+`, list(Ops_classes, "MISSING")) <- function(e1, e2) {
  .Primitive("+")(e1)
}

method(`+`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("+")(e1, e2)
}
#235

#' @export
`-` <- new_generic("-", c("e1", "e2"), function(e1, e2) dispatch())

method(`-`, list(Ops_classes, Ops_classes)) <- function(e1, e2) {
  .Primitive("-")(e1, e2)
}

method(`-`, list(Ops_classes, "MISSING")) <- function(e1, e2) {
  .Primitive("-")(e1)
}

method(`-`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("-")(e1, e2)
}

#' @export
`*` <- new_generic("*", c("e1","e2"), function(e1, e2) dispatch())

method(`*`, list(Ops_classes, Ops_classes)) <- function(e1, e2) {
  .Primitive("*")(e1, e2)
}

method(`*`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("*")(e1, e2)
}

#' @export
`/` <- new_generic("/", c("e1","e2"), function(e1, e2) dispatch())
method(`/`, list(Ops_classes, Ops_classes)) <- function(e1, e2) {
  .Primitive("/")(e1, e2)
}

method(`/`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("/")(e1, e2)
}

#' @export
`^` <- new_generic("^", c("e1","e2"), function(e1, e2) dispatch())
method(`^`, list(Ops_classes, Ops_classes)) <- function(e1, e2) {
  .Primitive("^")(e1, e2)
}

method(`^`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("^")(e1, e2)
}

#' @export
`%%` <- new_generic("%%", c("e1","e2"), function(e1, e2) dispatch())
method(`%%`, list(Ops_classes, Ops_classes)) <- function(e1, e2) {
  .Primitive("%%")(e1, e2)
}

method(`%%`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("%%")(e1, e2)
}

#' @export
`%/%` <- new_generic("%/%", c("e1","e2"), function(e1, e2) dispatch())
method(`%/%`, list(Ops_classes, Ops_classes)) <- function(e1, e2) {
  .Primitive("%/%")(e1, e2)
}

method(`%/%`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("%/%")(e1, e2)
}

#---- Logic ----

#' @export
`&` <- new_generic("&", c("e1","e2"), function(e1, e2) dispatch())
method(`&`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("&")(e1, e2)
}

#' @export
`|` <- new_generic("|", c("e1","e2"), function(e1, e2) dispatch())
method(`|`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("|")(e1, e2)
}

#' @export
`!` <- new_generic("!", c("x"), function(x) dispatch())
method(`!`, c("ANY")) <- function(x) {
  .Primitive("!")(x)
}

#---- Compare ----

#' @export
`==` <- new_generic("==", c("e1","e2"), function(e1, e2) dispatch())
method(`==`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("==")(e1, e2)
}

#' @export
`!=` <- new_generic("!=", c("e1","e2"), function(e1, e2) dispatch())
method(`!=`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("!=")(e1, e2)
}

#' @export
`<` <- new_generic("<", c("e1","e2"), function(e1, e2) dispatch())
method(`<`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("<")(e1, e2)
}

#' @export
`<=` <- new_generic("<=", c("e1","e2"), function(e1, e2) dispatch())
method(`<=`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive("<=")(e1, e2)
}

#' @export
`>=` <- new_generic(">=", c("e1","e2"), function(e1, e2) dispatch())
method(`>=`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive(">=")(e1, e2)
}

#' @export
`>` <- new_generic(">", c("e1","e2"), function(e1, e2) dispatch())
method(`>`, c("ANY", "ANY")) <- function(e1, e2) {
  .Primitive(">")(e1, e2)
}


