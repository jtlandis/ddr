
libname <- function(name) {
  box::file(paste0(name, .Platform$dynlib.ext))
}


#' @export
src_info <- tibble::tibble(
  name = c("method-dispatch"),
  func = list(c("method_call_", "get_class")),
  libname = libname(name)
)