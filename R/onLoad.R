.onLoad <- function(libname, pkgname) {
  op <- options()
  op.facebook <- list(
    facebook.api = "v2.10",
    facebook.maxitems = 500,
    facebook.verbose = FALSE,
    facebook.pagination = 25
  )
  toset <- !(names(op.facebook) %in% names(op))
  if(any(toset)) options(op.facebook[toset])
  
  invisible()
}
