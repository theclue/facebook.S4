.onLoad <- function(libname, pkgname) {
  op <- options()
  op.facebook <- list(
    facebook.api = "2.5",
    facebook.maxitems = 500,
    facebook.verbose = FALSE,
    facebook.pagination = 25
  )
  toset <- !(names(op.facebook) %in% names(op))
  if(any(toset)) options(op.facebook[toset])
  
  invisible()
}
