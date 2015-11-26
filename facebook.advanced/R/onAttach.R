.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    packageStartupMessage('Welcome to facebook.advanced!')
  }
}
