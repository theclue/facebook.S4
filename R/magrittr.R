#' Pipe an object forward into a function call/expression.
#' @export
#' @rdname pipe
#' @param lhs The value to be piped
#' @param rhs A function or expression
#' @source
#' This is just the imported function 
#' from the magrittr package. The documentation you should
#' read for the \%>\% function can be found here: \link[magrittr]{pipe}
#' 
#' Adding the function in the package while importing it 
#' is a trick taken from the {dplyr} package
#' (in the file chain.r)
#' 
#' @seealso \link[magrittr]{pipe}
#' @importFrom magrittr %>%
`%>%` <- magrittr::`%>%`
