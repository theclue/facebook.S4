#' @export
#' @title
#'  Checks if it's a valid collection
#'  
#'  @description
#'  \code{is.Pageset} checks if \code{x} is a valid collection of Facebook Pages built using \code{\link{FacebookPagesCollection}}
is.Pageset <- function(x) is(x, "FacebookPagesCollection")

#' @describeIn as.list Convert a collection of pages in a named list
setMethod("as.list", signature(x = "FacebookPagesCollection"), as.list.FacebookGenericCollection)

#' @describeIn as.data.frame Convert a valid Facebook Pages collection in a data frame.
setMethod("as.data.frame", signature(x = "FacebookPagesCollection", row.names = "logical", optional = "logical"), as.data.frame.FacebookGenericCollection)
