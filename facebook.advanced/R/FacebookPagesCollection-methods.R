#' @export
#' @title
#'  Checks if it's a valid collection
#'  
#'  @description
#'  \code{is.PagesCollrcyion} checks if \code{x} is a valid collection of Facebook Pages built using \code{\link{FacebookPagesCollection}},
is.PagesCollection <- function(x) is(x, "FacebookPagesCollection")

#' @describeIn as.list Convert to convert from a Collection of Pages.
setMethod("as.list", signature(x = "FacebookPagesCollection"), as.list.FacebookGenericCollection)

#' @describeIn as.data.frame to convert from a Collection of Pages.
setMethod("as.data.frame", signature(x = "FacebookPagesCollection", row.names = "logical", optional = "logical"), as.data.frame.FacebookGenericCollection)
