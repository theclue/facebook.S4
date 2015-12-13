#' @include FacebookGenericCollection-methods.R FacebookPagesCollection-class.R
#'
#' @title
#' Checks if the argument is a valid Collection of pages
#'  
#' @description
#' \code{is.PagesCollection} checks if \code{x} is a valid collection of Facebook Pages built using \code{\link{FacebookPagesCollection}}.
#'  
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#'  
#' @export
is.PagesCollection <- function(x) is(x, "FacebookPagesCollection")

#' @describeIn as.list to convert from a Collection of Pages.
setMethod("as.list", signature(x = "FacebookPagesCollection"), as.list.FacebookGenericCollection)

#' @describeIn as.data.frame to convert from a Collection of Pages.
setMethod("as.data.frame", signature(x = "FacebookPagesCollection", row.names = "logical", optional = "logical"), 
          as.data.frame.FacebookGenericCollection)
