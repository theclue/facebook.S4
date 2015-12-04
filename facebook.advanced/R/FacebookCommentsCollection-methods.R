#' @include FacebookGenericCollection-methods.R
#'
#' @title
#' Checks if the argument is a valid collection of comments
#'  
#' @description
#' \code{is.CommentsCollection} checks if \code{x} is a valid collection of Facebook Comments built using \code{\link{FacebookCommentsCollection}}.
#'  
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
is.CommentsCollection <- function(x) is(x, "FacebookCommentsCollection")

#' @describeIn as.list to convert from a Collection of Comments.
setMethod("as.list", signature(x = "FacebookCommentsCollection"), as.list.FacebookGenericCollection)

#' @describeIn as.data.frame to convert from a Collection of Comments.
setMethod("as.data.frame", signature(x = "FacebookCommentsCollection", row.names = "logical", optional = "logical"), 
          as.data.frame.FacebookGenericCollection)
