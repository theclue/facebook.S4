#' @include FacebookGenericCollection-methods.R
#' 
#' @title
#' Checks if the argument is a valid collection of posts
#'  
#' @description
#' \code{is.PostsCollection} checks if \code{x} is a valid collection of Facebook Posts built using \code{\link{FacebookPostsCollection}}.
#'  
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
is.PostsCollection <- function(x) is(x, "FacebookPostsCollection")

#' @describeIn as.list to convert from a Collection of Pages.
setMethod("as.list", signature(x = "FacebookPostsCollection"), as.list.FacebookGenericCollection)

#' @describeIn as.data.frame to convert from a Collection of Pages.
setMethod("as.data.frame", signature(x = "FacebookPostsCollection", row.names = "logical", optional = "logical"), 
          as.data.frame.FacebookGenericCollection)
