#' @include FacebookGenericCollection-methods.R FacebookLikesCollection-class.R
#'
#' @title
#' Checks if the argument is a valid collection of likes
#'  
#' @description
#' \code{is.CommentsCollection} checks if \code{x} is a valid collection of Facebook Likes built using \code{\link{FacebookLikesCollection}}.
#' 
#' @details 
#' Although they are named the same in the Facebook jargon, posts/comments likes and users'likes are different entitites.
#' This method covers the first, while \code{\link{isUsersLikesCollection}} the latter. 
#' 
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
is.LikesCollection <- function(x) is(x, "FacebookLikesCollection")

#' @describeIn as.list to convert from a Collection of Likes.
setMethod("as.list", signature(x = "FacebookLikesCollection"), as.list.FacebookGenericCollection)

#' @describeIn as.data.frame to convert from a Collection of Likes.
setMethod("as.data.frame", signature(x = "FacebookLikesCollection", row.names = "logical", optional = "logical"), 
          as.data.frame.FacebookGenericCollection)
