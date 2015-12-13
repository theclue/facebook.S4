#' @include FacebookGenericCollection-methods.R FacebookPostsCollection-class.R FacebookCommentsCollection-class.R FacebookLikesCollection-class.R
#'
#' @title
#' Checks if the argument is a valid Collection of users
#'  
#' @description
#' \code{is.UsersCollection} checks if \code{x} is a valid collection of Facebook users built using \code{\link{FacebookUsersCollection}}.
#'  
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#'  
#' @export
is.UsersCollection <- function(x) is(x, "FacebookUsersCollection")

#' @describeIn as.list to convert from a Collection of Users.
setMethod("as.list", signature(x = "FacebookUsersCollection"), as.list.FacebookGenericCollection)

#' @describeIn as.data.frame to convert from a Collection of Users.
setMethod("as.data.frame", signature(x = "FacebookUsersCollection", row.names = "logical", optional = "logical"), 
          as.data.frame.FacebookGenericCollection)
