#' @include FacebookGenericCollection-class.R FacebookPostsCollection-class.R FacebookCommentsCollection-class.R
#' 
#' @title 
#' Class for representing a Collection of Facebook likes
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook comments and build a \code{\link{FacebookLikesCollection-class}}
#' instance.
#' 
#' Use the \code{\link{FacebookLikesCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @details 
#' Although they are named the same in the Facebook jargon, posts/comments likes and users'likes are different entitites.
#' This collection covers the first, while \code{\link{FacebookUsersLikesCollection-class}} the latter.
#' 
#' @name FacebookLikesCollection-class
#' @exportClass FacebookLikesCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookLikesCollection",
         contains = "FacebookGenericCollection",
         validity = function(object){
           
           return(TRUE)
         }
)
