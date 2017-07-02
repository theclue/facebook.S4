#' @include FacebookGenericCollection-class.R FacebookPostsCollection-class.R FacebookCommentsCollection-class.R
#' @export
#' 
#' @title 
#' Class for representing a Collection of Facebook likes
#'
#' @description
#' This class is used to collect Facebook likes in a convenient structure.
#' 
#' Use the \code{\link{FacebookLikesCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @details 
#' Although they are named the same in the Facebook jargon, likes to posts or comments are different to users' likes.
#' This collection covers the first, while the \code{\link{facebook.object.likes}} the latter.
#' 
#' @name FacebookLikesCollection-class
#' @exportClass FacebookLikesCollection
#' 
#' @seealso \code{\link{facebook.object.likes}}
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
setClass("FacebookLikesCollection",
         contains = "FacebookGenericCollection")
