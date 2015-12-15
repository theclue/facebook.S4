#' @include FacebookGenericCollection-class.R FacebookUsersCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook posts
#'
#' @description
#' This class is used to collect Facebook posts in a convenient structure.
#' 
#' Use the \code{\link{FacebookPostsCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookPostsCollection-class
#' @exportClass FacebookPostsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookPostsCollection",
         contains = "FacebookGenericCollection")