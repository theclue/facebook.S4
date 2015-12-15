#' @include FacebookGenericCollection-class.R FacebookPostsCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook comments
#'
#' @description
#' This class is used to collect Facebook comments in a convenient structure.
#' 
#' Use the \code{\link{FacebookCommentsCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookCommentsCollection-class
#' @exportClass FacebookCommentsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookCommentsCollection",
         contains = "FacebookGenericCollection")
