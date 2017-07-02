#' @include FacebookGenericCollection-class.R FacebookPostsCollection-class.R FacebookCommentsCollection-class.R
#' @export
#' 
#' @title 
#' Class for representing a Collection of Facebook reactions
#'
#' @description
#' This class is used to collect Facebook reactions in a convenient structure.
#' 
#' Use the \code{\link{FacebookReactionsCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookReactionsCollection-class
#' @exportClass FacebookReactionsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
setClass("FacebookReactionsCollection",
         contains = "FacebookGenericCollection")
