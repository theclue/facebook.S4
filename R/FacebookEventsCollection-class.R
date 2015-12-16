#' @include FacebookGenericCollection-class.R FacebookPostsCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook events
#'
#' @description
#' This class is used to collect Facebook events in a convenient structure.
#' 
#' Use the \code{\link{FacebookEventsCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookEventsCollection-class
#' @exportClass FacebookEventsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookEventsCollection",
         contains = "FacebookGenericCollection")
