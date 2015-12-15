#' @include FacebookGenericCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook users
#'
#' @description
#' This class is used to collect Facebook pages in a convenient structure.
#' 
#' Use the \code{\link{FacebookUsersCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookUsersCollection-class
#' @exportClass FacebookPagesCollection
#'  
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' @export
setClass("FacebookUsersCollection",
         contains = "FacebookGenericCollection"
         )
