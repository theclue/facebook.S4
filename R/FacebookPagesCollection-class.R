#' @include FacebookGenericCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook pages
#'
#' @description
#' This class is used to collect Facebook pages in a convenient structure.
#' 
#' Use the \code{\link{FacebookPagesCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookPagesCollection-class
#' @exportClass FacebookPagesCollection
#' 
#' @template collection-slots
#'  
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookPagesCollection",
         contains = "FacebookGenericCollection")
