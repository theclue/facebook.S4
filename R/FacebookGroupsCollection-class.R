#' @include FacebookGenericCollection-class.R FacebookUsersCollection-class.R FacebookPagesCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook groups
#'
#' @description
#' This class is used to collect Facebook groups in a convenient structure.
#' 
#' Use the \code{\link{FacebookGroupsCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookGroupsCollection-class
#' @exportClass FacebookGroupsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookGroupsCollection",
         contains = "FacebookGenericCollection")