#' @include FacebookGenericCollection-class.R FacebookUsersCollection-class.R FacebookPagesCollection-class.R FacebookGroupsCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook videos
#'
#' @description
#' This class is used to collect Facebook videos in a convenient structure.
#' 
#' Use the \code{\link{FacebookVideosCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookVideosCollection-class
#' @exportClass FacebookVideosCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookVideosCollection",
         contains = "FacebookGenericCollection")