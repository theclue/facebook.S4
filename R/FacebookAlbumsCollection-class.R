#' @include FacebookGenericCollection-class.R FacebookUsersCollection-class.R FacebookPagesCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook albums
#'
#' @description
#' This class is used to collect Facebook albums in a convenient structure.
#' 
#' Use the \code{\link{FacebookAlbumsCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookAlbumsCollection-class
#' @exportClass FacebookAlbumsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookAlbumsCollection",
         contains = "FacebookGenericCollection")