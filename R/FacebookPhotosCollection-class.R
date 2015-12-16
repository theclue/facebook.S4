#' @include FacebookGenericCollection-class.R FacebookPostsCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook photos
#'
#' @description
#' This class is used to collect Facebook photos in a convenient structure.
#' 
#' Use the \code{\link{FacebookPhotosCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookPhotosCollection-class
#' @exportClass FacebookPhotosCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookPhotosCollection",
         contains = "FacebookGenericCollection")
