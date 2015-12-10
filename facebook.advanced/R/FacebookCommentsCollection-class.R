#' @include FacebookGenericCollection-class.R FacebookPostsCollection-class.R
#' 
#' @title 
#' Class for representing a Collection of Facebook comments
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook comments and build a \code{FacebookCommentsCollection-class}
#' instance.
#' 
#' Use the \code{\link{FacebookCommentsCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookCommentsCollection-class
#' @exportClass FacebookCommentsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookCommentsCollection",
         contains = "FacebookGenericCollection",
         validity = function(object){
           
           return(TRUE)
         }
)
