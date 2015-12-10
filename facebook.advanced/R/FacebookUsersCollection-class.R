#' @include FacebookGenericCollection-class.R
#' 
#' @title 
#' Class for representing a Collection of Facebook users
#'
#' @description
#' Connect to Facebook Graph API, get public information from a list of Facebook users  nd build a \code{FacebookUsersCollection-class}
#' instance.
#' 
#' Use the \code{\link{FacebookUsersCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookUsersCollection-class
#' @exportClass FacebookPagesCollection
#'  
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookUsersCollection",
         contains = "FacebookGenericCollection"
         )
