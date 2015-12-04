#' @include FacebookGenericCollection-class.R
#' 
#' @title 
#' Class for representing a Collection of Facebook pages
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook pages and build a \code{FacebookPagesCollection-class}
#' instance.
#' 
#' Use the \code{\link{FacebookPagesCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookPagesCollection-class
#' @exportClass FacebookPagesCollection
#'  
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookPagesCollection",
         contains = "FacebookGenericCollection",
         validity = function(object){
           # TBD
           return(TRUE)
         }
)
