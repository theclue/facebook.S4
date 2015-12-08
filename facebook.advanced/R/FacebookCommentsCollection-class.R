#' @include FacebookGenericCollection-class.R FacebookPostsCollection-class.R
#' 
#' @title 
#' Class for representing a Collection of Facebook comments
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook comments and build a \code{\link{FacebookCommentsCollection-class}}
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

setMethod("initialize",
          signature(.Object = "FacebookCommentsCollection"),
          definition=function(.Object, id=NULL, token=NULL, parameters=list(), fields=character(0), feed=NULL, n = n){
            
            # Validate parameters
            validObject(.Object)
            
            return(callNextMethod(.Object, id = id, token = token, parameters = parameters, fields = fields, n = n))
            
          }
)
