#' @include FacebookGenericCollection-class.R FacebookPagesCollection-class.R
#' 
#' @title 
#' Class for representing a Collection of Facebook posts
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook posts and build a \code{\link{FacebookPostsCollection-class}}
#' instance.
#' 
#' Use the \code{\link{FacebookPostsCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @name FacebookPostsCollection-class
#' @exportClass FacebookPostsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookPostsCollection",
         contains = "FacebookGenericCollection",
         validity = function(object){
           
           return(TRUE)
         }
)

setMethod("initialize",
          signature(.Object = "FacebookPostsCollection"),
          definition=function(.Object, id=NULL, token=NULL, parameters=list(), fields=character(0), feed=NULL, n = n){
            
            # Validate parameters
            validObject(.Object)
            
            token <- (function(){ 
              if(is.null(token) & is(id, "FacebookGenericCollection")){
                return(id@token)
              } else return(token)
            })()

            return(callNextMethod(.Object, id = id, token = token, parameters = parameters, fields = fields, n = n))
            
          }
)
