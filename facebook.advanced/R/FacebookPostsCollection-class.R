#' A collection of valid Facebook Posts.
#' @name FacebookPostsCollection-class
#' @exportClass FacebookPostsCollection
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
          definition=function(.Object, id=NULL, token=NULL, parameters=list(), fields=character(0)){
            
            # Validate parameters
            validObject(.Object)
            
            fields <- (function(){ 
              if(length(fields)>0){
                return(c(fields, "comments.summary(true).limit(0),likes.summary(true).limit(0)"))
              }
            })()
            
            return(callNextMethod(.Object, id, token, parameters, fields))
            
          }
)
