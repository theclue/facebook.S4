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
            
            fields <- (function(f){ 
              if(length(f) > 0){
                e.fields <- paste(paste0(fields, collapse=","), "comments.summary(true).limit(0),likes.summary(true).limit(0)", sep=",")

                if(is(id, "FacebookPagesCollection")){
                  return(paste0(ifelse(!is.null(feed) & feed, "feed", "posts"), ".fields(", e.fields, ")"))
                }
                else {
                  return(e.fields)
                  }
              } else return(NULL)
            })(fields)
            
            token <- (function(){ 
              if(is.null(token) & is(id, "FacebookGenericCollection")){
                return(id@token)
              } else return(token)
            })()

            return(callNextMethod(.Object, id = id, token = token, parameters = parameters, fields = fields, n = n))
            
          }
)
