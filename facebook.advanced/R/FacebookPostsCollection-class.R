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
          definition=function(.Object, id=NULL, token=NULL, parameters=list(), fields=character(0), feed=NULL, n = n){
            
            # Validate parameters
            validObject(.Object)
            
            fields <- (function(f){ 
              if(is(id, "FacebookPagesCollection")){
                return(paste0(ifelse(!is.null(feed) & feed, "feed", "posts"), ".fields(", f, ")"))
              }
              else return(f)
            })((function(){ 
              if(length(fields)>0){
                return(paste(fields, "comments.summary(true).limit(0),likes.summary(true).limit(0)", collapse=",", sep=","))
              }
            })())
            
            token <- (function(){ 
              if(is.null(token) & is(id, "FacebookGenericCollection")){
                return(id@token)
              } else return(token)
            })()

            return(callNextMethod(.Object, id = id, token = token, parameters = parameters, fields = fields, n = n))
            
          }
)
