#' @include FacebookGenericCollection-class.R FacebookUsersCollection-class.R
#' 
#' @title 
#' Class for representing a Collection of Facebook user's likes.
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of Facebook user's liles and build a \code{\link{FacebookUserLikesCollection-class}}
#' instance.
#' 
#' Use the \code{\link{FacebookUserLikesCollection}} constructor for building a instance of this class, as it provides better
#' sanity and validation checks.
#' 
#' @details 
#' Although they are named the same in the Facebook jargon, posts/comments likes and users'likes are different entities.
#' This collection covers the latter, while \code{\link{FacebookLikesCollection-class}} the first.
#' 
#' Please note that user's likes cannot be pulled if the \code{token} used to perform the query has not granted the proper \code{user_likes} permission correctly
#' in the scope of the Facebook application the token is referring to.
#'   
#' @name FacebookUserLikessCollection-class
#' @exportClass FacebookUserLikessCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookUserLikesCollection",
         contains = "FacebookGenericCollection",
         validity = function(object){
           
           return(TRUE)
         }
)

setMethod("initialize",
          signature(.Object = "FacebookUserLikessCollection"),
          definition=function(.Object, id=NULL, token=NULL, parameters=list(), fields=character(0), n, metadata=FALSE, .progress = create_progress_bar()){
            
            return(callNextMethod(.Object, id = id, token = token, parameters = parameters, fields = fields, n = n, metadata = metadata, .progress = .progress))
            
          }
)
