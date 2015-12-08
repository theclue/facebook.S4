#' @include FacebookPostsCollection.R FacebookCommentsCollection.R
#' 
#' @title 
#' Build a Collection of Facebook Post Comments
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook likes to posts or comments and build a \code{FacebookLikesCollection-class}
#' instance.
#' 
#' @details
#' \code{FacebookLikesCollection} is the constructor for the \code{\link{FacebookLikesCollection-class}}.
#' It returns data about likes to posts or comments but doesn't return the comments or posts themselves.
#' 
#' Since Facebook doesn't provide a key for a single like, the ID slot for this kind of collection is a matrix of two columns:
#' the user ID of who put the like and the post/comment ID of where the like was put on. This identifies a unique key for the like.
#' 
#' Consider pass an instance of a Facecook Posts Collection or a Facebook Comments Collection build using the construction \code{\link{FacebookPostsCollection}}
#' or \code{\link{FacebookCommentsCollection}}, if you need to bind to parent posts/comments.
#' 
#' Also, please note that this kind of collection cannot have mixed parents. You can eventually feed it with a FacebookPostsCollection
#' or a FacebookCommentsCollection, but no both at the same time: comments' likes and posts' likes are distinct collections.
#' 
#' Due to the network-graph nature of Facebook data model,
#' you can always specify fields details for each field eventually nesting \code{.fields()} clauses.
#' 
#' For example, if you need only \code{id} and \code{name} for the \code{from} node, this clause is valid among others:
#' \code{from.fields(id,name)}.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookCommentssCollection}}, \code{\link{FacebookPostssCollection}}, \code{\link{FacebookUsersLikesCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is a Collection, then \code{n} is the maximum number of posts to be pulled for any element of the Collection in \code{id}.
#' Otherwise, the parameter is ignored. It can be set to \code{Inf} to pull out any available public post and assume the default value from the value
#' of \code{facebook.maxitems} global option if missing.
#'
#' @return A collection of comments in a \code{\link{FacebookCommentsCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about two example Facebook Pages
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth)
#'  
#' ## Pull the latest 10 posts from each page in a pages collection
#'  fb.posts <- FacebookPostscollection(id = fb.pages, token = fb_oauth, n = 10)
#'  
#' ## Pull all the likes from each element of the posts collection
#' fb.posts.likes <- FacebookLikesCollection(fb.posts, fb_token, n = Inf)
#'  
#' ## Pull all the available comments from each post of the post collection
#'  fb.comments <- FacebookPostscollection(id = fb.posts, token = fb_oauth, n = Inf)
#'  
#' ## Pull all the likes from each element of the comments collections
#'  fb.comments.likes <- FacebookLikesCollection(id = fb.comments, token = fb_oauth, n = Inf)  
#'    
#' ## Convert the collection to a data frame
#' fb.posts.likes.df <- as.data.frame(fb.posts.likes)
#' 
#' # The same as before in a more compact fashion using the pipe operator
#' # chaining from a Pages then to a Posts Collection and finally building a Likes Collection
#' fb.posts.likes.pipe <- 
#'  FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth) %>%
#'    FacebookPostscollection(n = 10) %>% FacebookLikesCollection(n = Inf)
#' }
#'
#' @family Facebook Collection Costructors
#' @export
FacebookLikesCollection <- function(id, 
                                       token = NULL, 
                                       parameters = list(), 
                                       fields = c("id", "name", "profile_type"),
                                       n = getOption("facebook.maxitems")){
  
  real.n <- (function(n, p.limit){
    if(n > p.limit) {
      return(p.limit)
    }
    else {
      return(n)
    }
  })(n, getOption("facebook.pagination"))
  
  fields <- (function(f){ 
    if(length(f) > 0){
      
      if(is(id, "FacebookPostsCollection") | is(id, "FacebookCommentsCollection")){
        return(paste0("likes.fields(", paste0(f, collapse=",", sep=""), ").limit(", real.n , ").summary(true)", sep=""))
      }
      else {
        return(paste0(f, sep="", collapse=","))
      }
    } else return(NULL)
  })(fields)

  likes <- new("FacebookLikesCollection", id = id, token = token, parameters = parameters, fields = fields, n = n)
  keys <- as.matrix(cbind(likes@id, likes@parent))
  colnames(keys) <- c("id", ifelse(is(id, "FacebookPostsCollection"), "post.id", "comment.id"))
  likes@id <- keys
  return(likes)
  
}
