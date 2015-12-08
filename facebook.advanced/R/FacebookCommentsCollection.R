#' @include FacebookPostsCollection.R
#' 
#' @title 
#' Build a Collection of Facebook Post Comments
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook comments to posts and build a \code{FacebookPostsCollection-class}
#' instance.
#' 
#' @details
#' \code{FacebookCommentsCollection} is the constructor for the \code{\link{FacebookCommentsCollection-class}}.
#' It returns data about comments but doesn't return lists of their own comments or likes (altough it could return a summary view of both).
#' 
#' 
#' Consider pass an instance of a Facecook Posts Collection build using the construction \code{\link{FacebookPostsCollection}},
#' if you need to bind a comment to its parent post.
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
#' @seealso \code{\link{FacebookLikessCollection}}, \code{\link{FacebookPostssCollection}}, \code{\link{fbOAuth}}
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
#' ## Pull the latest 10 posts from each page in a post collection
#'  fb.posts <- FacebookPostscollection(id = fb.pages, token = fb_oauth, n = 10)
#'  
#' ## Pull all the available posts from each page in a post collection
#'  fb.posts.inf <- FacebookPostscollection(id = fb.pages, token = fb_oauth, n = Inf)
#' 
#' ## Pull all the available comments from each post of the first collection
#'  fb.comments.inf <- FacebookPostscollection(id = fb.posts, token = fb_oauth)
#'    
#' ## Convert the collection to a data frame
#' fb.comments <- as.data.frame(fb.comments)
#' 
#' # The same as before in a more compact fashion using the pipe operator
#' # chaining from a Pages and then a Posts Collection
#' fb.comments.pipe <- 
#'  FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth) %>%
#'    FacebookPostscollection(n = 10) %>% FacebookCommentsCollection(n = Inf)
#' }
#'
#' @family Facebook Collection Costructors
#' @export
FacebookCommentsCollection <- function(id, 
                                       token = NULL, 
                                       parameters = list(), 
                                       fields = c("id", "from.fields(id,name)", "message", "created_time", "like_count"),
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
      
      if(is(id, "FacebookPostsCollection")){
        return(paste0("comments.fields(", paste0(f, collapse=",", sep=""), ").limit(", real.n , ").summary(true)", sep=""))
      }
      else {
        return(paste0(f, sep="", collapse=","))
      }
    } else return(NULL)
  })(fields)
  
  return(new("FacebookCommentsCollection", id = id, token = token, parameters = parameters, fields = fields, n = n))
}
