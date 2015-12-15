#' @include FacebookPostsCollection.R FacebookAlbumsCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook comments to posts
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of Facebook comments to posts and build a \code{FacebookPostsCollection-class}
#' instance.
#' 
#' @details
#' \code{FacebookCommentsCollection} is the constructor for the \code{\link{FacebookCommentsCollection-class}}.
#' It returns data about comments but doesn't return lists of their own comments or likes,
#' altough it \strong{does} return an approximate total count for both (depending on the privacy settings of the users).
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookPostsCollection-class}} will build a collection with 
#'  the comments to the posts in the source collection.}
#'  \item{\code{\link{FacebookCommentsCollection-class}} will build a collection with 
#'  the replies to the comments in the source collection.}
#' }
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookLikesCollection}}, \code{\link{FacebookPostsCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of comments to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available comment and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of comments in a \code{\link{FacebookCommentsCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about two example Facebook Pages
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", 
#'                                             "NathanNeverSergioBonelliEditore"),
#'                                      token = fb_oauth)
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
#' # chaining first from a pages and then a posts collection
#' fb.comments.pipe <- 
#'  FacebookPagesCollection(id = c("9thcirclegames", 
#'                                 "NathanNeverSergioBonelliEditore"),
#'                          token = fb_oauth) %>% 
#'    FacebookPostscollection(n = 10) %>% 
#'    FacebookCommentsCollection(n = Inf)
#' }
#'
#' @family Facebook Collection Costructors
#' @importFrom plyr create_progress_bar progress_none
FacebookCommentsCollection <- function(id, 
                                       token = NULL, 
                                       parameters = list(), 
                                       fields = c("id",
                                                  "from.fields(id,name)",
                                                  "message",
                                                  "created_time"),
                                       n = getOption("facebook.maxitems"),
                                       metadata = FALSE,
                                       .progress = create_progress_bar()){
  
  if(length(fields)==0){
    message("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  real.n <- (function(n, p.limit){
    if(n > p.limit) {
      return(p.limit)
    }
    else {
      return(n)
    }
  })(n, getOption("facebook.pagination"))
  
  if(is(id, "FacebookPostsCollection") | is(id, "FacebookCommentsCollection") | is(id, "FacebookAlbumsCollection")){
    comments.fields <- paste0("comments.fields(", paste0(fields, collapse=",", sep=""), ",comments.summary(true).limit(0),likes.summary(true).limit(0)).limit(", real.n , ").summary(true)", sep="")
    return(new("FacebookCommentsCollection",
               id = id,
               token = token,
               parameters = parameters,
               fields = comments.fields,
               n = n,
               metadata = metadata,
               .progress = .progress))
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a comments collection starting from a ", class(id), "."))
  }
  # Atomic IDs
  return(new("FacebookCommentsCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = fields,
             n = n,
             metadata = metadata, 
             .progress = .progress))
}
