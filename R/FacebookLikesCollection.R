#' @include FacebookPostsCollection.R FacebookCommentsCollection.R FacebookAlbumsCollection.R
#' @export
#' 
#' @title 
#' Build a Collection of Facebook Likes to posts and comments
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of likes to public posts or comments and build a \code{FacebookLikesCollection-class}
#' instance.
#' 
#' @details
#' \code{FacebookLikesCollection} is the constructor for the \code{\link{FacebookLikesCollection-class}}.
#' It returns data about likes to posts or comments but doesn't return the comments or posts themselves.
#' 
#' Since Facebook doesn't provide a key for a single like, the ID slot for this kind of collection doesn't uniquely identify a like on Facebook.
#' The \code{id} (the user who put the like) coupled with the \code{parent} (the place where she put the like) identifies a unique key for the like.
#' 
#' As a consequence, you cannot build a like collection starting from atomic IDs, but you must pass an instance of a Facecook Posts Collection or a Facebook Comments Collection built using the construction \code{\link{FacebookPostsCollection}}
#' or \code{\link{FacebookCommentsCollection}} as \code{id} parameter.
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookPostsCollection-class}} will build a collection with 
#'  all the likes to the posts of the source collection}
#'  \item{\code{\link{FacebookCommentsCollection-class}} will build a collection with 
#'  all the likes to the comments of the source collection}
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the posts written on the walls of the users in the source collection.}
#' }
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookCommentsCollection}},
#' \code{\link{FacebookPostsCollection}},
#' \code{\link{facebook.object.likes}},
#' \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of likes to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available public like and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of likes in a \code{\link{FacebookLikesCollection-class}} object.
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
#'  FacebookPagesCollection(id = c("9thcirclegames",
#'                                 "NathanNeverSergioBonelliEditore"),
#'                          token = fb_oauth) %>%
#'      FacebookPostscollection(n = 10) %>%
#'      FacebookLikesCollection(n = Inf)
#' }
#'
#' @family Facebook Collection Costructors
#' @importFrom plyr create_progress_bar progress_none
FacebookLikesCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = c("id", "name", "profile_type"),
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
  
  if(is(id, "FacebookPostsCollection") | is(id, "FacebookCommentsCollection")| is(id, "FacebookAlbumsCollection")){
    likes.fields <- paste0("likes.fields(", paste0(fields, collapse=",", sep=""), ").limit(", real.n , ").summary(true)", sep="")
    return(new("FacebookLikesCollection",
               id = id,
               token = token,
               parameters = parameters,
               fields = likes.fields,
               n = n,
               metadata = metadata,
               .progress = .progress))
  }
  
  stop(paste0("you cannot build a likes collection starting from a ", class(id), "."))
}
