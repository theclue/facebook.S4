#' @include FacebookGenericCollection.R FacebookPagesCollection.R FacebookUsersCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook posts
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook posts and build a \code{\link{FacebookPostsCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookPostsCollection} is the constructor for the \code{\link{FacebookPostsCollection-class}}.
#' It returns data about posts but doesn't return lists of comments or likes, although it \strong{does} return 
#' an approximate total count for both (depending on privacy settings of the users).
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookPostsCollection-class}} will build a collection with 
#'  the shared posts from the posts of the source collection. It assumes the authors of the shared posts
#'  have granted the \code{user_posts} permission to the current application.}
#'  \item{\code{\link{FacebookVideosCollection-class}} will build a collection with 
#'  the shared posts from the videos of the source collection. It assumes the authors of the shared posts
#'  have granted the \code{user_posts} permission to the current application.}
#'  \item{\code{\link{FacebookPagesCollection-class}} will build a collection with 
#'  the posts written on the pages in the source collection.}
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the posts written on the walls of the users in the source collection. Users must have granted
#'  the \code{user_posts} permission to the current application to be able to perform this action.}
#'  \item{\code{\link{FacebookGroupsCollection-class}} will build a collection with 
#'  the posts written on the walls of the groups in the source collection. Users must have granted
#'  the \code{user_managed_groups} permission to the current application to be able to perform this action.}
#' }
#' 
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPagesCollection}}, \code{\link{FacebookCommentsCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param feed If \code{id} is a collection and \code{feed} is set to \code{TRUE}, the collection will also include posts 
#' written by others (not only by the owner of the collection items themselves). If \code{id} is not a collection or
#' it is a \code{\link{FacebookGroupsCollection-class}}, the parameter is ignored.
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of posts to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available public post and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of posts in a \code{\link{FacebookPostsCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about two example Facebook Pages
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames",
#'                                            "NathanNeverSergioBonelliEditore"), 
#'                                      token = fb_oauth)
#'  
#' ## Pull the latest 10 posts from each page
#'  fb.posts <- FacebookPostscollection(id = fb.pages, token = fb_oauth, n = 10)
#'  
#' ## Pull all the available posts from each page
#'  fb.posts.inf <- FacebookPostscollection(id = fb.pages, token = fb_oauth, n = Inf)
#'  
#' ## Convert the collection to a data frame
#' fb.posts.df <- as.data.frame(fb.posts)
#' 
#' # The same as before in a more compact fashion using the pipe operator
#' # chaining from a Pages Collection
#'  fb.posts.pipe <- FacebookPagesCollection(id = c("9thcirclegames", 
#'                                                 "NathanNeverSergioBonelliEditore"),
#'                                           token = fb_oauth) %>% FacebookPostsCollection(n = 10)
#'    
#' ## Build a collection of sharedposts from a posts collection
#'  fb.sharedposts <- FacebookPostsCollection(id = fb.posts, token = fb_oauth, n = Inf)
#' }
#' 
#' @family Facebook Collection Constructors
#' @importFrom plyr create_progress_bar progress_none join
FacebookPostsCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = c("id",
                                               "from.fields(id,name)",
                                               "message",
                                               "created_time",
                                               "type",
                                               "link",
                                               "name"),
                                    feed = TRUE,
                                    n = getOption("facebook.maxitems"),
                                    metadata = FALSE,
                                    .progress = create_progress_bar()){
  
  if(length(fields)==0){
    message("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  e.fields <- paste(paste0(fields, collapse=","), "comments.summary(true).limit(0),likes.summary(true).limit(0)", sep=",")
  
  if(is(id, "FacebookGroupsCollection")){
    feed <- TRUE
  }
  
  if(is(id, "FacebookPagesCollection") | is(id, "FacebookUsersCollection") | is(id, "FacebookGroupsCollection")){
    
    posts.idx <- new("FacebookPostsCollection",
                     id = id,
                     token = token,
                     parameters = parameters,
                     fields = paste0(ifelse(feed, "feed", "posts"), ".fields(", e.fields, ")"),
                     n = n,
                     metadata = FALSE,
                     .progress = .progress)
    
    if(metadata){
      the.posts <-  new("FacebookMixedCollection",
                        id = unique(posts.idx@id),
                        token = posts.idx@token,
                        parameters = parameters,
                        fields = "id",
                        n = n,
                        metadata = TRUE)

      posts.idx@type <- join(data.frame(id=posts.idx@id, 
                                        stringsAsFactors = FALSE),
                             data.frame(id=the.posts@id, 
                                        type=the.posts@type, 
                                        stringsAsFactors = FALSE), 
                             by = "id")$type
    }
    
    return(posts.idx)
  }
  
  if(is(id, "FacebookPostsCollection") | is(id, "FacebookVideosCollection")){
    return(new("FacebookPostsCollection",
               id = id,
               token = token,
               parameters = parameters,
               fields = paste0("sharedposts.fields(", e.fields, ")"),
               n = n,
               metadata = metadata,
               .progress = .progress))
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a posts collection starting from a ", class(id), "."))
  }
  
  # Atomic IDs
  return(new("FacebookPostsCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = e.fields,
             n = n,
             metadata = metadata,
             .progress = .progress))
}
