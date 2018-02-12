#' @include FacebookPostsCollection.R FacebookAlbumsCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook videos to posts
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of Facebook videos to posts and build a \code{FacebookPostsCollection-class}
#' instance.
#' 
#' @details
#' \code{FacebookVideosCollection} is the constructor for the \code{\link{FacebookVideosCollection-class}}.
#' It returns metadata about videos but doesn't return the raw image nor the various image formats available.
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the videos that belong to the users in the source collection, assuming they have given the \code{user_videos}
#'  or the \code{user_posts} permission to the current application.}
#'  \item{\code{\link{FacebookPagesCollection-class}} will build a collection with 
#'  the videos uploaded by the pages in the source collection.}
#'  \item{\code{\link{FacebookEventsCollection-class}} will build a collection with 
#'  the videos took at the events in the source collection.}
#' }
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookAlbumsCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of videos to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available video and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of videos in a \code{\link{FacebookVideosCollection-class}} object.
#'
#' @family Facebook Collection Constructors
#' @importFrom plyr create_progress_bar progress_none
#' @importFrom futile.logger flog.warn
FacebookVideosCollection <- function(id, 
                                     token = NULL, 
                                     parameters = list(), 
                                     fields = c("id",
                                                "from.fields(id,name)",
                                                "source",
                                                "length",
                                                "content_category",
                                                "created_time"),
                                     n = getOption("facebook.maxitems"),
                                     metadata = FALSE,
                                     .progress = create_progress_bar(),
                                     stop.condition = function(x){ FALSE }){
  
  if(length(fields)==0){
    flog.warn("You've specified no fields. Only the ID will be pulled into the collection.")
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
  
  e.fields <- paste(paste0(fields, collapse=","), "comments.summary(true).limit(0),likes.summary(true).limit(0)", sep=",")
  
  if(is(id, "FacebookGroupsCollection") | is(id, "FacebookUsersCollection") | is(id, "FacebookPagesCollection") | is(id, "FacebookEventsCollection")){
    videos.fields <- paste0("videos.fields(", paste0(e.fields, collapse=",", sep=""), ").limit(", real.n , ")", sep="")
    return(new("FacebookVideosCollection",
               id = id,
               token = token,
               parameters = parameters,
               fields = videos.fields,
               n = n,
               metadata = metadata,
               .progress = .progress,
               stop.condition = stop.condition))
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a videos collection starting from a ", class(id), "."))
  }
  # Atomic IDs
  return(new("FacebookVideosCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = e.fields,
             n = n,
             metadata = metadata, 
             .progress = .progress,
             stop.condition = stop.condition))
}
