#' @include FacebookPostsCollection.R FacebookAlbumsCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook photos to posts
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of Facebook photos to posts and build a \code{FacebookPhotosCollection-class}
#' instance.
#' 
#' @details
#' \code{FacebookPhotosCollection} is the constructor for the \code{\link{FacebookPhotosCollection-class}}.
#' It returns metadata about photos but doesn't return the raw image nor the various image formats available.
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookAlbumsCollection-class}} will build a collection with 
#'  the photos that belong to the albums in the source collection.}
#'  \item{\code{\link{FacebookEventsCollection-class}} will build a collection with 
#'  the photos shot at the events in the source collection.}
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the photos that belong to the users in the source collection, assuming they have given the \code{user_photos}
#'  permission to the current application.}
#'  \item{\code{\link{FacebookPagesCollection-class}} will build a collection with 
#'  the photos uploaded by the pages in the source collection. By default, the picture profile is returned, unless
#'  you specify the \code{type=uploaded} or the \code{type=tagged} parameters.}
#' }
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookAlbumsCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of photos to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available photo and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of photos in a \code{\link{FacebookPhotosCollection-class}} object.
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
#' ## Pull at most 10 albums from each page
#'  fb.albums <- FacebookAlbumscollection(id = fb.pages, token = fb_oauth, n = 10)
#'  
#' ## Pull all the available photos from each album
#'  fb.photos.inf <- FacebookPhotoscollection(id = fb.albums, n = Inf)
#' }
#'
#' @family Facebook Collection Constructors
#' @importFrom plyr create_progress_bar progress_none
#' @importFrom futile.logger flog.warn
FacebookPhotosCollection <- function(id, 
                                     token = NULL, 
                                     parameters = list(), 
                                     fields = c("id",
                                                "from.fields(id,name)",
                                                "link",
                                                "created_time"),
                                     n = getOption("facebook.maxitems"),
                                     metadata = FALSE,
                                     .progress = create_progress_bar(),
                                     stop.condition = function(x){ FALSE }){
  
  if(length(fields)==0){
    flog.warn("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  e.fields <- paste(paste0(fields, collapse=","), "comments.summary(true).limit(0),likes.summary(true).limit(0)", sep=",")
  
  real.n <- (function(n, p.limit){
    if(n > p.limit) {
      return(p.limit)
    }
    else {
      return(n)
    }
  })(n, getOption("facebook.pagination"))
  
  if(is(id, "FacebookAlbumsCollection") | is(id, "FacebookUsersCollection") | is(id, "FacebookPagesCollection") | is(id, "FacebookEventsCollection")){
    photos.fields <- paste0("photos.fields(", paste0(e.fields, collapse=",", sep=""), ").limit(", real.n , ")", sep="")
    return(new("FacebookPhotosCollection",
               id = id,
               token = token,
               parameters = parameters,
               fields = photos.fields,
               n = n,
               metadata = metadata,
               .progress = .progress,
               stop.condition = stop.condition))
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a photos collection starting from a ", class(id), "."))
  }
  # Atomic IDs
  return(new("FacebookPhotosCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = e.fields,
             n = n,
             metadata = metadata, 
             .progress = .progress,
             stop.condition = stop.condition))
}
