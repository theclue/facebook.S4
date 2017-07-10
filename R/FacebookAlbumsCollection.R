#' @include FacebookGenericCollection.R FacebookPagesCollection.R FacebookUsersCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook albums
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook albums and build a \code{\link{FacebookAlbumsCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookAlbumsCollection} is the constructor for the \code{\link{FacebookAlbumsCollection-class}}.
#' It returns data about albums but doesn't return lists of comments or likes, although it \strong{does} return 
#' an approximate total count for both (depending on privacy settings of the users).
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookPagesCollection-class}} will build a collection with 
#'  the albums created on the pages in the source collection.}
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the albums created the users in the source collection. Users must have granted
#'  the \code{user_photos} permission to the current application to be able to perform this action.}
#'  \item{\code{\link{FacebookGroupsCollection-class}} will build a collection with 
#'  the albums created in the groups in the source collection. Users must have granted
#'  the \code{user_managed_groups} permission to the current app to be able to perform this action.}
#' }
#' 
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPagesCollection}}, \code{\link{FacebookUsersCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of albums to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available public album and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of albums in a \code{\link{FacebookAlbumsCollection-class}} object.
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
#' ## Pull all the available albums from each page
#'  fb.albums.inf <- FacebookAlbumscollection(id = fb.pages, token = fb_oauth, n = Inf)
#'  
#' ## Convert the collection to a data frame
#' fb.albums.df <- as.data.frame(fb.albums)
#' 
#' # The same as before in a more compact fashion using the pipe operator
#' # chaining from a Pages Collection
#'  fb.albums.pipe <- FacebookPagesCollection(id = c("9thcirclegames", 
#'                                                 "NathanNeverSergioBonelliEditore"),
#'                                           token = fb_oauth) %>% FacebookAlbumsCollection(n = 10)
#' }
#' 
#' @family Facebook Collection Constructors
#' @importFrom plyr create_progress_bar progress_none join
FacebookAlbumsCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = c("id",
                                               "cover_photo",
                                               "from.fields(id,name)",
                                               "created_time",
                                               "description",
                                               "name",
                                               "count"),
                                    n = getOption("facebook.maxitems"),
                                    metadata = FALSE,
                                    .progress = create_progress_bar()){
  
  if(length(fields)==0){
    message("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  e.fields <- paste(paste0(fields, collapse=","), "comments.summary(true).limit(0),likes.summary(true).limit(0)", sep=",")
  
  if(is(id, "FacebookPagesCollection") | is(id, "FacebookUsersCollection")){
    
    albums.idx <- new("FacebookAlbumsCollection",
                     id = id,
                     token = token,
                     parameters = parameters,
                     fields = paste0("albums.fields(", e.fields, ")"),
                     n = n,
                     metadata = FALSE,
                     .progress = .progress)
    
    if(metadata){
      the.albums <-  new("FacebookMixedCollection",
                        id = unique(albums.idx@id),
                        token = albums.idx@token,
                        parameters = parameters,
                        fields = "id",
                        n = n,
                        metadata = TRUE)

      albums.idx@type <- join(data.frame(id=albums.idx@id, 
                                        stringsAsFactors = FALSE),
                             data.frame(id=the.albums@id, 
                                        type=the.albums@type, 
                                        stringsAsFactors = FALSE), 
                             by = "id")$type
    }
    
    return(albums.idx)
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a albums collection starting from a ", class(id), "."))
  }
  
  # Atomic IDs
  return(new("FacebookAlbumsCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = e.fields,
             n = n,
             metadata = metadata,
             .progress = .progress))
}
