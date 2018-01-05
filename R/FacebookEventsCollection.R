#' @include FacebookPostsCollection.R FacebookAlbumsCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook events
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of Facebook events and build a \code{FacebookEventsCollection-class}
#' instance.
#' 
#' @details
#' \code{FacebookEventsCollection} is the constructor for the \code{\link{FacebookEventsCollection-class}}.
#' It returns metadata about events but doesn't return the list of attenders by default.
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the events of the users in the source collection. By default, only the attending events are returned, but
#'  this behaviour can be changed using the \code{type=(attending|created|declined|maybe|not_replied)} parameter.}
#'  \item{\code{\link{FacebookPagesCollection-class}} will build a collection with 
#'  the events linked to the pages in the source collection.}
#'  \item{\code{\link{FacebookGroupsCollection-class}} will build a collection with 
#'  the events linked to the groups in the source collection.}
#' }
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookGroupsCollection}}, \code{\link{FacebookPagesCollection}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of events to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available event and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of events in a \code{\link{FacebookEventsCollection-class}} object.
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
#'  fb.events <- FacebookEventscollection(id = fb.pages, token = fb_oauth, n = 10)
#'  
#' }
#'
#' @family Facebook Collection Constructors
#' @importFrom plyr create_progress_bar progress_none
FacebookEventsCollection <- function(id, 
                                     token = NULL, 
                                     parameters = list(), 
                                     fields = c("id",
                                                "owner.fields(id,name)",
                                                "category",
                                                "description",
                                                "end_time",
                                                "attending_count",
                                                "declined_count",
                                                "maybe_count",
                                                "noreply_count",
                                                "interested_count"),
                                     n = getOption("facebook.maxitems"),
                                     metadata = FALSE,
                                     .progress = create_progress_bar(),
                                     stop.condition = function(x){ FALSE }){
  
  if(length(fields)==0){
    message("You've specified no fields. Only the ID will be pulled into the collection.")
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
  
  if(is(id, "FacebookGroupsCollection") | is(id, "FacebookUsersCollection") | is(id, "FacebookPagesCollection")){
    events.fields <- paste0("events.fields(", paste0(e.fields, collapse=",", sep=""), ").limit(", real.n , ")", sep="")
    return(new("FacebookEventsCollection",
               id = id,
               token = token,
               parameters = parameters,
               fields = events.fields,
               n = n,
               metadata = metadata,
               .progress = .progress,
               stop.condition = stop.condition))
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a events collection starting from a ", class(id), "."))
  }
  # Atomic IDs
  return(new("FacebookEventsCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = e.fields,
             n = n,
             metadata = metadata, 
             .progress = .progress,
             stop.condition = stop.condition))
}
