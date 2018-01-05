#' @include FacebookGenericCollection.R FacebookPagesCollection.R FacebookUsersCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook groups
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook groups and build a \code{\link{FacebookGroupsCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookGroupsCollection} is the constructor for the \code{\link{FacebookGroupsCollection-class}}.
#' It returns data about groups but doesn't return it member lists, although it \strong{does} return 
#' an approximate total count for both (depending on privacy settings of the users).
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the groups the users in the source collection belong to, assuming they granted the \code{user_managed_groups} permission.}
#' }
#' 
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookAlbumsCollection}}, \code{\link{FacebookUsersCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of groups to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available public group and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of groups in a \code{\link{FacebookGroupsCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Pull at most 10 groups from the current user
#'  fb.groups <- FacebookUsersCollection("me", fb_oauth) %>% FacebookGroupscollection(n = 10)
#'
#' ## Get the members of these groups
#'  fb.members <- FacebookUsersCollection(fb.groups)
#'  
#' ## Convert the collection to a data frame
#' fb.groups.df <- as.data.frame(fb.groups)
#' 
#' }
#' 
#' @family Facebook Collection Constructors
#' @importFrom plyr create_progress_bar progress_none join
FacebookGroupsCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = c("id",
                                               "cover.fields(source)",
                                               "owner.fields(id,name)",
                                               "created_time",
                                               "description",
                                               "name",
                                               "link"),
                                    n = getOption("facebook.maxitems"),
                                    metadata = FALSE,
                                    .progress = create_progress_bar(),
                                    stop.condition = function(x){ FALSE }){
  
  if(length(fields)==0){
    message("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  e.fields <- paste(paste0(fields, collapse=","), "members.summary(true).limit(0)", sep=",")
  
  if(is(id, "FacebookUsersCollection")){
    
    groups.idx <- new("FacebookGroupsCollection",
                     id = id,
                     token = token,
                     parameters = parameters,
                     fields = paste0("groups.fields(", e.fields, ")"),
                     n = n,
                     metadata = FALSE,
                     .progress = .progress,
                     stop.condition = stop.condition)
    
    if(metadata){
      the.groups <-  new("FacebookMixedCollection",
                        id = unique(groups.idx@id),
                        token = groups.idx@token,
                        parameters = parameters,
                        fields = "id",
                        n = n,
                        metadata = TRUE,
                        stop.condition = stop.condition)

      groups.idx@type <- join(data.frame(id=groups.idx@id, 
                                        stringsAsFactors = FALSE),
                             data.frame(id=the.groups@id, 
                                        type=the.groups@type, 
                                        stringsAsFactors = FALSE), 
                             by = "id")$type
    }
    
    return(groups.idx)
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a groups collection starting from a ", class(id), "."))
  }
  
  # Atomic IDs
  return(new("FacebookGroupsCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = e.fields,
             n = n,
             metadata = metadata,
             .progress = .progress,
             stop.condition = stop.condition))
}
