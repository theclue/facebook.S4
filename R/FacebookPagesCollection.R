#' @include FacebookGenericCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook Pages
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook pages and build a \code{\link{FacebookPagesCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookPagesCollection} is the constructor for the \code{\link{FacebookPagesCollection-class}}.
#' It returns data about pages but doesn't return lists of posts or the fanbase (altough it will return a summary view for the latter).
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookPagesCollection-class}} will build a collection with 
#'  the same elements as the source collection.}
#'  \item{\code{\link{FacebookMixedCollection-class}} will build a collection with 
#'  only the page elements of the source collection.}
#' }
#'  
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPostsCollection}}, \code{\link{FacebookCommentsCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#'
#' @return A collection of pages in a \code{\link{FacebookPagesCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about 9th Circle Games' Facebook Page
#' fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", 
#'                                            "NathanNeverSergioBonelliEditore"),
#'                                     token = fb_oauth)
#' 
#' ## Getting informations from the same pages, but with a different set of fields
#'  fb.pages.covers <- FacebookPagesCollection(id = fb.pages,
#'                                             fields = c("id",
#'                                                        "name",
#'                                                        "cover.fields(id,source,height,width)")
#'                                             )
#'  
#' ## Convert the collection to a data frame
#'  fb.pages.df <- as.data.frame(fb.pages)
#'  
#' ## Build a collection of the pages the current user likes
#'  likes.pages <- FacebookUsersCollection("me", fb_token, fields = "") %>%
#'    facebook.object.likes() %>% 
#'    FacebookPagesCollection()
#' }
#'
#' @family Facebook Collection Costructors
#' @importFrom plyr create_progress_bar progress_none
FacebookPagesCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = c("id", 
                                               "username", 
                                               "name", 
                                               "about", 
                                               "category", 
                                               "description", 
                                               "likes", 
                                               "link", 
                                               "talking_about_count"),
                                    metadata = FALSE,
                                    .progress = create_progress_bar()){
  
  if(length(fields)==0){
    message("You've specified no fields. Only the IDs will be pulled into the collection.")
    fields <- "id"
  }
  
  if(is(id, "FacebookPagesCollection") | is(id, "FacebookMixedCollection")){
    token <- (function(){
      if(is.null(token)) {
        return(id@token)
      }
      return(token)
    })()
    
    id.pages <- (function(){
      if(is(id, "FacebookMixedCollection")){
        return(id[which(id@type=="page")]@id)
      }
      return(id@id)
    })()

    the.pages <- new("FacebookPagesCollection",
                     id = id.pages,
                     token = token,
                     parameters = parameters,
                     fields = fields,
                     metadata = metadata,
                     .progress = .progress)
    
    the.pages@parent <- id.pages
    the.pages@parent.collection <- id
    return(the.pages)
  }
  
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a pages collection from a ", class(id), "."))
  }
  
  # Atomic IDs
  return(new("FacebookPagesCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = fields,
             metadata = metadata,
             .progress = .progress))
}
