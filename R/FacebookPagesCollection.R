#' @include FacebookGenericCollection.R
#' 
#' @title 
#' Build a Collection of Facebook Pages
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook pages and build a \code{\link{FacebookPagesCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookPagesCollection} is the constructor for the \code{\link{FacebookPagesCollection-class}}.
#' It returns data about pages but doesn't return lists of posts or the fanbase (altough it will return a summary view for the latters).
#' 
#' Due to the network-graph nature of Facebook data model,
#' you can always specify fields details for each field eventually nesting \code{.fields()} clauses.
#'
#' For example, if you need only \code{id} and \code{source} for the \code{covern} node, this clause is valid among others:
#' \code{cover.fields(id,source)}.
#'  
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPostset}}, \code{\link{FacebooCommentset}}, \code{\link{fbOAuth}}
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
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth)
#'  
#' ## Getting informations from the same pages, but with a different set of fields
#'  fb.pages.covers <- FacebookPagesCollection(id = fb.pages, fields = c("id", "name", "cover.fields(id,source,height,width)"))
#'  
#' ## Convert the collection to a data frame
#'  fb.pages.df <- as.data.frame(fb.pages)
#' }
#'
#' @export
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
    message("You've specified no fields. Only the ID will be pulled into the collection.")
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
    
    the.pages@parent.collection <- id
    return(the.pages)
  }
  
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a pages collection from a ", class(id), "."))
  }
  
  return(new("FacebookPagesCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = fields,
             metadata = metadata,
             .progress = .progress))
}
