#' @rdname facebook.senders
#' @export
#'
#' @title 
#' Pull the list of senders from a set of inbox conversations
#'
#' @description
#' \code{facebook.senders} pulls information about the users who have sent a message in a set of inbox page conversations 
#' in a \code{\link{FacebookConversationsCollection-class}} and pushes into a \code{\link{FacebookMixedCollection-class}} instance.
#' 
#' @details
#' This function requires the use of a OAuth page access token with \code{read_pages_mailboxes}
#' permission granted.
#' 
#' Only the \code{id} and the \code{type} is returned in a mixed collection.
#' Then, a proper collection for each type could be built accordingly.
#' 
#' Since this is a finder function, duplicated \code{id} won't be removed to the
#' output collection unless they also have the same \code{parent}.
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookConversationsCollection}}, \code{\link{facebook.search}}, \code{\link{facebook.participants}}
#'
#' @param id An existing \code{\link{FacebookConversationsCollection}}.
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{\link{fbOAuth}}. If it's \code{NULL}, the token from \code{id} is used. Otherwise, no query is performed
#' to the Facebook Graph API and an empty Collection is returned.
#' 
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}.
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints nothing to the console. See \link[plyr]{create_progress_bar} for details.
#' 
#' @param n An integer value with the maximum number of senders to be pulled for each conversation in \code{id}. It can be set to \code{Inf} 
#' to pull out any sender of a given conversation and assumes the default value to \code{facebook.maxitems} global option if missing. 
#'
#' @return A collection of users in a \code{\link{FacebookMixedCollection-class}} object with the \code{id} and the \code{type} for
#' each element included.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Returns the conversations of a given page
#'  conversations <- FacebookPagesCollection(id="9thcirclegames", fb_oauth) %>%
#'    FacebookConversationsCollection()
#'  
#' ## Who sent a message to the page
#'  senders <- facebook.senders(conversations) %>% FacebookUsersCollection()
#'}
#'
#' @family FacebookFinders
#' @importFrom plyr join create_progress_bar
facebook.senders <- function(id, 
                                 token = NULL,
                                 parameters = list(),
                                 n = getOption("facebook.maxitems"), 
                                 .progress = create_progress_bar()){
  
  if(!is(id, "FacebookConversationsCollection")){
    if(is(id, "FacebookGenericCollection")){
      stop(paste0("you cannot build a likes collection from a ", class(id), "."))
    }
    stop("id must be a collection of one of the supported types.")
  }
  
  real.n <- (function(n, p.limit){
    if(n > p.limit) {
      return(p.limit)
    }
    else {
      return(n)
    }
  })(n, getOption("facebook.pagination"))
  

  senders.idx <- new("FacebookMixedCollection",
                          id = id,
                          token = token,
                          parameters = parameters,
                          fields = paste0("senders.fields(id).limit(", real.n , ")", sep=""),
                          n = n,
                          metadata = FALSE,
                          .progress = .progress)

  the.senders <- new("FacebookMixedCollection",
                          id = unique(senders.idx@id),
                          token = senders.idx@token,
                          fields="id",
                          parameters = parameters,
                          metadata = TRUE)
  
  senders.idx@type <- join(data.frame(id=senders.idx@id, 
                                              stringsAsFactors = FALSE),
                                data.frame(id=the.senders@id, 
                                              type=the.senders@type, 
                                              stringsAsFactors = FALSE), 
                                by = "id")$type
  
  return(senders.idx)
}
