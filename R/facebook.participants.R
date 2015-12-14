#' @rdname facebook.participants
#' @export
#'
#' @title 
#' Pull the list of participants from a set of inbox conversations
#'
#' @description
#' \code{facebook.participants} pulls information about the users and the pages who took part of a set of inbox conversations 
#' in a \code{link{FacebookConversationsCollection}} and push into a  \code{\link{FacebookMixedCollection-class}} instance.
#' 
#' @details
#' This function requires the use of a OAuth page access token with \code{read_pages_mailboxes}
#' permission granted if \code{id} is a collection of pages conversations
#' or a user access token with \code{read_mailbox} permission granted and
#' \strong{if the users are developers for the app making the request}
#' if \code{id} is a collection of users' conversations.
#' 
#' Only the \code{id} and the \code{type} is returned in a mixed collection.
#' Then, a proper collection for each type must be built accordingly.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookConversationsCollection}}, \code{\link{facebook.search}}, \code{\link{fbOAuth}}
#'
#' @param id An existing \code{\link{FacebookConversationsCollection}}
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{\link{fbOAuth}}. If \code{NULL} and \code{id} is a Collection, get that one instead. Otherwise, no query is performed
#' to the Facebook Graph API and an empty Collection is returned
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}
#' @param .progress progress_bar object as defined in the plyr package.
#' #' By default the \code{none} progress bar is used, which prints nothing to the console.
#' @param n An integer value with the maximum number of participants to be pulled for each conversation in \code{id}. It can be set to \code{Inf} 
#' to pull out any participant of a given conversation and assumes the default value to \code{facebook.maxitems} global option if missing. 
#'
#' @return A collection of mixed likes in a \code{\link{FacebookMixedCollection-class}} object with the \code{id} and the \code{type} for
#' each element included.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Returns the id and the type of the current user's likes
#'  me.likes <- facebook.users.likes(id = "me", token = fb_oauth)
#'  
#' ## Do the same, but starting from a users collection
#'  me.likes <- FacebookUsersCollection(id="me", fb_oauth) %>% facebook.users.likes()
#'  
#' ## Build a pages collection from all the pages (and ONLY the pages)
#' ## the current user likes
#'  me.likes.pages <- me.likes %>% FacebookPagesCollection()
#'}
#'
facebook.participants <- function(id, 
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
  

  participants.idx <- new("FacebookMixedCollection",
                          id = id,
                          token = token,
                          parameters = parameters,
                          fields = paste0("participants.fields(id).limit(", real.n , ")", sep=""),
                          n = n,
                          metadata = FALSE,
                          .progress = .progress)

  the.participants <- new("FacebookMixedCollection",
                          id = unique(participants.idx@id),
                          token = participants.idx@token,
                          fields="id",
                          parameters = parameters,
                          metadata = TRUE)
  
  participants.idx@type <- join(data.frame(id=participants.idx@id, 
                                              stringsAsFactors = FALSE),
                                data.frame(id=the.participants@id, 
                                              type=the.participants@type, 
                                              stringsAsFactors = FALSE), 
                                by = "id")$type
  
  return(participants.idx)
  
}
