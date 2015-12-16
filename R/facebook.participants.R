#' @rdname facebook.participants
#' @export
#'
#' @title 
#' Pull the list of participants from a set of inbox conversations
#'
#' @description
#' \code{facebook.participants} pulls information about the users and the pages who took part of a set of inbox conversations 
#' in a \code{link{FacebookConversationsCollection-class}} and push into a \code{\link{FacebookMixedCollection-class}} instance.
#' 
#' @details
#' This function requires the use of a OAuth page access token with \code{read_pages_mailboxes}
#' permission granted if \code{id} is a collection of pages conversations
#' or a user access token with \code{read_mailbox} permission granted and
#' \strong{when the users are developers for the app making the request}
#' if \code{id} is a collection of users' conversations.
#' 
#' Only the \code{id} and the \code{type} is returned in a mixed collection.
#' Then, a proper collection for each type could be built accordingly.
#' 
#' Since this is a finder function, duplicated \code{id} won't be removed to the
#' output collection if they have a different \code{parent}.
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookConversationsCollection}}, \code{\link{facebook.search}}, \code{\link{facebook.senders}}
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
#' @param n An integer value with the maximum number of participants to be pulled for each conversation in \code{id}. It can be set to \code{Inf} 
#' to pull out any participant of a given conversation and assumes the default value to \code{facebook.maxitems} global option if missing. 
#'
#' @return A collection of users and pages in a \code{\link{FacebookMixedCollection-class}} object with the \code{id} and the \code{type} for
#' each element included.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Returns the conversations of the current user
#'  my.conversations <- FacebookUsersCollection(id="me", fb_oauth) %>%
#'     FacebookConversationsCollection()
#'  
#' ## Who talks to the current users
#'  participants <- facebook.participants(my.conversations) %>% FacebookUsersCollection()
#'}
#'
#' @family FacebookFinders
#' @importFrom plyr join create_progress_bar
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
