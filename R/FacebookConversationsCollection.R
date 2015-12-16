#' @include FacebookGenericCollection.R FacebookPagesCollection.R FacebookUsersCollection.R
#' @export
#' 
#' @title 
#' Build a Collection of Facebook inbox conversations
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of inbox conversations and build a \code{\link{FacebookConversationsCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookConversationsCollection} is the constructor for the \code{\link{FacebookConversationsCollection-class}}.
#' It returns data about inbox conversations of users or pages but doesn't return lists of messages or users involved.
#' 
#' A \code{\link{FacebookMessagesCollection-class}} can be built on top to pull out the messages of each conversation.
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookPagesCollection-class}} will build a collection with 
#'  all the inbox conversations to pages of the source collection. A page access token with \code{read_page_mailboxes} is needed
#'  to read the related mailboxes.}
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  all the conversations that person is involved with \strong{if they are a developer of the app making the request.}
#'  A user access token with \code{read_mailbox} is needed to perform this action.}
#'  \item{\code{\link{FacebookMixedCollection-class}} will build a collection with 
#'  all the conversations, filtering only to the \code{user} and \code{page} in the source collection.}
#' }
#' 
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookMessagesCollection}}, \code{\link{facebook.participants}}, \code{\link{facebook.senders}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of conversations to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available conversation and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of conversations in a \code{\link{FacebookConversationsCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_page_oauth")
#'  
#' ## Getting the conversations of an example page. A page access token is needed to access the inbox
#'  conversations <- FacebookConversationsCollection(FacebookPagesCollection(
#'                                                     "9thcirclegames", fb_page_oauth))
#' }
#'
#' @family Facebook Collection Costructors
#' @importFrom plyr create_progress_bar progress_none
FacebookConversationsCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = c("id",
                                               "snippet",
                                               "updated_time",
                                               "message_count"),
                                    n = getOption("facebook.maxitems"),
                                    metadata = FALSE,
                                    .progress = create_progress_bar()){
  
  if(length(fields)==0){
    message("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  #e.fields <- paste(paste0(fields, collapse=","), "participants.summary(true).limit(0)", sep=",")
  
  if(is(id, "FacebookUsersCollection") | is(id, "FacebookPagesCollection")){
    return(new("FacebookConversationsCollection",
               id = id,
               token = token,
               parameters = parameters, 
               fields = paste0("conversations.fields(", paste0(fields, collapse=","), ")"),
               n = n,
               metadata = metadata,
               .progress = .progress))
  }
  
  if(is(id, "FacebookMixedsCollection")){

    the.conversations <- new("FacebookConversationsCollection",
               id = id[which(id@type=="page" | id@type=="user")],
               token = token,
               parameters = parameters, 
               fields = paste0("conversations.fields(", paste0(fields, collapse=","), ")"),
               n = n,
               metadata = metadata,
               .progress = .progress)
    
    the.conversations@parent.collection <- id
    return(the.conversations)
  }
  
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a conversations collection from a ", class(id), "."))
  }
  
  # Atomic IDs
  return(new("FacebookConversationsCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = paste0(fields, collapse=","),
             n = n,
             metadata = metadata,
             .progress = .progress))
}
