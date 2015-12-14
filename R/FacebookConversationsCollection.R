#' @include FacebookGenericCollection.R FacebookPagesCollection.R FacebookUsersCollection.R
#' 
#' @title 
#' Build a Collection of Facebook Inbox Conversations
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of inbox conversations and build a \code{\link{FacebooConversationsCollection-class}}
#' instance.  Then, a \code{\link{FacebookMessagesCollection-class}} can be built to pull out the message of each conversation.
#' 
#' @details
#' \code{FacebookConversationssCollection} is the constructor for the \code{\link{FacebookConversationssCollection-class}}.
#' It returns data about inbox conversations of users or pages but doesn't return lists of messages or users involved
#' (altough it will return a summary view of both).
#' 
#' Consider using the twin functions \code{\link{FacebookMessagessCollection}}, \code{\link{FacebookUserssCollection}} to focus on these kinds
#' of items.
#' 
#' Use a \code{\link{FacebookPagesCollection-class}} as \code{id} to get a list of inbox conversations
#' related to a page (a page access token with \code{read_page_mailboxes} is needed) or a \code{\link{FacebookUserssCollection-class}}
#' (a user access token with \code{read_mailbox} is needed and \strong{the user must be a developer for the app making the request}).
#' 
#' Due to the network-graph nature of Facebook data model,
#' you can always specify fields details for each field eventually nesting \code{.fields()} clauses.
#' 
#' For example, if you need only \code{id} and \code{name} for the \code{from} node, this clause is valid among others:
#' \code{from.fields(id,name)}.
#' 
#' Assuming the senders granted the \code{user_posts} permission in the scope of the current application, you can pass 
#' a collection of \code{FacebookPostsCollection} as \code{id} parameter. In this cases, a collection of sharedposts from the
#' given posts will be fed.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPagesCollection}}, \code{\link{FacebookUsersCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is a Collection, then \code{n} is the maximum number of conversations to be pulled for any element of the Collection in \code{id}.
#' Otherwise, the parameter is ignored. It can be set to \code{Inf} to pull out any available public post and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing.
#'
#' @return A collection of posts in a \code{\link{FacebookPostsCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about two example Facebook Pages
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth)
#'  
#' }
#'
#' @family Facebook Collection Costructors
#' @export
FacebookConversationsCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = c("id", "snippet", "updated_time", "message_count"),
                                    feed = TRUE,
                                    n = getOption("facebook.maxitems"),
                                    metadata = FALSE,
                                    .progress = create_progress_bar()){
  
  if(length(fields)==0){
    message("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  #e.fields <- paste(paste0(fields, collapse=","), "participants.summary(true).limit(0)", sep=",")
  
  if(is(id, "FacebookUsersCollection") | is(id, "FacebookPagesCollection")){
    return(new("FacebookConversationsCollection", id = id, token = token, parameters = parameters, fields = paste0("conversations.fields(", paste0(fields, collapse=","), ")"), n = n, metadata = metadata, .progress = .progress))
  }
  
  return(new("FacebookConversationsCollection", id = id, token = token, parameters = parameters, fields = paste0(fields, collapse=","), n = n, metadata = metadata, .progress = .progress))
}
