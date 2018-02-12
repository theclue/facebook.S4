#' @include FacebookGenericCollection.R FacebookPagesCollection.R FacebookUsersCollection.R FacebookConversationsCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook inbox messages
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of inbox messages and build a \code{\link{FacebookMessagesCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookMessagesCollection} is the constructor for the \code{\link{FacebookMessagesCollection-class}}.
#' It returns data about inbox messages between users or users and pages.
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookConversationsCollection-class}} will build a collection with 
#'  all the messages from the given conversations. A page access token with \code{read_page_mailboxes} is needed
#'  to read the related mailboxes user access token with \code{read_mailbox} is needed to perform this action,
#'  in case of conversations between users.}
#' }
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookConversationsCollection}}, \code{\link{facebook.participants}}, \code{\link{facebook.senders}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of conversations to be pulled for each element of the source collection
#' in \code{id}. It can be set to \code{Inf} to pull out any available conversation and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of messages in a \code{\link{FacebookMessagesCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_page_oauth")
#'  
#' ## Getting the messages of the first conversation of an example page.
#' ## A page access token is needed to access the inbox
#'  messages <- FacebookPagesCollection("9thcirclegames", fb_page_oauth) %>%
#'              FacebookConversationsCollection(n = 1) %>%
#'              FacebookMessagesCollection()
#' }
#'
#' @family Facebook Collection Constructors
#' @importFrom plyr create_progress_bar progress_none
#' @importFrom futile.logger flog.warn
FacebookMessagesCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = c("id",
                                               "from",
                                               "created_time",
                                               "message"),
                                    n = getOption("facebook.maxitems"),
                                    metadata = FALSE,
                                    .progress = create_progress_bar(),
                                    stop.condition = function(x){ FALSE }){
  
  if(length(fields)==0){
    flog.warn("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  #e.fields <- paste(paste0(fields, collapse=","), "participants.summary(true).limit(0)", sep=",")
  
  if(is(id, "FacebookConversationsCollection")){
    return(new("FacebookMessagesCollection",
               id = id,
               token = token,
               parameters = parameters, 
               fields = paste0("messages.fields(", paste0(fields, collapse=","), ")"),
               n = n,
               metadata = metadata,
               .progress = .progress,
               stop.condition = stop.condition))
  }
  
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a messages collection from a ", class(id), "."))
  }
  
  # Atomic IDs
  return(new("FacebookMessagesCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = paste0(fields, collapse=","),
             n = n,
             metadata = metadata,
             .progress = .progress,
             stop.condition = stop.condition))
}
