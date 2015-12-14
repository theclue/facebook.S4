#' @include FacebookGenericCollection-class.R FacebookUsersCollection-class.R FacebookPagesCollection-class.R
#' 
#' @title 
#' Class for representing a Collection of Facebook inbox conversations of users or pages
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of inbox conversations and build a \code{FacebookConversationsCollection-class}
#' instance. Then, a \code{\link{FacebookMessagesCollection-class}} can be built to pull out the message of each conversation.
#' 
#' 
#' @name FacebookPostsCollection-class
#' @exportClass FacebookPostsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookConversationsCollection",
         contains = "FacebookGenericCollection")
