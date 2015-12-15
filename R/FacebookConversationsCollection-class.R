#' @include FacebookGenericCollection-class.R FacebookUsersCollection-class.R FacebookPagesCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook inbox conversations of users or pages
#'
#' @description
#' This class is used to collect Facebook inbox conversations in a convenient structure.
#' 
#' @name FacebookConversationsCollection-class
#' @exportClass FacebookConversationsCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookConversationsCollection",
         contains = "FacebookGenericCollection")
