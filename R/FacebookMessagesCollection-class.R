#' @include FacebookGenericCollection-class.R FacebookUsersCollection-class.R FacebookPagesCollection-class.R FacebookPagesCollection-class.R
#' 
#' @title 
#' Class for representing a collection of Facebook messages between users or users and pages
#'
#' @description
#' This class is used to collect Facebook inbox messages in a convenient structure.
#' 
#' @name FacebookMessagesCollection-class
#' @exportClass FacebookMessagesCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookMessagesCollection",
         contains = "FacebookGenericCollection")
