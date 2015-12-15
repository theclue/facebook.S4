#' @include FacebookGenericCollection-class.R
#' 
#' @title
#' A generic collection of Facebook elements
#' 
#' @description
#' This class is used to collect in a convenient structure elements of any kind.
#' 
#' @details 
#' This collection should not be built directly, as it performs no sanity check on its content.
#' However, among all the available collections, it's the only one that can have mixed content inside, so
#' many finder functions like \code{\link{facebook.search}} or \code{\link{facebook.object.likes}} fill this as return value.
#' But, for the same reason, it cannot hold fields different from \code{id}.
#' 
#' If you exactly know what you're doing you \emph{could} eventually build an instance of this class to perform generic queries to Graph API,
#' but it's not guaranteed to work and it probably won't, actually.
#' 
#' @template collection-slots
#'  
#' @name FacebookMixedCollection-class
#' @exportClass FacebookMixedCollection
#'  
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookMixedCollection",
         contains = "FacebookGenericCollection")
