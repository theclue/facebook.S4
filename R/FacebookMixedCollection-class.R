#' @include FacebookGenericCollection-class.R
#' 
#' @title
#' A generic collection of Facebook elements
#' 
#' @description
#' Connect to Facebook Graph API, get public information from a list of Facebook elements of any kind.
#' 
#' @details 
#' This collection should not be built directly, as it performs no sanity check on its content.
#' However, among all the available collections, it's the only one that can have mixed content inside, so
#' many commodity endpoint functions like \code{\link{facebook.search}} fill this as return value.
#' 
#' If you exactly know what you're doing you \emph{could} eventually build an instance of this class to perform generic queries to Graph API,
#' but it's not guaranteed to work and it probably won't, actually.
#' 
#' @name FacebookMixedCollection-class
#' @exportClass FacebookMixedCollection
#'  
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookMixedCollection",
         contains = "FacebookGenericCollection")
