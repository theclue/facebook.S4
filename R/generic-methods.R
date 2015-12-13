#' @include magrittr.R
NULL

#' @title
#'  Returns a data frame representation of various kinds of Facebook collections
#' 
#' @param x A valid collection of Facebook elements
#' @param row.names If set to \code{TRUE}, names the rows of the returned data frame with IDs of the elements
#' @param optional Not used in this context.
#' @rdname as.data.frame
#' 
#' @export 
setGeneric("as.data.frame")

#' @title
#'  Returns a list representation of various kinds of Facebook collections
#' 
#' @param x A valid collection of Facebook elements
#' @rdname as.list
#' 
#' @export 
setGeneric("as.list")

#' @title
#'  Returns a summarized rapresentation of a Facebook collection
#' 
#' @param x A valid collection of Facebook elements
#' @rdname summary
#' 
#' @export 
setGeneric("summary")
