#' @include magrittr.R
NULL

#' @title
#'  Return parts of a Facebook collection
#'  
#' @description
#' This generic returns parts of a given Facebook collections
#' 
#' @param x A valid collection of Facebook elements
#' @param i slicing on the first dimension index
#' @param j not used in this context
#' @param ... not used in this context
#' @param drop not used in this context
#' 
#' @rdname square-methods
#' @name [
#' @exportMethod [
#' 
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about two example Facebook Pages
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames",
#'                                            "NathanNeverSergioBonelliEditore"), 
#'                                      token = fb_oauth)
#'  
#' ## Pull at most 20 albums from each page
#'  fb.albums <- FacebookAlbumscollection(id = fb.pages, token = fb_oauth, n = 20)
#'  
#' ## Create a new collection skipping the first 10 albums
#'  fb.oldest.albums <- fb.albums[11:length(fb.oldest.albums)]
#' }
setGeneric("[")

setGeneric("as.data.frame")

setGeneric("as.list")

#' @title
#'  Returns a summary of a Facebook collection
#'  
#' @description
#' This generic returns a summarized version from various Facebook collections
#' 
#' @param object A valid collection of Facebook elements
#' @param ... not used in this context
#' 
#' @rdname summary-methods
#' @name summary
#' @exportMethod summary
setGeneric("summary")
