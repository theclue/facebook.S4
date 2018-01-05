#' @title 
#' Build a generic Collection of Facebook data
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook items and build a \code{\link{FacebookGenericCollection-class}}
#' instance.
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPostsCollection}}, \code{\link{FacebookCommentsCollection}}, \code{\link{fbOAuth}}
#'
#' @param id A character vector or a comma-delimited string of IDs or an existing Facebook Collection of any 
#' of the supported types (see below).
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{\link{fbOAuth}}. If token \code{NULL} and \code{id} is a collection,
#' use the token of the source collection. Otherwise, no query is performed
#' to the Facebook Graph API and an empty collection is returned.
#' 
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}.
#' 
#' @param fields A character vector with the fields to get for each \code{id}. If no value
#' for a given field is found, it will be set to \code{NULL}.
#' 
#' @param metadata If set to \code{TRUE}, the metadata for each ID is pulled with the data
#' and the \code{type} slot is fed accordingly.
#' Please note that setting this to \code{TRUE} could considerably
#' slow down the execution time, as more queries are needed.
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints nothing to the console. See \link[plyr]{create_progress_bar} for details.
#' 
#' @param stop.condition anonymous function that is executed for each element of the collection, passed to the function as a list called 'x'. If the function returns
#' \code{TRUE}, the execution of the query will end up at the end of the current page (even if more pages of data are available). By default, this function will
#' always return \code{FALSE}.
#'
#' @return A collection of elements in a \code{\link{FacebookGenericCollection-class}}
#' object or one of its subclasses.
#' 
#' @keywords internal
#' @importFrom plyr create_progress_bar
FacebookGenericCollection <- function(id, 
                            token, 
                            parameters = list(), 
                            fields = character(0),
                            .progress = create_progress_bar(),
                            metadata = FALSE,
                            stop.condition = function(x){ FALSE }){
  
  return(new("FacebookGenericCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = fields,
             metadata = metadata, 
             .progress = .progress,
             stop.condition = stop.condition))
}
