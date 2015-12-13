#' @title 
#' Build a generic Collection of Facebook data
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook items and build a \code{\link{FacebookGenericCollection-class}}
#' instance.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPostsCollection}}, \code{\link{FacebookCommentsCollection}}, \code{\link{fbOAuth}}
#'
#' @param id A character vector or a comma-delimited string of IDs or an existing Facebook Collection of any of the supported types
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{\link{fbOAuth}}. If \code{NULL} and \code{id} is a Collection, get that one instead. Otherwise, no query is performed
#' to the Facebook Graph API and an empty Collection is returned
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}
#' @param fields A character vector or a comma-delimited string with the list of fields to get for each ID. If no value for a given fields is found, it will
#' be set to \code{NULL}
#' @param metadata If set to \code{TRUE}, the medatata from each ID is pulled with the data and the \code{type} slot is fed. Please note
#' that setting this to \code{TRUE} could considerably slow down the execution time, as more queries are needed.
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints nothing to the console.
#'
#' @return A collection of elements in a \code{\link{FacebookGenericCollection-class}} object on on of its subclasses.
#' @keywords internal
FacebookGenericCollection <- function(id, 
                            token, 
                            parameters = list(), 
                            fields = character(0),
                            .progress = create_progress_bar(),
                            metadata = FALSE){
  
  return(new("FacebookGenericCollection", id = id, token = token, parameters = parameters, fields = fields, metadata = metadata, .progress = .progress))
}
