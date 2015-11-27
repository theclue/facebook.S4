#' @title 
#' Extract list of pages from Facebook
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook pages and build a \code{\link{FacebookPagesCollection-class}}
#' instance.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPostset}}, \code{\link{FacebooCommentset}}, \code{\link{fbOAuth}}
#'
#' @param id A character vector or a comma-delimited string of pages IDs or an existing Facebook Pages collection
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}
#'
#' @param fields A character vector or a comma-delimited string with the list of fields to get for each page. If no value for a given fields is found, it will
#' be set to \code{NULL}
#'
#' @return A collection of pages in a \code{\link{FacebookPagesCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'  load("fb_oauth")
#'  fb_pages <- FacebookPagesCollection(pages="9thcirclegames", token=fb_oauth, parameters=list(since="2015-01-01"), fields="id,name,about")
#' ## Convert the collection to a data frame
#' fb.pages.df <- as.data.frame(fb_pages)
#' }
#'
#' @export
FacebookPagesCollection <- function(id, 
                            token, 
                            parameters = list(), 
                            fields = "id,username,name,about,category,description,likes,link,talking_about_count"){
  
  return(new("FacebookPagesCollection", id = id, token = token, parameters = parameters, fields = fields))
}
