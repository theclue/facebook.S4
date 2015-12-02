#' @title 
#' Build a Collection of Facebook Pages
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
#' @inheritParams FacebookGenericCollection
#'
#' @return A collection of pages in a \code{\link{FacebookPagesCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#' ## Getting information about 9th Circle Games' Facebook Page
#'  fb.pages <- FacebookPagesCollection(id = "9thcirclegames", token = fb_oauth)
#' ## Convert the collection to a data frame
#' fb.pages.df <- as.data.frame(fb.pages)
#' }
#'
#' @export
FacebookPagesCollection <- function(id, 
                            token, 
                            parameters = list(), 
                            fields = "id,username,name,about,category,description,likes,link,talking_about_count"){
  
  return(new("FacebookPagesCollection", id = id, token = token, parameters = parameters, fields = fields))
}
