#' @title 
#' Build a Collection of Facebook Posts
#'
#' @description
#' Connect to Facebook Graph API, get information from a list of public Facebook posts and build a \code{\link{FacebookPostsCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookPostsCollection} is the constructor for the \code{\link{FacebookPostsCollection-class}}.
#' It returns data about posts but doesn't return lists of comments or likes (altough it will return a summary view of both).
#' 
#' You can actually get some informations about comments and likes using fields nesting (see below), but this is not actually
#' recommended.
#' 
#' Consider using the twin functions \code{\link{FacebookCommentsCollection}}, \code{\link{FacebookLikesCollection}} to focuse on these nodes
#' of data.
#' 
#' Due to the network-graph nature of Facebook data model,
#' you can always specify fields details for each field eventually nesting \code{.fields()} clauses.
#' 
#' For example, if you need only \code{id} and \code{name} for the \code{from} node, this clause is valid among others:
#' \code{from.fields(id,name)}.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPagesCollection}}, \code{\link{FacebookCommentsCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param feed If \code{id} is a Collection and \code{feed} is set to If \code{TRUE}, the Collection will also include posts 
#' eritten by others (not only by the owner of the Collection items). If \code{id} is not a collection, the parameter is ignored.
#' 
#' @param n If \code{id} is a Collection, then \code{n} is the maximum number of posts to be pulled for any element of the Collection in \code{id}.
#' Otherwise, the parameter is ignored. It can be set to \code{Inf} to pull out any available public post.
#'
#' @return A collection of pages in a \code{\link{FacebookPagesCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about two example Facebook Pages
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth)
#'  
#' ## Pull the latest 10 posts from each page
#'  fb.posts <- FacebookPostscollection(id = fb.pages, token = fb_oauth, n = 10)
#'  
#' ## Convert the collection to a data frame
#' fb.posts.df <- as.data.frame(fb.posts)
#' 
#' # The same as before in a more compact fashion using the pipe operator
#' fb.posts.df <- as.data.frame(
#'  FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth) %>%
#'    FacebookPostscollection(n = 10)
#'  )
#' }
#'
#' @family Facebook Collection Costructors
#' @export
FacebookPostsCollection <- function(id, 
                                    token = NULL, 
                                    parameters = list(), 
                                    fields = "id,from.fields(id,name),message,created_time,type,link,name",
                                    feed = TRUE,
                                    n = getOption("facebook.maxitems")){
  
  return(new("FacebookPostsCollection", id = id, token = token, parameters = parameters, fields = fields, feed = feed, n = n))
}
