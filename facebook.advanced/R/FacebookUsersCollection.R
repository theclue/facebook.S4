#' @include FacebookGenericCollection.R
#' 
#' @title 
#' Build a Collection of Facebook Users
#'
#' @description
#' Connect to Facebook Graph API, get public information from a list of Facebook users and build a \code{\link{FacebookUsersCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookUsersCollection} is the constructor for the \code{\link{FacebookUsersCollection-class}} and it returns public data about users.
#' 
#' After version 2.0 of the Facebook API, only id, name, and picture are available
#' through the API as public informations. All the remaining fields will be missing unless the Application eventually asks
#' for extended permissions. 
#' 
#' Due to the network-graph nature of Facebook data model,
#' you can always specify fields details for each field eventually nesting \code{.fields()} clauses.
#'
#' For example, if you need only \code{id} and \code{source} for the \code{covern} node, this clause is valid among others:
#' \code{cover.fields(id,source)}.
#' 
#' Be careful when buinding this kind of collection starting from a \code{\link{FacebookPostsCollection}}, \code{\link{FacebookCommentsCollection}}
#' or a \code{\link{FacebookLikesCollection}}.
#' 
#' In Facebook one can publish, comment or like acting as a user or as a page. But since users and pages have different sets of fields 
#' and you won't know in advance if a (commenting) user is a page or not, the constructor of this collection would fail.
#' To avoid this, if \code{id} is an instance of one of the aforementioned collection, an intermediate \code{\link{FacebookMixedCollection}}
#' is built with 
#' this function will extract both. Parameters \code{users.fields} and \code{pages.fields}
#' are used to drive which attributes to get. 
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
#'  
#' ## Getting information about 9th Circle Games' Facebook Page
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth)
#'  
#' ## Getting the commenters of the latest 10 posts
#'  fb.comments <- fb.pages %>% FacebookPostsCollection(n = 10) %>% FacebookCommentsCollection(fields=c("id, "from.fields(id,name)"), n = Inf)
#'
#' ## Build a collection of users from who actually commented those posts  
#'  fb.commenting.users <- fb.comments -> FacebookUsersCollection()
#'  
#' ## Convert the collection to a data frame
#'  fb.commenting.df <- as.data.frame(fb.commenting.users)
#' }
#'
#' @export
FacebookUsersCollection <- function(id, 
                            token, 
                            parameters = list(), 
                            fields = c("id", "name", "first_name", "last_name", "gender", "locale", "picture.fields(url).type(large)"),
                            metadata = FALSE,
                            .progress = create_progress_bar()){
  

  return(new("FacebookUsersCollection", id = id, token = token, parameters = parameters, fields = fields, metadata = metadata, .progress = .progress))
}
