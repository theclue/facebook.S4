#' @rdname getPost
#' @export
#'
#' @title 
#' Extract information about a public Facebook post
#'
#' @description
#' \code{getPost} retrieves information about a public Facebook post, including
#' list of comments and likes.
#'
#' @details 
#' \code{getPost} is a convenient wrapper for \code{\link{getPosts}} intented to be
#' 100% compatibile with the original Rfacebook function of the same name.
#' It is deprecated and it will drop sometime sooner or later. 
#' \code{getPost} returns a list with three components: \code{post}, 
#' \code{likes}, and \code{comments}. First, \code{post} contains information
#' about the post: author, creation date, id, counts of likes, comments, and 
#' shares, etc. Second, \code{likes} is a data frame that contains names and
#' Facebook IDs of all the users that liked the post. Finally, \code{comments}
#' is a data frame with information about the comments to the post (author, 
#' message, creation time, id).
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@gabrielebaldassarre.com}
#' @seealso \code{\link{getPosts}}
#'
#' @param post A post ID
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n numeric, maximum number of comments and likes to return.
#'
#' @param comments logical, default is \code{TRUE}, which will return data frame
#' with comments to the post.
#'
#' @param likes logical, default is \code{TRUE}, which will return data frame
#' with likes for the post.
#'
#' @param n.likes numeric, maximum number of likes to return. Default is \code{n}.
#'
#' @param n.comments numeric, maximum number of likes to return. Default is 
#' \code{n}.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'  load("fb_oauth")
#'	fb_page <- getPage(page="facebook", token=fb_oauth)
#' ## Getting information and likes/comments about most recent post
#'	post <- getPost(post=fb_page$id[1], n=2000, token=fb_oauth)
#' }
#'
getPost <- function(post, token, n=500, comments=TRUE, likes=TRUE, n.likes=n ,n.comments=n){
  
  warning("This function is deprecated and will be eventually dropped in future versions. Consider using the getPosts instead.")
  p <- getPosts(posts = post, token = token, n = n, n.likes = ifelse(likes == TRUE, n.likes, 0), n.comments = ifelse(comments == TRUE, comments, 0), fields = "from,message,created_time,type,link" )
  
  return(p)
  
}