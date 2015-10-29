#' @rdname facebook.users.likes
#' @export
#'
#' @title 
#' Extract list of likes from Facebook users
#'
#' @description
#' \code{facebook.users.likes} retrieves information about the likes from a list of Facebook IDs and/or names.
#' 
#' @details
#' This is a tiny wrapper around the generic \code{\link{facebook.get}} and 
#' requires the use of an OAuth token with the following
#' permissions: user_likes, friends_likes
#' 
#' This function requires the use of a OAuth token with user_likes 
#' permission granted. After the introduction of version 2.0 of the Graph API,
#' only likes from users who are using the application that you used to generate the 
#' token to query the API will be returned.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{facebook.friends}}, \code{\link{fbOAuth}}
#'
#' @param users A vector or comma-delimited string with IDs or screen names.
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Maximum number of likes to return for each user.
#' 
#' @param fields vector or comma-separated string of fields to get
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints
#' nothing to the console.
#'
#' @examples \dontrun{
#'  load("fb_oauth")
#'  me.likes <- facebook.users.likes(users="me", token=fb_oauth)
#' }
#'

facebook.users.likes <- function(users, 
                                 token, 
                                 n=500, 
                                 fields="category,name,id,created_time",
                                 .progress = create_progress_bar()){
  
  return(facebook.get(ids = users, type = "likes", token = token, n = n, fields = fields, .progress = .progress))
  
}
