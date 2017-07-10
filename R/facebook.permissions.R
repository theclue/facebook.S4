#' @rdname facebook.permissions
#' @export
#'
#' @title 
#' Extract information about the permissions for the current user
#'
#' @description
#' \code{facebook.permissions} retrieves informations about the permissions the current user actually has with the
#' current application.
#'
#' @details 
#' \code{facebook.permissions} returns a character vector where each row is an action the authenticated user is granted to perform
#' through Facebook Graph API with the current token.
#'
#' @author
#' Gabriele Baldassarre
#' @seealso \code{\link{fbOAuth}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about current user's permissions.
#'	load("fb_oauth")
#'	me.permissions <- facebook.permissions(token=fb_oauth)
#' }
#'
facebook.permissions <- function(token){
  
  content <- parseFbList(id = "me", token = token, fields="permissions")
  
  permissions <- sapply(content$me$permissions$data, function(x) { if(x$status == "granted") return(x$permission) })
  
  return(permissions)
  
}
