#' @rdname getPermissions
#' @export
#'
#' @title 
#' Extract information about the permissions for the current user
#'
#' @description
#' \code{getPermissions} retrieves informations about the permissions the current user actually has.
#'
#' @details 
#' \code{getPermissions} returns a character vector where each row is an action the authenticated user is granted to perform
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
#'	me.permissions <- getPermissions(token=fb_oauth)
#' }
#'

getPermissions <- function(token){
  
  url <- "https://graph.facebook.com/v2.3/?ids=me&fields=permissions"
  content <- callAPI(url=url, token=token)
  
  permissions <- sapply(p$me$permissions$data, function(x) { if(x$status == "granted") return(x$permission) })
  
  return(permissions)
  
}
