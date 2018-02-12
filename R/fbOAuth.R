#' @rdname fbOAuth
#' @export
#'
#' @title 
#' Create OAuth token to Facebook R session
#'
#' @description
#' \code{fbOAuth} creates a long-lived OAuth access token that enables R to make
#' authenticated calls to the Facebook API. The token can be saved as a
#' file in disk to be re-used in future sessions. This function relies on the
#' \code{httr} package to create the OAuth token, and is a simplified version
#' of one of its examples.
#'
#' This function will automatically detect the API version for the token you create. 
#'
#' @details
#' There are two different ways of making authenticated requests. One is to obtain
#' a temporary access token from \url{https://developers.facebook.com/tools/explorer/},
#' which can be used as argument in any of the functions in Rfacebook. An example is 
#' shown below.
#'
#' However, this token has a 2-hour lifetime by default and after it expires, it
#' needs to be renewed. The second alternative is to create an OAuth token. The 
#' process to create it is a bit more tedious. It is divided in three steps.
#' 
#' First, go to \url{https://developers.facebook.com/apps}, register as a developer
#' and create a new app. You will also need a verified Facebook account.
#' After that, click in "Show" under "App Secret" to find your 'App ID' and 'App Secret'.
#'
#' Second, run the \code{fbOAuth} function with your "App ID" and "App Secret" as 
#' arguments.
#'
#' Third, after pressing Enter, R will try to open a browser window to sign the token. If 
#' everything works well, you will get a message that says you can return to R. If not,
#' try again in a few minutes to make sure your application had its settings updated properly.
#' 
#' If you set \code{cache=TRUE}, a file \code{.httr-oauth} is then saved in your home directory.
#' This file caches the OAuth credentials so you don't need to perform the browser authentication a second time
#' if you don't change the App ID, the App Secret or the permissions you grant.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu},
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{facebook.search}}, \code{\link{FacebookPagesCollection}}, \code{\link{FacebookPostsCollection}}
#'
#' @param app_id numeric, App ID of application to be used to create OAUth token. Available
#' at \url{https://developers.facebook.com/apps}
#' 
#' @param app_secret string, App Secret of application to be used to create OAUth token.
#' Available at \url{https://developers.facebook.com/apps}, in Basic Settings panel.
#' 
#' @param cache If set to \code{TRUE} a file with the authentication details for the current user
#' is saved to disk as \code{.http-oauth}, so it won't be asked to authenticate through the browser anymore.
#'
#' @param permissions A character vector with a list of the permissions to grant to the application.
#' After version 2.0 of the Graph API, creating an application with extended permissions
#' requires passing App Review (\url{https://developers.facebook.com/docs/facebook-login/review})
#'
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#'	me <- FacebookUsersCollection("me", token=fb_oauth)
#'	as.data.frame(me)
#' 
#' ## an example of a request using a temporary access token
#' 	token <- "XXXXXXAAAAAAA1111"
#' 	me <- FacebookUsersCollection("me", token=token)
#' }
#'
#' @importFrom httr oauth_endpoints oauth_app oauth2.0_token GET
#' @importFrom rjson fromJSON
#' @importFrom futile.logger flog.info
fbOAuth <- function(app_id, app_secret, permissions="public_profile,user_friends", cache=TRUE)
{
  
  facebook <- oauth_endpoints("facebook")
  
  myapp <- oauth_app("facebook", app_id, app_secret)
  
  #Sys.setenv("HTTR_SERVER_PORT" = "1410/")
  fb_oauth <- oauth2.0_token(facebook, myapp,
                             scope = permissions, type = "application/x-www-form-urlencoded", cache=cache)
  
  first.try  <- GET("https://graph.facebook.com/me", config(token=fb_oauth))
  
  if (first.try$status==200){
    flog.info("Authentication successful. You can query the FB Graph API using the OAuth token provided.")
    class(fb_oauth)[4] <- first.try$headers$`facebook-api-version`
    class(fb_oauth)[5] <- "StandardToken"
  } else {
    second.try <- GET(paste0("https://graph.facebook.com/me?access_token=", (fromJSON(names(fb_oauth$credentials)))$access_token))
    if (second.try$status==200){
      flog.info("Authentication successful. Queries will be performed using the access_token included into the OAuth Token due to recent changes in FB policy.")
      class(fb_oauth)[4] <- second.try$headers$`facebook-api-version`
      class(fb_oauth)[5] <- "NonStandardToken"
      if(is.null(getOption("facebook.api"))){
        flog.info(paste("Since you have not specified the Facebook API version to use, queries will be sent using the latest Facebook default (", class(fb_oauth)[4], ")", sep="")) 
        options("facebook.api" = class(fb_oauth)[4])
      }
    } else {
      stop(paste0(second.try$headers$`WWW-Authenticate`, collapse = " - "))
    }
  }
  
  return(fb_oauth)
}
