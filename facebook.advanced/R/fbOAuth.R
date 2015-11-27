#' Auth
#'
#' @export
fbOAuth <- function(app_id, app_secret, permissions="public_profile,user_friends", cache=TRUE)
{
  
  facebook <- oauth_endpoints("facebook")
  	
  myapp <- oauth_app("facebook", app_id, app_secret)
  
  Sys.setenv("HTTR_SERVER_PORT" = "1410/")
    fb_oauth <- oauth2.0_token(facebook, myapp,
                               scope = permissions, type = "application/x-www-form-urlencoded", cache=cache)		
    if (GET("https://graph.facebook.com/me", config(token=fb_oauth))$status==200){
      message("Authentication successful.")
    }	
    
  return(fb_oauth)
}
