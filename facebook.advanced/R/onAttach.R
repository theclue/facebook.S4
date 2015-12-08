.onAttach <- function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    packageStartupMessage("\nThe Facebook Graph API Wrapper Package has been loaded\nTo connect to Facebook API for the first time, you need to login to Facebook and register a new application on https://developers.facebook.com/apps\nThen, use the provided App ID and App Secret in fbOAuth() function.\n\nThe first time you connect to an application, R will try to open a browser window to sign the token.\nThen, the token is cached in your home directory for successive uses.\n\nIf you need further assistance, follow the tutorials on https://gabrielebaldassarre.com/r/facebook")
    
  }
}
