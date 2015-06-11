#' @rdname facebook.query
#' @export
#'
#' @title 
#' Perform a generic query
#'
#' @description
#' \code{query} performs a generic query on Facebook and returns a list reflecting the raw json tree
#' given by the API.
#'
#' @details
#' Write the query as you would do using the Graph Explroer at \url{https://developers.facebook.com/tools/explorer}
#' without including the API version number and the trailing slashes.
#' 
#' Please note that this function has no automatic paging support.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{fbOAuth}} \code{\link{facebook.search}}
#'
#' @param query string containing the URL fragment for the FB Graph API
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#' 
#' @param endpoint string with the endpoint to the FB Graph. You can optionally end the URI with the API version, here
#' For example \code{https://graph.facebook.com/v2.3}
#'
#' @examples \dontrun{
#' ## Copy and paste token created at FB Graph API Explorer
#'  token <- "XXXXXX"
#'	results.json <- facebook.query(query="me/posts/?limit=10", token=token)
#' }
#'
facebook.query <- function(query, token, endpoint = "https://graph.facebook.com/v2.3"){
  
  url <- 
    paste0(
      ifelse(!is.null(endpoint) & length(endpoint)>0, ifelse((!grepl('/$', endpoint) & !grepl('^/', query)), paste0(endpoint,"/"), endpoint), ""),
      query,
      ifelse(grepl("\\?",query), "", "?")
    )
  
  content <- callAPI(url=url, token=token)
  
  # Check for permission
  if (length(content)==0){ 
    stop("You're not authorized to perform this query. Please check your permissions before retrying.")
  }  
  # error traps: retry 3 times if error
  error <- 0
  
  while (length(content$error_code)>0){
    Sys.sleep(1)
    error <- error + 1
    content <- callAPI(url=url, token=token)  	
    if (error==3){ stop(content$error_msg) }
  }
  
  return(content)
}
