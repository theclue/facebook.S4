#' @rdname query
#' @export
#'
#' @title 
#' Pproxt function to perform a generic query against FB Graph API
#'
#' @description
#' \code{query} performs a generic query against Facebook Graph API and returns a list reflecting the outputted json tree.
#'
#' @details
#' Write the query as you would do using the Graph Explroer at \url{https://developers.facebook.com/tools/explorer}
#' but don't include the API version number and the trailing slash.
#' Please note that this function has no automatic paging support.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{fbOAuth}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @examples \dontrun{
#' ## Copy and paste token created at FB Graph API Explorer
#'  token <- "XXXXXX"
#'  
#'	results.json <- query(query="me/posts/?limit=10", token=token)
#' }
#'
query <- function(query, token){
  
    url <- paste0(
      "https://graph.facebook.com/v2.3/",
      query,
      ifelse(grepl("\\?",query), "", "?")
    )
    
    content <- callAPI(url=url, token=token)
    
    # Check for permission
    if (length(content)==0){ 
      stop("You're not authorized to get this information, Please check your permissions before retrying.")
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
