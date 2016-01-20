#' @rdname facebook.current.accounts
#' @export
#'
#' @title 
#' Get the pages the current user currently manages.
#'
#' @description
#' \code{facebook.current.accounts} pulls the list of pages the current user actually manages and put into a
#' \code{\link{FacebookPagesCollection-class}} instance.
#' 
#' @details
#' This function requires the use of a OAuth token with \code{pages_show_list}
#' permission granted. 
#' 
#' Only the \code{id} and the \code{type} is returned in a mixed collection.
#' In normal circumnstances, \code{type} is always equal to \code{page}.
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{fbOAuth}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#' @param n An integer value with the maximum number of participants to be pulled for each conversation in \code{id}. It can be set to \code{Inf} 
#' to pull out any participant of a given conversation and assumes the default value to \code{facebook.maxitems} global option if missing. 
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}.
#' @return A collection of mixed accounts in a \code{\link{FacebookMixedCollection-class}} object with the \code{id} and the \code{type} for
#' each element included.
#' 
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#'	admin.pages <- facebook.current.accounts(fb_oauth)
#'	
#'	extended.pages <- admin.pages %>% FacebookPagesCollection()
#'	
#' }
#'
facebook.current.accounts <- function(token,
                                      parameters = list(),
                                      n = getOption("facebook.maxitems")){
  
  real.n <- (function(n, p.limit){
    if(n > p.limit) {
      return(p.limit)
    }
    else {
      return(n)
    }
  })(n, getOption("facebook.pagination"))
  
  query <- paste0(
    "https://graph.facebook.com/v", getOption("facebook.api"), "/me/accounts?fields=id")
  
  if(getOption("facebook.verbose")) message("Query URL: ", query)
  
  # Pagination using a lambda function
  accounts.idx  <- (function() {
    page <- 0
    p <- character()
    total.pages <- 0
    
    repeat { 
      pagedata <- NULL
      if(page == 0){
        pagedata <- callAPI(url = query , token=token)
      } else {
        pagedata <- callAPI(url = next.url, token=token)
      }
      next.url <- pagedata$paging$`next`
      p.id <- detailsDataToDF(pagedata$data, fields = "id")$id
      
      if(!is.null(p.id) & length(p.id) > 0) {
        p <- c(p, p.id)
        page <- page + 1
        total.pages <- total.pages + length(p.id)
      }
      
      # exit conditions
      if(total.pages >= n | is.null(next.url))
      {
        return(head(p, n))
      }
      
    }
  })()
  
  the.accounts <- new("FacebookMixedCollection",
                   id = unique(accounts.idx),
                   token = token,
                   fields="id",
                   parameters = parameters,
                   metadata = TRUE,
                   .progress = .progress)

  return(the.accounts)
}
