#' @rdname facebook.search
#' @export
#'
#' @title 
#' Perform a search on Facebook
#'
#' @description
#' \code{facebook.search} retrieves public content that mention a given keyword(s) and for a given type.
#' Please note that each type has its unique set of fields,
#' as referenced on official documentation \url{https://developers.facebook.com/docs/graph-api/using-graph-api/v2.5#search}
#' 
#' Since this is a finder function, duplicated \code{id} won't be removed to the
#' output collection unless they also have the same \code{parent}.
#' 
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{fbOAuth}}
#'
#' @param query A character vector vector containing keywords to search in OR condition.
#' 
#' @param type A character vector containing the types of content to get.
#' Valid content types are \code{user, page, event, group, place} 
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}.
#' 
#' @param n Maximum number of elements to return for each type.
#'
#' @return A \code{\link{FacebookMixedCollection-class}} collection with the \code{id} and the \code{type} for
#' each element included.
#' 
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Searching 100 public pages that mention cats
#'  load("fb_oauth")
#'	pages <- facebook.search(query="cats", type="page", token=fb_oauth, n=100)
#' }
#'
#' @importFrom futile.logger flog.trace
facebook.search <- function(query,
                            token,
                            parameters = list(),
                            n = getOption("facebook.maxitems"),
                            type = c("page", "group")){

  query.parameters <- sub("&$", "",
                          sub('([[:punct:]])\\1+', '\\1',
                              do.call(paste, list(
                                lapply(seq_along(parameters), function(y, n, i) {
                                  if(is.null((y[[i]]))) return("")
                                  paste(n[[i]], y[[i]], sep="=")
                                }
                                , y=parameters, n=names(parameters)),
                                collapse = "&"))
                          )
  )
all.results <- do.call(c,lapply(type, function(t) { 
  query <- paste0(
    "https://graph.facebook.com/", getOption("facebook.api"), "/search",
    "?q=", paste0(query, collapse = " "),
    ifelse(length(parameters), paste0("&", query.parameters), ""),
    "&type=", t,
    "&limit=", n,
    "&fields=id")
  
  flog.trace("FB GraphAPI GET URL: %s", query)
  
  # Pagination using a lambda function
  all.Pages  <- (function() {
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
  
  the.collection <- new("FacebookMixedCollection", fields="id")
  the.collection@id <- all.Pages
  the.collection@type <- rep(t, length(all.Pages))
  the.collection@token <- token
  the.collection@fields <- "id"
  the.collection@data <- do.call(c, lapply(all.Pages, function(s){
    ss <- list()
    ss[[s]] <- list(id=s)
    return(ss)
  }))
  return(the.collection)
})
)
}
