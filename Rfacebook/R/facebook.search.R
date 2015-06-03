#' @rdname facebook.search
#' @export
#'
#' @title 
#' Perform a search on Facebook
#'
#' @description
#' \code{facebook.search} retrieves public content that mention a given keyword(s) and for a given type.
#' Please note that each type has its unique set of fields,
#' as referenced on official documentation \url{https://developers.facebook.com/docs/graph-api/using-graph-api/v2.3#search}
#' 
#' If \code{fields} is not specified or null, the function return a data frame containing \code{id} and \code{name} for each result
#' plus the given \code{type}.
#' 
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{fbOAuth}}, \code{\link{query}}
#'
#' @param query comma-delimited string or string vector containing keywords to search.
#' Note that the returned results will contain any of the keywords. 
#' 
#' @param type string containing the type of content to get.
#' Valid content types are \code{user, page, event, group, place, placetopic} 
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Maximum number of elements to return.
#' 
#' @param fields comma-delimited string or string vector with the details to get.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Searching 100 public pages that mention "facebook"
#'  load("fb_oauth")
#'	pages <- facebook.search(query="cats", type="page", token=fb_oauth, n=100)
#' }
#'

facebook.search <- function(query, token, n=200, type="page", fields="id,name"){
  
  fields <- ifelse(is.null(fields), "id,name", fields)
  
  limit <- ifelse(is.null(n), 200, n)
  
  pages.fields.url <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )
  
  pages.fields <- paste0(unique(
    unlist(strsplit(gsub('\\.fields\\((.*?)\\)','', 
                         gsub('\\.type\\((.*?)\\)','', fields, perl = TRUE)
                         , perl = TRUE), split = ","))),
    collapse = ","
  )
  
  url <- URLencode(paste0(
    "https://graph.facebook.com/v2.3/search?q=",
    paste0(query, collapse = " "),
    "&type=",
    type,
    "&limit=",
    limit,
    "&fields=", pages.fields.url
  ))
  
  # Pagination using a lambda function
  all.Pages  <- (function() {
    page <- 0
    p <- data.frame()
    total.pages <- 0
    
    repeat {
      pagedata <- NULL
      if(page == 0){
        pagedata <- callAPI(url=url , token=token)
      } else {
        pagedata <- callAPI(url=next.url, token=token)
      }
      next.url <- pagedata$paging$`next`
      p.page <- detailsDataToDF(pagedata$data, fields = pages.fields)
      
      if(!is.null(p.page) && nrow(p.page) > 0) {
        p <- rbind.fill(p, p.page)
        page <- page + 1
        total.pages <- total.pages + nrow(p.page)
      }
      
      # exit conditions
      if(total.pages >= n |
           is.null(next.url)
      )
      {
        cat(paste0("Found ", min(n, total.pages), " ", paste0(type, ifelse(total.pages == 1, "", "s")), " for '", paste(query, collapse = " "), "'.\n"))
        p$type = type
        return(head(p, n))
      }
      
      # Graceful waiting before next call
      Sys.sleep(0.5)
      
    }
  })()
}