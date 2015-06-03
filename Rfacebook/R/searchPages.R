#' @rdname searchPages
#' @export
#'
#' @title 
#' Search pages that mention a string
#'
#' @description
#' \code{searchPages} retrieves public pages that mention a given keyword
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}, Joel Gombin \email{joel.gombin@@gmail.com}, Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{fbOAuth}}, \code{\link{searchFacebook}}
#'
#' @param query string or string vector containing keywords to search.
#' Note that the returned results will contain any of the keywords. 
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Maximum number of pages to return.
#' 
#' @param fields vector or comma-delimited string with the post-level details to get.
#' \code{n}.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Searching 100 public pages that mention "facebook"
#'  load("fb_oauth")
#'	pages <- searchPages(query="facebook", token=fb_oauth, n=100 )
#' }
#'

searchPages <- function(query, token, n=200, fields="id,username,name,about,category,description,likes,link,talking_about_count"){
  
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
    "&type=page&limit=",
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
                               cat(paste0("Found ", min(n, total.pages), " ", ifelse(total.pages == 1, "page", "pages"), " for '", paste(query, collapse = " "), "'.\n"))
                               return(head(p, n))
                             }
                             
                             # Graceful waiting before next call
                             Sys.sleep(0.5)
                             
                           }
                         })()
  }