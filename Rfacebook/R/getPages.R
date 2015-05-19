#' @rdname getPages
#' @export
#'
#' @title 
#' Extract a list of pages and posts attributes list of posts from public Facebook pages
#'
#' @description
#' \code{getPages} returns a list of two data frames: \code{pages} contains all metadata
#' of the pages while \code{posts} has a summary of the posts of the pages.
#' You can then use \code{\link{getPosts}} to further dig into posts details.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{getUsers}}, \code{\link{getPosts}}, \code{\link{fbOAuth}}
#'
#' @param A vector or a comma-delimited string of page IDs or page name.
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Number of posts of page to return. Note that number can be sometimes
#' higher or lower, depending on status of API.
#'
#' @param since A UNIX timestamp or strtotime data value that points to
#' the start of the time range to be searched. For more information on the
#' accepted values, see: \url{http://php.net/manual/en/function.strtotime.php}
#'
#' @param until A UNIX timestamp or strtotime data value that points to
#' the end of the time range to be searched. For more information on the
#' accepted values, see: \url{http://php.net/manual/en/function.strtotime.php}
#' 
#' @param fields vector or a comma-delimited string with the page-level metadata set to get.
#'
#' @param feed If \code{TRUE}, the function will also return posts on the page
#' that were made by others (not only the admin of the page).
#'
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'	load("fb_oauth")
#'	fb_pages <- getPage(page="facebook", token=fb_oauth)
#' ## Getting posts on 9th Circle Games page, including posts by others users
#' ## (not only owner of page)
#'  page <- getPage(page="9thcirclegames", token=fb_oauth, feed=TRUE)
#' ## Getting posts on Humans of New York page in January 2013
#'  page <- getPage(page="humansofnewyork", token=fb_oauth, n=1000
#'		since='2013/01/01', until='2013/01/31')
#' }
#'


getPages <- function(pages, token, n=100, since=NULL, until=NULL, feed=FALSE, fields="id,username,name,about,category,description,likes,link,talking_about_count"){
  
  if(!is.null(since)) since <- ifelse("POSIXct" %in% class(since), since, as.numeric(formatFbDate(since)))
  if(!is.null(until)) until <- ifelse("POSIXct" %in% class(until), until, as.numeric(formatFbDate(until)))
  
  pages.pagination.define <- 25
  posts.pagination.define <- 50
  
  page.fields <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )
  pages.v <- unique(unlist(strsplit(pages, split = ",")))
  pages.f <- rep(seq_len(ceiling(length(pages.v) / pages.pagination.define)),each = pages.pagination.define,length.out = length(pages.v))
  pages.chunks <- split(pages.v, f = pages.f)
  
  if(length(pages.chunks) > 1){
    
    do.call(rbind,
            lapply(pages.chunks, function(single.chunk) {
              getPages(pages = single.chunk, token = token, n = n, since = since, until = until, feed=FALSE, fields = fields)
              
            })
    )
    
  }
  
  else {
    
    url <- paste0(
      "https://graph.facebook.com/v2.3/?ids=",
      paste0(pages.v, collapse = ","),
      "&fields=", page.fields,
      ",", (ifelse(feed == TRUE, "posts", "feed")),
      ".limit(", ifelse(n > posts.pagination.define, posts.pagination.define, n), ")",
      ifelse(!is.null(since), paste0(".since(", since, ")"), ""),
      ifelse(!is.null(until), paste0(".until(", until, ")"), ""),
      ifelse(n > 0, "{id,from{id,name},message,created_time,type,link,likes.limit(0).summary(true),comments.limit(0).summary(true)}", "")
    )
    
    content <- callAPI(url=url, token=token)
    
    # Check for permission
    if (length(content)==0) stop("You're not authorized to get this information, Please check your permissions before retrying.")
    
    error <- 0
    while (length(content$error_code)>0){
      cat("Error!\n")
      Sys.sleep(0.5)
      error <- error + 1
      content <- callAPI(url=url, token=token)		
      if (error==3){ stop(content$error_msg) }
    }
    
    all.Pages <- do.call(rbind.fill,
                         lapply(content, function(sublist) {
                           pageDataToDF(sublist, page.fields)
                         })
    )
    
    # Posts
    all.Posts <- data.frame()
    if (n > 0) {
      
      min.since <- ifelse(!is.null(since), as.Date(since, origin="1970-01-01"), as.Date('1970/01/01'))
      
      all.Posts <- do.call(rbind.fill,
                           lapply(content, function(sublist) {
                             page <- 0
                             p <- data.frame()
                             total.posts <- 0
                             
                             # TODO: make a better log
                             cat(paste("\nGetting posts for ", sublist$name, sep = ""))
                             
                             repeat {
                               postdata <- NULL
                               if(page == 0){
                                 postdata <- ifelse(feed == TRUE, sublist$feed, sublist$post)
                               } else {
                                 postdata <- callAPI(url=next.url, token=token)
                               }
                               next.url <- postdata$paging$`next`
                               
                               p.page <- do.call(rbind.fill,
                                                 lapply(postdata$data, function(sublist) {
                                                   postDataToDF(sublist, "id,from,message,created_time,type,link")
                                                 })
                               )
                               
                               
                               if(!is.null(p.page) && nrow(p.page) > 0) {
                                 p.page$page.id <- sublist$id
                                 p <- rbind.fill(p, p.page)
                                 page <- page + 1
                                 total.posts <- total.posts + nrow(p.page)
                                 
                                 cat(paste("...", total.posts, sep = ""))
                                 
                               }
                               
                               # unregarding of since() query parameter, FB Graph somethimes
                               # brings back posts older than 'since', so here
                               # I'm also making sure the function stops when that happens
                               if(total.posts >= n |
                                    is.null(next.url) |
                                    ifelse(!is.null(p.page), (min(formatFbDate(p.page$created_time, "date")) < min.since), FALSE)
                               )
                               {
                                 cat("...Done!\n")
                                 return(head(p, n))
                               }
                               
                               # Graceful waiting before next call
                               Sys.sleep(0.5)
                               
                             }
                           }
                           )
      )
      
    }
    
    return(fb.Pages(pages = all.Pages, posts = all.Posts))
  }
}