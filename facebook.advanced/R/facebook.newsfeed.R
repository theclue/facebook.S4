#' @rdname facebook.newsfeed
#' @export
#'
#' @title 
#' Download recent posts from the authenticated user's newsfeed
#'
#' @description
#' \code{faceook.newsfeed} returns a data frame with the current user' newsfeed.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{fbOAuth}}, \code{\link{getPosts}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n numeric, maximum number of comments and likes to return.
#' 
#' @param fields vector or comma-delimited string with the post-level details to get.
#' \code{n}.
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints
#' nothing to the console.
#' 
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Capture 100 most recent posts on my newsfeed and max 100 likes/comments for each one
#'	load("fb_oauth")
#'	my.newsfeed <- facebook.newsfeed(token=fb_oauth, n=200)
#' }
#'
facebook.newsfeed <- function(token, 
                        n=500, 
                        fields = "id,from.fields(id,name),message,created_time,type,link,name", 
                        .progress = create_progress_bar()
                        ){
  
  details.pagination.define <- 20
  
  parsed <- parse.input.fields(fields)
  
  # Init the progress bar
  .progress$init(floor(n/details.pagination.define)+1)
  .progress$step()
  
  query <- URLencode(
    paste0(
      "?ids=me&fields=home",
      ".limit(", details.pagination.define, ")",
      ".fields(", parsed$url,
      ",comments.summary(true).limit(0)",
      ",likes.summary(true).limit(0)",
      ")"
    )
  )
  
  content <- facebook.query(query = query, token = token)
  
  all.Posts <- do.call(rbind.fill,
                       lapply(content, function(sublist) {
                         page <- 0
                         p <- data.frame()
                         total.posts <- 0
                     
                         repeat {
                           postdata <- NULL
                           
                           .progress$step()
                           
                           if(page == 0){
                             postdata <- sublist$home 
                           } else {
                             postdata <- facebook.query(query = next.url, token = token, endpoint = NULL)
                           }
                           next.url <- postdata$paging$`next`
                           
                           p.page <- data.frame(detailsDataToDF(postdata$data, fields = parsed$fields),
                                                summaryDataToDF(postdata$data, fields = "likes.count,comments.count,shares.count"))
                           
                           if(!is.null(p.page) && nrow(p.page) > 0) {
                             
                             p.page$user.id <- sublist$id
                             p <- rbind.fill(p, p.page)
                             page <- page + 1
                             total.posts <- total.posts + nrow(p.page)
                             
                           }

                           if(total.posts >= n |
                                is.null(next.url))
                           {
                             return(head(p, n))
                           }
                           
                         }
                       }
                       )
  )
  .progress$term()
  return(all.Posts)
}
