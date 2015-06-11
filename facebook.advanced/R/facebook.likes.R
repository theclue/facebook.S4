#' @rdname facebook.likes
#' @export
#'
#' @title 
#' Extract likes about a set of public Facebook posts
#'
#' @description
#' \code{facebook.likes} gathers postlikes data from a set of public Facebook posts.
#'
#' @details 
#' Due to the network-graph nature of Facebook data model,
#' you can always specify fields details for each field eventually nesting \code{.fields()} clauses.
#' 
#' For example, if you need only \code{id} and \code{name} for the \code{from} node, this clause is valid among others:
#' \code{from.fields(id,name)}.
#'
#' @author
#' Gabriele Baldassarre
#' @seealso \code{\link{facebook.comments}}, \code{\link{facebook.posts}}, \code{\link{fbOAuth}}
#'
#' @param posts string vector or a comma-delimited string of post IDs
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n numeric, maximum number of likes to return for each post.
#' 
#' @param fields vector or comma-delimited string with the post-level details to get.
#' Subfields nesting is supported.
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints
#' nothing to the console.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'	load("fb_oauth")
#'	fb.page <- facebook.pages("9thcirclegames", token=fb_oauth)
#' ## Getting at most 2000 likesfor the ten most recent posts
#'	recent.posts.likes <- facebook.likes(post=fb.page$posts[10,]$id, n=2000, token=fb_oauth)
#' }
#'
facebook.likes <- function(posts, 
                           token, 
                           n=500, 
                           fields = "id,name,profile_type",
                           .progress = create_progress_bar()){
  
  details.pagination.define <- 500
  posts.pagination.define <- 10
  
  parsed <- parse.input.fields(fields)
  
  posts.v <- unique(unlist(strsplit(posts, split = ",")))
  posts.f <- rep(seq_len(ceiling(length(posts.v) / posts.pagination.define)),each = posts.pagination.define,length.out = length(posts.v))
  posts.chunks <- split(posts.v, f = posts.f)
  
  if(length(posts.chunks) > 1){
    
    # Init the progress bar
    .progress$init(length(posts)+1)
    .progress$step()
    
    do.call(rbind,
            lapply(posts.chunks, function(single.chunk) {
              facebook.likes(posts = single.chunk, token = token, n = n, fields = fields, .progress = .progress)
            })
    )
    
  }
  
  else {
    
    query <- URLencode(
      paste0(
        "?ids=",
        paste0(posts.v, collapse = ","),
        "&fields=id",
        ",likes.summary(true)",
        ifelse(n > 0, paste0(".fields(", parsed$url, ").limit(", ifelse(n > details.pagination.define, details.pagination.define, n), ")"), ".limit(0)"),
        ",comments.summary(false)")
    )
    
    content <- facebook.query(query = query, token = token)
    
    all.Posts <- detailsDataToDF(content, fields = "id")
    
    all.Likes <- do.call(rbind.fill,
                            lapply(content, function(sublist) {
                              page <- 0
                              c <- data.frame()
                              total.likes <- 0
                              
                              # Advance the progress bar
                              if(inherits(try(.progress$step(), silent=T), "try-error")){
                                .progress$init(length(posts)+1)
                                .progress$step()
                              }
                              
                              repeat {
                                likedata <- NULL
                                if(page == 0){
                                  likedata <- sublist$likes
                                } else {
                                  likedata <- facebook.query(query = next.url, token = token, endpoint = NULL)
                                }
                                next.url <- likedata$paging$`next`
                                
                                c.page <- detailsDataToDF(likedata$data, fields = parsed$fields)
                                if(!is.null(c.page) && nrow(c.page) > 0) {
                                  c.page$post.id <- sublist$id
                                  c <- rbind.fill(c, c.page)
                                  page <- page + 1
                                  total.likes <- total.likes + nrow(c.page)
                                }
                                
                                if(total.likes >= n | is.null(next.url)){
                                  return(head(c, n))
                                }
                                
                              }
                            }
                            )
    )
    return(all.Likes)
  }
}
