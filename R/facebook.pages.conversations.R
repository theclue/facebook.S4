#' @rdname facebook.posts
#' @export
#'
#' @title 
#' Extract information about a set of public Facebook posts
#'
#' @description
#' \code{facebook.posts} retrieves information about a set of public Facebook posts.
#'
#' @details
#' \code{facebook.posts} returns data about posts but doesn't return lists of comments or likes (altough it will return a summary view of both).
#' You can actually get some informations about comments and likes using fields nesting (see below), but this is not actually
#' recommended.
#' 
#' Consider using the twin functions \code{\link{facebook.comments}}, \code{\link{facebook.likes}} to focuse of these nodes
#' of data.
#' 
#' Due to the network-graph nature of Facebook data model,
#' you can always specify fields details for each field eventually nesting \code{.fields()} clauses.
#' 
#' For example, if you need only \code{id} and \code{name} for the \code{from} node, this clause is valid among others:
#' \code{from.fields(id,name)}.
#'
#' @author
#' Gabriele Baldassarre
#' @seealso \code{\link{facebook.pages}}, \code{\link{facebook.comments}}, \code{\link{facebook.likes}}, \code{\link{fbOAuth}}
#'
#' @param posts A vector or a comma-delimited string of post IDs
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param fields vector or comma-delimited string with the post-level details to get.
#' \code{n}.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'	load("fb_oauth")
#'	fb.page <- getPages("9thcirclegames", token=fb_oauth)
#' ## Getting information about 10 most recent posts
#'	post.details <- facebook.posts(post=fb.page$posts[10,]$id, token=fb_oauth)
#' }
#'
facebook.posts <- function(posts, 
                           token, 
                           fields = "id,from.fields(id,name),message,created_time,type,link,name",
                           .progress = create_progress_bar()){
  
  posts.pagination.define <- 10
  
  parsed <- parse.input.fields(fields)
  
  posts.v <- unique(unlist(strsplit(posts, split = ",")))
  posts.f <- rep(seq_len(ceiling(length(posts.v) / posts.pagination.define)),each = posts.pagination.define,length.out = length(posts.v))
  posts.chunks <- split(posts.v, f = posts.f)
  
  if(length(posts.chunks) > 1){
    
    # Init the progress bar
    .progress$init(length(posts.chunks)+1)
    .progress$step()
    
    do.call(rbind,
            lapply(posts.chunks, function(single.chunk) {
              facebook.posts(posts = single.chunk, token = token, fields = fields, .progress = .progress)
            })
    )
  }
  
  else {
    
    query <- URLencode(
      paste0(
        "?ids=",
        paste0(posts.v, collapse = ","),
        "&fields=", parsed$url,
        ",comments.summary(true).limit(0)",
        ",likes.summary(true).limit(0)"
      )
    )
    
    content <- facebook.query(query = query, token = token)
    
    all.Posts <- data.frame(detailsDataToDF(content, parsed$fields),
                            summaryDataToDF(content, fields = "likes.count,comments.count,shares.count"))
    
    # Advance the progress bar
    if(inherits(try(.progress$step(), silent=T), "try-error")){
      .progress$init(length(posts.chunks)+1)
      .progress$step()
    }
    
    return(all.Posts)
  }
}
