#' @rdname getPosts
#' @export
#'
#' @title 
#' Extract information about a set of public Facebook posts
#'
#' @description
#' \code{getPosts} retrieves information about a set of public Facebook posts, including
#' list of comments and likes.
#'
#' @details 
#' \code{getPosts} returns a list with three data frames: \code{posts}, 
#' \code{likes}, and \code{comments}. First, \code{posts} contains information
#' about the posts: author, creation date, id, counts of likes, comments, and 
#' shares, etc. Second, \code{likes} is a data frame that contains names and
#' Facebook IDs of all the users that liked the post. Finally, \code{comments}
#' is a data frame with information about the comments to the posts (author, 
#' message, creation time, id).
#'
#' @author
#' Gabriele Baldassarre
#' @seealso \code{\link{getUsers}}, \code{\link{getPage}}, \code{\link{fbOAuth}}
#'
#' @param posts A vector or a comma-delimited string of post IDs
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n numeric, maximum number of comments and likes to return.
#'
#' @param n.likes numeric, maximum number of likes to return. Default is \code{n}.
#'
#' @param n.comments numeric, maximum number of likes to return. Default is 
#' \code{n}.
#' 
#' @param fields vector or comma-delimited string with the page-level metadata set to get.
#' \code{n}.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about Facebook's Facebook Page
#'	load("fb_oauth")
#'	fb_page <- getPage(page="facebook", token=fb_oauth)
#' ## Getting information and likes/comments about 10 most recent posts
#'	posts <- getPosts(post=fbpages[10,]$id, n=2000, token=fb_oauth)
#' }
#'

getPosts <- function(posts, token, n=500, n.likes=n, n.comments=n, fields = "id,from,message,created_time,type,link,name"){
  
  details.pagination.define <- 500
  posts.pagination.define <- 10
  
  post.fields <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )
  posts.v <- unique(unlist(strsplit(posts, split = ",")))
  posts.f <- rep(seq_len(ceiling(length(posts.v) / posts.pagination.define)),each = posts.pagination.define,length.out = length(posts.v))
  posts.chunks <- split(posts.v, f = posts.f)

  if(length(posts.chunks) > 1){
    
    do.call(rbind,
            lapply(posts.chunks, function(single.chunk) {
                getPosts(posts = single.chunk, token = token, n = n, n.likes = n.likes, n.comments = n, fields = fields)
      
    })
    )
    
  }
  
  else {
  
  url <- paste0(
    "https://graph.facebook.com/v2.3/?ids=",
    paste0(posts.v, collapse = ","),
    "&fields=", post.fields,
    ",comments.summary(true)",
    ifelse(n.comments > 0, paste0(".fields(id,from,message,created_time,like_count).limit(", ifelse(n.comments > details.pagination.define, details.pagination.define, n.comments), ")"), ".limit(0)"),
    ",likes.summary(true)",
    ifelse(n.likes > 0, paste0(".fields(id,name,profile_type).limit(", ifelse(n.likes > details.pagination.define, details.pagination.define, n.likes), ")"), ".limit(0)")
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
  

all.Posts <- data.frame(detailsDataToDF(content, fields = "id,from,message,created_time,type,link"),
           summaryDataToDF(content, fields = "likes.count,comments.count,shares.count"))

  
  # Likes
  if (n.likes > 0) {
    all.Likes <- do.call(rbind.fill,
                              lapply(content, function(sublist) {
                                page <- 0
                                l <- data.frame()
                                total.likes <- 0
                                
                                # TODO: make a better log
                                cat(paste("\nGetting likes for post ", sublist$id, sep = ""))
                                
                                repeat {
                                  likedata <- NULL
                                  if(page == 0){
                                    likedata <- sublist$likes
                                  } else {
                                    likedata <- callAPI(url=next.url, token=token)
                                  }
                                  next.url <- likedata$paging$`next`
                                  
                                  l.page <- detailsDataToDF(likedata$data, fields = "id,name")
                                  if(!is.null(l.page) && nrow(l.page) > 0) {
                                    l.page$post.id <- sublist$id
                                    l <- rbind.fill(l, l.page)
                                    page <- page + 1
                                    total.likes <- total.likes + nrow(l.page)
                                    
                                    cat(paste("...", total.likes, sep = ""))
                                    
                                  }
                                  
                                  if(total.likes >= n.likes | is.null(next.url)){
                                    cat("...Done!\n")
                                    return(head(l, n.likes))
                                  }
                                  
                                  # Graceful waiting before next call
                                  Sys.sleep(0.5)
                                  
                                }
                              }
                              )
    )
    
  }
  
  if (n.comments > 0) {
    all.Comments <- do.call(rbind.fill,
                                 lapply(content, function(sublist) {
                                   page <- 0
                                   c <- data.frame()
                                   total.comments <- 0
                                   
                                   # TODO: make a better log
                                   cat(paste("\nGetting comments for post ", sublist$id, sep = ""))
                                   
                                   repeat {
                                     commentdata <- NULL
                                     if(page == 0){
                                       commentdata <- sublist$comments
                                     } else {
                                       commentdata <- callAPI(url=next.url, token=token)
                                     }
                                     next.url <- commentdata$paging$`next`
                                     
                                     c.page <- detailsDataToDF(commentdata$data, fields = "id,from,message,created_time,like_count")
                                     if(!is.null(c.page) && nrow(c.page) > 0) {
                                       c.page$post.id <- sublist$id
                                       c <- rbind.fill(c, c.page)
                                       page <- page + 1
                                       total.comments <- total.comments + nrow(c.page)
                                       
                                       cat(paste("...", total.comments, sep = ""))
                                       
                                     }
                                     
                                     if(total.comments >= n.comments | is.null(next.url)){
                                       cat("...Done!\n")
                                       return(head(c, n.comments))
                                     }
                                     
                                     # Graceful waiting before next call
                                     Sys.sleep(0.5)
                                     
                                   }
                                 }
                                 )
    )
    
  }
  
  fb.c <- fb.Posts(all.Posts, all.Likes, all.Comments)
  #fb.c[["raw"]] <- content
  
  return(fb.c)
  }
}
