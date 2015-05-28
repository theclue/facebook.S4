#' @rdname getNewsfeed
#' @export
#'
#' @title 
#' Download recent posts from the authenticated user's newsfeed
#'
#' @description
#' \code{getNewsfeed} returns a data frame with the current user' newsfeed.
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
#' 
#' @param fields vector or comma-delimited string with the post-level details to get.
#' \code{n}.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Capture 100 most recent posts on my newsfeed and max 100 likes/comments for each one
#'	load("fb_oauth")
#'	my_newsfeed <- getNewsfeed(token=fb_oauth, n=100)
#' }
#'
getNewsfeed <- function(token, n=500, fields = "id,from.fields(id,name),message,created_time,type,link,name"){
  
  details.pagination.define <- 500
  
  post.fields.url <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )
  
  post.fields <- paste0(unique(
    unlist(strsplit(gsub('\\.fields\\((.*?)\\)','',fields, perl = TRUE), split = ","))),
    collapse = ","
  )

  url <- paste0(
      "https://graph.facebook.com/v2.3/?ids=me&fields=home{", post.fields.url,
      ",comments.summary(true).limit(0)",
      ",likes.summary(true).limit(0)",
      "}"
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
    
    
  all.Posts <- do.call(rbind.fill,
                       lapply(content, function(sublist) {
                         page <- 0
                         p <- data.frame()
                         total.posts <- 0
                         
                         # TODO: make a better log
                         cat(paste("\nGetting newsfeed", sublist$name, sep = ""))
                         
                         repeat {
                           postdata <- NULL
                           if(page == 0){
                               postdata <- sublist$home 
                           } else {
                             postdata <- callAPI(url=next.url, token=token)
                           }
                           next.url <- postdata$paging$`next`
                           
                           p.page <- data.frame(detailsDataToDF(postdata$data, fields = post.fields),
                                                summaryDataToDF(postdata$data, fields = "likes.count,comments.count,shares.count"))
                           
                           if(!is.null(p.page) && nrow(p.page) > 0) {
                             
                             p.page$user.id <- sublist$id
                             p <- rbind.fill(p, p.page)
                             page <- page + 1
                             total.posts <- total.posts + nrow(p.page)
                             
                             cat(paste("...", total.posts, sep = ""))
                             
                           }
                           
                           # unregarding of since() query parameter, FB Graph somethimes
                           # brings back posts older than 'since', so here
                           # I'm also making sure the function stops when that happens
                           if(total.posts >= n |
                                is.null(next.url))
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
return(all.Posts)
}
# getNewsfeed <- function(token, n=200)
# {
# 	
# 
# 	url <- "https://graph.facebook.com/me/home?limit="
# 	if (n<=200){
# 		url <- paste(url, n, sep="")
# 	}
# 	if (n>200){
# 		url <- paste(url, "200", sep="")
# 	}
# 	url <- paste(url, 
# 		"&fields=from.fields(name,id),to,message,created_time,type,link,likes.summary(true).limit(1),comments.summary(true).limit(1),shares",
# 		sep="")
# 
# 	## making query
# 	content <- callAPI(url=url, token=token)
# 	l <- length(content$data); cat(l, "posts ")
# 	
# 	## retrying 3 times if error was found
# 	error <- 0
# 	while (length(content$error_code)>0){
# 		cat("Error!\n")
# 		Sys.sleep(0.5)
# 		error <- error + 1
# 		content <- callAPI(url=url, token=token)		
# 		if (error==3){ stop(content$error_msg) }
# 	}
# 	if (length(content$data)==0){ 
# 		stop("No more posts were found on News Feed.")
# 	}
# 	df <- newsDataToDF(content$data)
# 
# 	## paging if n>200
# 	if (n>200){
# 		df.list <- list(df)
# 		while (l<n & length(content$data)>0 &
# 			!is.null(content$paging$`next`)){
# 			url <- content$paging$`next`
# 			content <- callAPI(url=url, token=token)
# 			l <- l + length(content$data)
# 			if (length(content$data)>0){ cat(ifelse(l<n,l,n), " ") }
# 
# 			## retrying 3 times if error was found
# 			error <- 0
# 			while (length(content$error_code)>0){
# 				cat("Error!\n")
# 				Sys.sleep(0.5)
# 				error <- error + 1
# 				content <- callAPI(url=url, token=token)		
# 				if (error==3){ stop(content$error_msg) }
# 			}
# 
# 			df.list <- c(df.list, list(newsDataToDF(content$data)))
# 		}
# 		df <- do.call(rbind, df.list)
# 		if (nrow(df)>n){ df <- df[1:n,]}
# 	}
# 	return(df)
# }



