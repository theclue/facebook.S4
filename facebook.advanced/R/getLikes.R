#' @rdname getLikes
#' @export
#'
#' @title 
#' Extract list of likes from Facebook users
#'
#' @description
#' \code{getLikes} retrieves information about the likes from a list of Facebook IDs and/or names.
#' 
#' @details
#' 
#' This function requires the use of an OAuth token with the following
#' permissions: user_likes, friends_likes
#' 
#' This function requires the use of a OAuth token with user_likes 
#' permission granted. After the introduction of version 2.0 of the Graph API,
#' only likes from users who are using the application that you used to generate the 
#' token to query the API will be returned.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{facebook.friends}}, \code{\link{fbOAuth}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param user A vector or comma-delimited string with IDs or screen names.
#'
#' @param n Maximum number of likes to return for each user.
#'
#' @examples \dontrun{
#'  load("fb_oauth")
#'  me.likes <- getLikes(users="me", token=fb_oauth)
#' }
#'

getLikes <- function(users, n=500, token, fields="category,name,id,created_time"){
  
  
  details.pagination.define <- 500
  likes.pagination.define <- 10  
  
  
  likes.fields <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )
  
  likes.v <- unique(unlist(strsplit(users, split = ",")))
  likes.f <- rep(seq_len(ceiling(length(likes.v) / likes.pagination.define)),each = likes.pagination.define,length.out = length(likes.v))
  likes.chunks <- split(likes.v, f = likes.f)
  
  if(length(likes.chunks) > 1){
    
    do.call(rbind,
            lapply(likes.chunks, function(single.chunk) {
              getLikes(users = single.chunk, n = n,  token = token, fields = fields)
              
            })
    )
    
  }
  
  else {
    
    url <- paste0(
      "https://graph.facebook.com/v2.3/?ids=",
      paste0(likes.v, collapse = ","),
      "&fields=id,name,likes{", likes.fields, "}"
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
    
    # Likes
    all.Likes <- data.frame()
    if (n > 0) {
      
      all.Likes <- do.call(rbind.fill,
                           lapply(content, function(sublist) {
                             page <- 0
                             l <- data.frame()
                             total.likes <- 0
                             
                             # TODO: make a better log
                             cat(paste("\nGetting likes for user ", sublist$name, "...", sep = ""))
                             
                             repeat {
                               likesdata <- NULL
                               if(page == 0){
                                 likesdata <- sublist$likes 
                               } else {
                                 likesdata <- callAPI(url=next.url, token=token)
                               }
                               next.url <- likesdata$paging$`next`
                               
                               l.page <- detailsDataToDF(likesdata$data, fields = likes.fields)
                               
                               
                               if(!is.null(l.page) && nrow(l.page) > 0) {
                                 # TODO: use detailsDataToToDF here instead
                                 l.page$parent.id <- sublist$id
                                 l.page$parent.name <- sublist$name
                                 l <- rbind.fill(l, l.page)
                                 page <- page + 1
                                 total.likes <- total.likes + nrow(l.page)
                                 
                                 cat(paste("...", total.likes, sep = ""))
                                 
                               }
                               
                               if(total.likes >= n |
                                    is.null(next.url)
                               )
                               {
                                 cat("...Done!\n")
                                 return(head(l, n))
                               }
                               
                               # Graceful waiting before next call
                               Sys.sleep(0.5)
                               
                             }
                           }
                           )
      )
      
    }
    
    return(all.Likes)
  }
}

# query <- paste0('https://graph.facebook.com/', user, 
#                 '?fields=likes.limit(', n, ').fields(id,name,website)')
# content <- callAPI(query, token)
# df <- userLikesToDF(content$likes$data)
# next_url <- content$likes$paging$`next`
# while (!is.null(next_url)){
#   content <- callAPI(next_url, token)
#   df <- rbind(df, userLikesToDF(content$data))
#   next_url <- content$paging$`next`
# }
# return(df)


