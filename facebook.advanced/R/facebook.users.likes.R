#' @rdname facebook.users.likes
#' @export
#'
#' @title 
#' Extract list of likes from Facebook users
#'
#' @description
#' \code{facebook.users.likes} retrieves information about the likes from a list of Facebook IDs and/or names.
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
#' @param users A vector or comma-delimited string with IDs or screen names.
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param n Maximum number of likes to return for each user.
#' 
#' @param fields vector or comma-separated string of fields to get
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints
#' nothing to the console.
#'
#' @examples \dontrun{
#'  load("fb_oauth")
#'  me.likes <- facebook.users.likes(users="me", token=fb_oauth)
#' }
#'

facebook.users.likes <- function(users, 
                                 token, 
                                 n=500, 
                                 fields="category,name,id,created_time",
                                 .progress = create_progress_bar()){
  
  
  details.pagination.define <- 500
  likes.pagination.define <- 10
  
  parsed.user.likes <- parse.input.fields(fields)
  
  
  likes.v <- unique(unlist(strsplit(users, split = ",")))
  likes.f <- rep(seq_len(ceiling(length(likes.v) / likes.pagination.define)),each = likes.pagination.define,length.out = length(likes.v))
  likes.chunks <- split(likes.v, f = likes.f)
  
  if(length(likes.chunks) > 1){
    
    # Init the progress bar
    .progress$init(length(users)+1)
    .progress$step()
    
    # Recursive calls for each chunk
    do.call(rbind,
            lapply(likes.chunks, function(single.chunk) {
              facebook.users.likes(users = single.chunk, token = token, n = n, fields = fields, .progress = .progress)         
            })
    )   
  }
  
  else {
    
    query <- paste0(
      "?ids=",
      paste0(likes.v, collapse = ","),
      "&fields=id,name,likes{", likes.fields, "}"
    )
    
    content <- facebook.query(query=query, token=token)
    
    # Likes
    all.Likes <- data.frame()
    if (n > 0) {
      
      all.Likes <- do.call(rbind.fill,
                           lapply(content, function(sublist) {
                             page <- 0
                             l <- data.frame()
                             total.likes <- 0
                             
                             # Advance the progress bar
                             if(inherits(try(.progress$step(), silent=T), "try-error")){
                               .progress$init(length(users)+1)
                               .progress$step()
                             }
                             
                             repeat {
                               likesdata <- NULL
                               if(page == 0){
                                 likesdata <- sublist$likes 
                               } else {
                                 likesdata <- facebook.query(query=next.url, token=token, endpoint=NULL)
                               }
                               next.url <- likesdata$paging$`next`
                               
                               l.page <- detailsDataToDF(likesdata$data, fields = likes.fields)
                               
                               if(!is.null(l.page) && nrow(l.page) > 0) {
                                 
                                 l.page$parent.id <- sublist$id
                                 l.page$parent.name <- sublist$name
                                 l <- rbind.fill(l, l.page)
                                 page <- page + 1
                                 total.likes <- total.likes + nrow(l.page)
                                 
                               }
                               
                               if(total.likes >= n |
                                    is.null(next.url)
                               )
                               {
                                 return(head(l, n))
                               }
                               
                             }
                           }
                           )
      )
      
    }
    return(all.Likes)
  }
}
