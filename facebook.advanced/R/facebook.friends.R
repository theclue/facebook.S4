#' @rdname facebook.friends
#' @export
#'
#' @title 
#' Extract list of friends with their information
#'
#' @description
#' \code{facebook.friends} retrieves information about the user's friends.
#'
#' @details
#' 
#' This function requires the use of a OAuth token with extended 
#' permissions. After the introduction of version 2.0 of the Graph API,
#' only friends who are using the application that you used to generate the 
#' token to query the API will be returned.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{getUsers}}, \code{\link{fbOAuth}}
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param fields vector or comma-separated string of fields to get
#'
#' @examples \dontrun{
#' ## Copy and paste token created at FB Graph API Explorer
#'  token <- "XXXXXX"
#'	my.friends <- facebook.friends(token=token)
#' ## Since users are ordered by ID, this will return 10 oldest user accounts
#'	head(my.friends, n=10)
#' }
#'

facebook.friends <- function(token, users="me", fields = "id,name"){
  
  details.pagination.define <- 500
  friends.pagination.define <- 10  
  
  if (getTokenVersion(token)=="v1"){
    stop("Calls using v1 token is no longer available.")
  }
  
  friends.fields <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )
  
  friends.v <- unique(unlist(strsplit(users, split = ",")))
  friends.f <- rep(seq_len(ceiling(length(friends.v) / friends.pagination.define)),each = friends.pagination.define,length.out = length(friends.v))
  friends.chunks <- split(friends.v, f = friends.f)
  
  if(length(friends.chunks) > 1){
    
    do.call(rbind,
            lapply(friends.chunks, function(single.chunk) {
              facebook.friends(token = token, users = single.chunk, fields = fields)
              
            })
    )
    
  }
  
  else {
    
    url <- paste0(
      "https://graph.facebook.com/v2.3/?ids=",
      paste0(friends.v, collapse = ","),
      "&fields=id,name,friends.summary(false){", friends.fields, "}"
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
    
    # Friends
    all.Friends <- do.call(rbind.fill,
                           lapply(content, function(sublist) {
                             page <- 0
                             f <- data.frame()
                             total.friends <- 0
                             
                             # TODO: make a better log
                             cat(paste("\nGetting ", sublist$name, "'s friends using this app", sep = ""))
                             
                             repeat {
                               friendsdata <- NULL
                               if(page == 0){
                                 friendsdata <- sublist$friends
                               } else {
                                 friendsdata <- callAPI(url=next.url, token=token)
                               }
                               next.url <- friendsdata$paging$`next`
                               
                               f.page <- detailsDataToDF(friendsdata$data, fields = friends.fields)
                               if(!is.null(f.page) && nrow(f.page) > 0) {
                                 # TODO: use detailsDataToToDF here instead
                                 f.page$parent.id <- sublist$id
                                 f.page$parent.name <- sublist$name
                                 f <- rbind.fill(f, f.page)
                                 page <- page + 1
                                 total.friends <- total.friends + nrow(f.page)
                                 
                               }
                               
                               if(is.null(next.url)){
                                 cat("...Done!\n")
                                 return(f)
                               }
                               
                               # Graceful waiting before next call
                               Sys.sleep(0.5)
                               
                             }
                           }
                           )
    )
    
    
    return(all.Friends)
  }
  
}
