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
#' After the introduction of version 2.0 of the Graph API,
#' only friends who are using the application that you used to generate the 
#' token to query the API will be returned.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{facebook.users}}, \code{\link{fbOAuth}}
#'
#' @param users vector or comma-separated string of users to get friends
#'
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param fields vector or comma-separated string of fields to get
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints
#' nothing to the console.
#'
#' @examples \dontrun{
#' ## Copy and paste token created at FB Graph API Explorer
#'  token <- "XXXXXX"
#'	my.friends <- facebook.friends(token=token)
#' ## Since users are ordered by ID, this will return 10 oldest user accounts
#'	head(my.friends, n=10)
#' }
#'
facebook.friends <- function(users="me", token, fields = "id,name", .progress = create_progress_bar()){
  
  details.pagination.define <- 500
  friends.pagination.define <- 10  
  
  friends.fields <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )
  
  parsed <- parse.input.fields(fields)
  
  friends.v <- unique(unlist(strsplit(users, split = ",")))
  friends.f <- rep(seq_len(ceiling(length(friends.v) / friends.pagination.define)),each = friends.pagination.define,length.out = length(friends.v))
  friends.chunks <- split(friends.v, f = friends.f)
  
  if(length(friends.chunks) > 1){

    # Init the progress bar
    .progress$init(length(users)+1)
    .progress$step()
    
    # Recursive calls for each chunk
    do.call(rbind,
            lapply(friends.chunks, function(single.chunk) {
              facebook.friends(token = token, users = single.chunk, fields = fields, .progress = .progress)
              
            })
    )
    
  }
  
  else {
    
    query <- URLencode(
      paste0(
        "?ids=",
        paste0(friends.v, collapse = ","),
        "&fields=id,name,friends.summary(false){", parsed$url, "}"
      )
    )
    
    content <- facebook.query(query = query, token = token)
    
    # Friends
    all.Friends <- do.call(rbind.fill,
                           lapply(content, function(sublist) {
                             page <- 0
                             f <- data.frame()
                             total.friends <- 0
                             
                             # Advance the progress bar
                             if(inherits(try(.progress$step(), silent=T), "try-error")){
                               .progress$init(length(users)+1)
                               .progress$step()
                             }
                             
                             repeat {
                               friendsdata <- NULL
                               if(page == 0){
                                 friendsdata <- sublist$friends
                               } else {
                                 friendsdata <- facebook.query(query=next.url, token=token, endpoint=NULL)
                               }
                               next.url <- friendsdata$paging$`next`
                               
                               f.page <- detailsDataToDF(friendsdata$data, fields = parsed$fields)
                               if(!is.null(f.page) && nrow(f.page) > 0) {

                                 f.page$parent.id <- sublist$id
                                 f.page$parent.name <- sublist$name
                                 f <- rbind.fill(f, f.page)
                                 page <- page + 1
                                 total.friends <- total.friends + nrow(f.page)
                                 
                               }
                               
                               if(is.null(next.url)){
                                 return(f)
                               }
                             
                             }
                           }
                           )
    )
    return(all.Friends)
  }
}
