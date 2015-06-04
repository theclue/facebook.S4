#' @rdname facebook.users
#' @export
#'
#' @title 
#' Extract information about one or more Facebook users
#'
#' @description
#' \code{facebok.users} retrieves public information about one or more Facebook users.
#'
#' After version 2.0 of the Facebook API, only id, name, and picture are available
#' through the API. All the remaining fields will be missing.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{facebook.friends}}, \code{\link{getPosts}}, \code{\link{facebook.search}}
#'
#' @param users A vector of user or page IDs.
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#' 
#' @param users.fields vector or comma-separated string of fields to get (for user IDs)
#'
#' @param page.fields vector or comma-separated string of fields to get (for page IDs)
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about the authenticated user
#'  load("fb_oauth")
#'	fb <- facebook.users("me,9thcirclegames", token=fb_oauth)
#'	fb$username
#' }
#'
facebook.users <- function(users, token, user.fields = "id,name,first_name,last_name,gender,locale,picture.fields(url).type(large)", page.fields="id,name,category,picture.fields(url).type(large)"){
  
  users.pagination.define <- 25
  
  users.fields.url <- paste0(unique(
    unlist(strsplit(user.fields, split = ","))),
    collapse = ","
  )
  
  users.fields <- paste0(unique(
    unlist(strsplit(gsub('\\.fields\\((.*?)\\)','', 
                         gsub('\\.type\\((.*?)\\)','', user.fields, perl = TRUE)
                         , perl = TRUE), split = ","))),
    collapse = ","
  )

  pages.fields.url <- paste0(unique(
    unlist(strsplit(page.fields, split = ","))),
    collapse = ","
  )
  
  pages.fields <- paste0(unique(
    unlist(strsplit(gsub('\\.fields\\((.*?)\\)','', 
                         gsub('\\.type\\((.*?)\\)','', page.fields, perl = TRUE)
                         , perl = TRUE), split = ","))),
    collapse = ","
  )
  
  users.v <- unique(unlist(strsplit(users, split = ",")))
  users.f <- rep(seq_len(ceiling(length(users.v) / users.pagination.define)),each = users.pagination.define,length.out = length(users.v))
  users.chunks <- split(users.v, f = users.f)
  
  if(length(users.chunks) > 1){
    
    do.call(rbind,
            lapply(posts.chunks, function(single.chunk) {
              facebook.users(users = single.chunk, token = token, fields = fields)
              
            })
    )
    
  }
  
  else {

    ## first query: checking what users are actual users vs pages
    content <- callAPI(paste0("https://graph.facebook.com/v2.3/?ids=", paste0(users.v, collapse=",")), token)
    
    actual.users <- which(unlist(lapply(content, function(x) is.null(x$category))))
    actual.pages <- which(unlist(lapply(content, function(x) !is.null(x$category))))

    all.df <- rbind.fill((function() {

    if (length(actual.users)>0){

      url <- paste0(
        "https://graph.facebook.com/v2.3/?ids=",
        paste0(names(actual.users), collapse = ","),
        "&fields=", users.fields.url
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
      
      return(detailsDataToDF(content, fields = users.fields))
    } else return(data.frame())
      
    })()
    , (function() {
      
      if (length(actual.pages)>0){
        
        url <- paste0(
          "https://graph.facebook.com/v2.3/?ids=",
          paste0(names(actual.pages), collapse = ","),
          "&fields=", pages.fields.url
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
        
        return(detailsDataToDF(content, fields = pages.fields))
      } else return(data.frame())
      
    }

  )())
  # returning in original order of users
  # Something still wrong here...
  return(all.df[order(match(all.df$id, users.v)),])
  
  }
}
