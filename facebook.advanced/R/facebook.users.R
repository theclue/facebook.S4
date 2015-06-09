#' @rdname facebook.users
#' @export
#'
#' @title 
#' Extract information about one or more Facebook users
#'
#' @description
#' \code{facebok.users} retrieves public information about one or more Facebook users or pages.
#'
#' After version 2.0 of the Facebook API, only id, name, and picture are available
#' through the API. All the remaining fields will be missing.
#' 
#' Since users and pages have different sets of attributes and you won't know in advance if a (commenting)
#' user is a page or not, this function will extract both. Parameters \code{users.fields} and \code{pages.fields}
#' are used to drive which attributes to get.
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
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints
#' nothing to the console.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#' ## Getting information about the authenticated user
#'  load("fb_oauth")
#'	fb <- facebook.users("me,9thcirclegames", token=fb_oauth)
#'	fb$username
#' }
#'
facebook.users <- function(users, 
                           token, 
                           user.fields = "id,name,first_name,last_name,gender,locale,picture.fields(url).type(large)", 
                           page.fields="id,name,category,picture.fields(url).type(large)",
                           .progress = create_progress_bar()){
  
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
    
    # Init the progress bar to the number of queries that will be performed (2 queries for each chunk)
    .progress$init((length(users.chunks)*2)+1)
    .progress$step()
    
    do.call(rbind,
            lapply(users.chunks, function(single.chunk) {
              facebook.users(users = single.chunk, token = token, fields = fields, .progress = .progress)
              
            })
    )
    
  }
  
  else {

    ## first query: checking what users are actual users vs pages
    content <- facebook.query(query=paste0("?ids=", paste0(users.v, collapse=",")), token=token)
    
    actual.users <- which(unlist(lapply(content, function(x) is.null(x$category))))
    actual.pages <- which(unlist(lapply(content, function(x) !is.null(x$category))))

    all.df <- rbind.fill((function() {

    if (length(actual.users)>0){

      query <- paste0(
        "?ids=",
        paste0(names(actual.users), collapse = ","),
        "&fields=", users.fields.url
      )

      content <- facebook.query(query=query, token=token)
      
      # Advance the progress bar
      if(inherits(try(.progress$step(), silent=T), "try-error")){
        .progress$init((length(users.chunks)*2)+1)
        .progress$step()
      }
      
      return(detailsDataToDF(content, fields = users.fields))
    } else return(data.frame())
      
    })()
    , (function() {
      
      if (length(actual.pages)>0){
        
        query <- paste0(
          "?ids=",
          paste0(names(actual.pages), collapse = ","),
          "&fields=", pages.fields.url
        )

        content <- facebook.query(query=query, token=token)
        
        # Advance the progress bar
        if(inherits(try(.progress$step(), silent=T), "try-error")){
          .progress$init((length(users.chunks)*2)+1)
          .progress$step()
        }
        
        return(detailsDataToDF(content, fields = pages.fields))
      } else return(data.frame())
      
    }

  )())
  # returning in original order of users
  # TODO: Something still wrong here...
  return(all.df[order(match(all.df$id, users.v)),])
  
  }
}
