#' @rdname facebook.get
#' @export
#'
#' @title 
#' Extract lists of IDs
#'
#' @description
#' \code{facebook.get} returns a vector of node IDs to be used in other lookup functions.
#' 
#' @details
#' This generic function conveniently returns a
#' data.frame of \code{type} belonging to \code{ids}.
#' It's particulary useful and fast when children eventually paginate.
#' 
#' All \code{ids} must share the same type.
#' 
#' This function, bundled with a good understanding of the Facebook Data Model
#' enable to get virtually any kind of node from Facebook Graph API.
#' 
#' If partent ID ha no children of \code{type}, an error is triggered.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{facebook.query}}, \code{\link{facebook.posts}}, \code{\link{fbOAuth}}
#'
#' @param ids string vector comma-delimited string with IDs of the given type to extract.
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth}.
#'
#' @param type string with the child elements to get. For example \code{posts}, \code{comments}, \code{likes}.
#'
#' @param n Maximum number of elements to return for each ID.
#' 
#' @param fields vector or comma-separated string of fields to get
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints
#' nothing to the console.
#'
#' @examples \dontrun{
#'  load("fb_oauth")
#'  some.posts <- facebook.get(ids="9thcirclegames", type="posts", token=fb_oauth)
#' }
#'
facebook.get <- function(ids,
                         token,
                         type = "posts",
                         n=500,
                         fields="id",
                         .progress = create_progress_bar()){
  
  
  details.pagination.define <- 100
  id.pagination.define <- 10
  
  parsed <- parse.input.fields(fields)
  
  
  id.v <- unique(unlist(strsplit(ids, split = ",")))
  id.f <- rep(seq_len(ceiling(length(id.v) / id.pagination.define)),each = id.pagination.define,length.out = length(id.v))
  id.chunks <- split(id.v, f = id.f)
  
  if(length(id.chunks) > 1){
    
    # Init the progress bar
    .progress$init(length(ids)+1)
    .progress$step()
    
    # Recursive calls for each chunk
    do.call(rbind,
            lapply(id.chunks, function(single.chunk) {
              facebook.get(ids = single.chunk, token = token, n = n, fields = fields, .progress = .progress)         
            })
    )   
  }
  
  else {
    
    query <- URLencode(
      paste0(
        "?ids=",
        paste0(id.v, collapse = ","),
        "&fields=id,name,",
        type, 
        ifelse(n > 0, paste0(".fields(", parsed$url, ").limit(", ifelse(n > details.pagination.define, details.pagination.define, n), ")"), ".limit(0)")
      )
    )
    
    content <- facebook.query(query = query, token = token)
    
    # Children
    all.Children <- data.frame()
    if (n > 0) {
      
      all.Children <- do.call(rbind.fill,
                              lapply(content, function(sublist) {
                                page <- 0
                                l <- data.frame()
                                total.children <- 0
                                
                                # Advance the progress bar
                                if(inherits(try(.progress$step(), silent=T), "try-error")){
                                  .progress$init(length(ids)+1)
                                  .progress$step()
                                }
                                
                                repeat {
                                  childrendata <- NULL
                                  if(page == 0){
                                    childrendata <- sublist[[type]] 
                                  } else {
                                    childrendata <- facebook.query(query=next.url, token=token, endpoint=NULL)
                                  }
                                  next.url <- childrendata$paging$`next`
                                  
                                  l.page <- detailsDataToDF(childrendata$data, fields = parsed$fields)
                                  
                                  if(!is.null(l.page) && nrow(l.page) > 0) {
                                    
                                    l.page$parent.id <- sublist$id
                                    l <- rbind.fill(l, l.page)
                                    page <- page + 1
                                    total.children <- total.children + nrow(l.page)
                                    
                                  }
                                  
                                  if(total.children >= n |
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
    return(all.Children)
  }
}
