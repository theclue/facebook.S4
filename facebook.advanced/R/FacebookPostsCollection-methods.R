#' @export
#' @title
#'  Checks if it's a valid collection of posts
#'  
#'  @description
#'  \code{is.Postset} checks if \code{x} is a valid collection of Facebook Posts built using \code{\link{FacebookPostsCollection}}
is.PostsCollection <- function(x) is(x, "FacebookPostsCollection")

#' @describeIn as.list Convert a collection of posts in a named list
setMethod("as.list", signature(x = "FacebookPostsCollection"), as.list.FacebookGenericCollection)

setMethod("as.data.frame", signature(x = "FacebookPostsCollection", row.names = "logical", optional = "logical"), 
          definition=function (x, row.names = FALSE, optional = FALSE, ...) {
            df <- callNextMethod(x, row.names = row.names, optional = optional)
            if(optional == FALSE){
              df <- df[,-which(grepl("(comments|likes)\\.summary\\.(can|has)_", colnames(df), perl=TRUE))]
            }
            
            return(df)
          })
