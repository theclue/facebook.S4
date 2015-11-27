#' @export
#' @title
#'  Checks if it's a valid collection of posts
#'  
#'  @description
#'  \code{is.Postset} checks if \code{x} is a valid collection of Facebook Posts built using \code{\link{FacebookPostsCollection}}
is.Postset <- function(x) is(x, "FacebookPostsCollection")

#' @describeIn as.list Convert a collection of posts in a named list
setMethod("as.list", signature(x = "FacebookPostsCollection"), as.list.FacebookGenericCollection)

#' @export
as.data.frame.FacebookPostsCollection <- function (x, row.names = FALSE, optional = FALSE, ...) {
  df <- data.frame(detailsDataToDF(x@data, x@fields),
                   summaryDataToDF(x@data, fields = "likes.count,comments.count,shares.count"))
  if(row.names == TRUE){
    row.names(df) <- x@id
  }
  return(df)
}
setAs("FacebookPostsCollection", "data.frame", function(from) as.data.frame.FacebookPostsCollection(from))
setMethod("as.data.frame", signature(x = "FacebookPostsCollection", row.names = "logical", optional = "logical"), as.data.frame.FacebookPostsCollection)
