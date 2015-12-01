setMethod("[",
          signature="FacebookGenericCollection",
          function(x,i,j,drop){
            
            empty.set <- new(class(x))
            
            slot(empty.set, "fields") <- x@fields
            slot(empty.set, "token") <- x@token
            
            slot(empty.set, "id") <- (function(idx){
              if(is.numeric(i)) return(x@id[i])
              return(x@id[x@id %in% as.character(i)])
            })(i)
            
            slot(empty.set, "data") <- (function(idx){
              if(is.numeric(i)) return(x@data[i])
              return(x@data[x@data %in% as.character(i)])
            })(i)
            
            return(empty.set)
          }
)

setMethod("c",
          signature(x = "FacebookGenericCollection"),
          function (x, ..., recursive = FALSE) 
          {
            optional.elems <- list(...)
            empty.set <- new(class(x), fields = x@fields)
            
            # Only bind collections of the same kind
            lapply(optional.elems, function(list.elem) {
              stopifnot(class(x) != class(list.elem))
            })
            
            empty.set@data <- (do.call(c, list(x@data,
                                               do.call(c,lapply(optional.elems, slot, "data"))
            )
            ))
            
            # TODO: add dummy fields for subcollections without certain fields?
            empty.set@fields <- unique(do.call(c, list(x@fields,
                                                      do.call(c,lapply(optional.elems, slot, "fields"))
            )
            ))
            
            empty.set@token <- x@token
            
            return(empty.set)
          }
)

#' TODO: better check for null
#' @export
as.data.frame.FacebookGenericCollection <- function (x, row.names = FALSE, optional = FALSE, ...) {
  df <- detailsDataToDF(x@data, x@fields)
  
  numeric.cols <- which(grepl("(total|count)", colnames(df), perl=TRUE))
  logical.cols <- which(grepl("(has|can)", colnames(df), perl=TRUE))
  datetime.cols <- which(grepl("time", colnames(df), perl=TRUE))
  
  df[,numeric.cols] <- as.numeric(df[,datetime.cols])
  df[,datetime.cols] <- formatFbDate(df[,datetime.cols])
  df[,logical.cols] <- as.logical(df[,logical.cols]) 
  
  if(row.names == TRUE){
    row.names(df) <- x@id
  }
  
  return(df)
}
setAs("FacebookGenericCollection", "data.frame", function(from) as.data.frame.FacebookGenericCollection(from))
setMethod("as.data.frame", signature(x = "FacebookGenericCollection", row.names = "logical", optional = "logical"), as.data.frame.FacebookGenericCollection)

as.list.FacebookGenericCollection <- function (x, ...) 
{
  optional.elems <- list(...)
  
  # Only bind collections of the same kind
  lapply(optional.elems, function(list.elem) {
    stopifnot(class(x) != class(list.elem))
  })
  
  return(do.call(c, list(x@data,
                         do.call(c,lapply(optional.elems, slot, "data"))
  )
  ))
}
setAs("FacebookGenericCollection", "list", function(from) as.list.FacebookGenericCollection(from))
setMethod("as.list", signature(x = "FacebookGenericCollection"), as.list.FacebookGenericCollection)
