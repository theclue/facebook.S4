#' @include generic-methods.R
NULL

setMethod("summary",
          signature(object = "FacebookGenericCollection"),
          function (object, ...) 
          {
            cat(paste0(rep_len("-", nchar(class(object))), collapse=""))
            cat(paste0("\n", class(object), "\n"))
            cat(paste0(rep_len("-", nchar(class(object))), collapse=""))
            cat(paste0("\nLength: ", length(object), " element", ifelse(length(object) != 1, "s", "")))
            cat(paste0("\nFields: ", paste(object@fields, collapse=", ")))
            cat(paste0("\nParent Collection: ", 
                       ifelse(!is.null(object@parent.collection), 
                              paste0(class(object@parent.collection),
                                     " (",
                                     length(object@parent.collection),
                                     " element", ifelse(length(object@parent.collection) != 1, "s", ""), ")"), "NA")
            )
            )
            cat(paste0("\n", ifelse(length(object) > 5 , "First 5 IDs: ", "IDs: "), paste0(head(object@id, 5), collapse=", ")))
            cat("\n\nFacebook Application ID:", ifelse(is.character(object@token), "NONE - A token from Graph API Explorer was used", object@token$app$key))
            cat("\n\nContent Example (only the first 3 fields are shown):\n")
            
            separator <- (function(){
              if(length(object@fields) > 1) {
                return(paste0(rep_len("-", sum(apply(head(as.data.frame(object),5)[,1:min(3, length(object@fields))], MARGIN=2, function(r) { max(nchar(as.character((r))))})) + 4), collapse=""))
              }
              return(paste0(rep_len("-", max(nchar(head(as.data.frame(object),5)[,1])) + 2), collapse = ""))
            })()
            
            cat(paste0(separator, "\n"))
            if(length(object@fields) > 1) {
              print(head(as.data.frame(object), 5)[,1:min(3, length(object@fields))])
            } else {
              snap <- as.data.frame(head(as.data.frame(object), 5)[,1])
              colnames(snap) <- object@fields
              print(snap)
            }
            cat(ifelse(length(object) > 5, paste0("\n (", length(object) - 5, " more element", ifelse(length(object) - 5 != 1, "s", ""), ")\n"), ""))
            cat(paste0(separator, "\n"))
            invisible(object)
          }
)

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
            
            slot(empty.set, "parent") <- (function(idx){
              if(is.numeric(i)) return(x@parent[i])
              return(x@parent[x@id %in% as.character(i)])
            })(i)
            
            slot(empty.set, "parent.collection") <- (function(idx){
              return(x@parent.collection)
            })(i)
            
            slot(empty.set, "type") <- (function(idx){
              if(is.numeric(i)) return(x@type[i])
              return(x@type[x@id %in% as.character(i)])
            })(i)
            
            slot(empty.set, "data") <- (function(idx){
              if(is.numeric(i)) return(x@data[i])
              return(x@data[x@id %in% as.character(i)])
            })(i)
            
            return(empty.set)
          }
)

setMethod("length",
          signature="FacebookGenericCollection",
          function(x){
            return(length(x@id))
          }
)

setMethod("as.character",
          signature="FacebookGenericCollection",
          function(x){
            return(paste0(x@id, collapse=","))
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
              if(!is(list.elem, class(x))){
                stop(paste0("Cannot bind collection of different kinds. Found: ", class(list.elem), ". Expected: ", class(x)))
              }
            })
            
            id <- (do.call(c, list(x@id,
                                   do.call(c,lapply(optional.elems, slot, "id"))
            )
            ))
            
            parent <- (do.call(c, list(x@parent,
                                       do.call(c,lapply(optional.elems, slot, "parent"))
            )
            ))
            
            duplicated.idx <- duplicated(matrix(c(id,parent), ncol=2))
            
            empty.set@id <- id[!duplicated.idx]
            
            empty.set@data <- (do.call(c, list(x@data,
                                               do.call(c,lapply(optional.elems, slot, "data"))
            )
            ))[!duplicated.idx]
            
            empty.set@fields <- unique(do.call(c, list(x@fields,
                                                       do.call(c,lapply(optional.elems, slot, "fields"))
            )
            ))
            
            empty.set@parent <- parent[!duplicated.idx]
            
            empty.set@type <- (do.call(c, list(x@type,
                                               do.call(c,lapply(optional.elems, function(x){ as.character(slot(x, "type"))}))
            )
            ))[!duplicated.idx]
            
            secondary.collection <-
              lapply(
                optional.elems, function(p) {
                  p@parent.collection
                })
            
            # TODO: remove dupes while concatening collections
            
            empty.set@parent.collection <- c(x@parent.collection, do.call(c, secondary.collection))
            
            empty.set@token <- x@token
            
            return(empty.set)
          }
)

#' @noRd
#' @export
as.data.frame.FacebookGenericCollection <- function (x, row.names = FALSE, optional = FALSE, ...) {
  df <- detailsDataToDF(x@data, x@fields)
  
  numeric.cols <- which(grepl("(total|count)", colnames(df), perl=TRUE))
  logical.cols <- which(grepl("(has|can)", colnames(df), perl=TRUE))
  datetime.cols <- which(grepl("time", colnames(df), perl=TRUE))
  
  for(n in names(df)[numeric.cols]){df[[n]] <- as.numeric(df[[n]])}
  for(n in names(df)[datetime.cols]){df[[n]] <- formatFbDate(df[[n]])}
  for(n in names(df)[logical.cols]){df[[n]] <- as.logical(df[[n]])}
  
  if(row.names == TRUE){
    row.names(df) <- x@id
  }
  
  return(df)
}
setAs("FacebookGenericCollection", "data.frame", function(from) as.data.frame.FacebookGenericCollection(from))
setMethod("as.data.frame", signature(x = "FacebookGenericCollection", row.names = "logical", optional = "logical"), as.data.frame.FacebookGenericCollection)

#' @noRd
#' @export
as.list.FacebookGenericCollection <- function (x, ...) 
{
  optional.elems <- list(...)
  
  # Only bind Collections of the same types
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
