#' @include generic-methods.R
NULL

#' A class to represent a collection of valid Facebook Pages.
#'
#' @slot pages character vector with the ids/names of all the pages of the collection.
#' @slot fields character vector with the content field of each page for the collection 
#' @slot data named list representing the raw collection
#' @name FacebookPageset-class
#' @rdname FacebookPageset-class
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @exportClass FacebookPageset
#' @family API Resources
setClass("FacebookPageset",
         slots = c(pages = "character",
                   fields= "character",
                   data="list"
         ),
         validity = function(object){
           # TBD
           return(TRUE)
         }
)

#' @rdname facebookpageset-methods
setMethod("[",
  signature="FacebookPageset",
  function(x,i,j,drop){
    empty.set <- new("FacebookPageset")
    
    slot(empty.set, "fields", x@fields)
    
    slot(empty.set, "pages") <- (function(idx){
      if(is.numeric(i)) return(x@pages[i])
      return(x@pages[x@pages %in% as.character(i)])
    })(i)
    
    slot(empty.set, "data") <- (function(idx){
      if(is.numeric(i)) return(x@data[i])
      return(x@data[x@data %in% as.character(i)])
    })(i)
    
    return(empty.set)
   }
)

#' @rdname facebookpageset-methods
setMethod("c",
          signature(x = "FacebookPageset"),
          function (x, ..., recursive = FALSE) 
          {
            optional.elems <- list(...)
            empty.set <- new("FacebookPageset", fields = x@fields)
            
            lapply(optional.elems, function(list.elem) {
              stopifnot("FacebookPageset" %in% class(list.elem))
            })
            
            empty.set@data<- (do.call(c, list(x@data,
                                              do.call(c,lapply(optional.elems, slot, "data"))
            )
            ))
            
            # TODO: add dummy fields for subcollections without certain fields?
            empty.set@fields<- unique(do.call(c, list(x@fields,
                                                      do.call(c,lapply(optional.elems, slot, "fields"))
            )
            ))
            
            return(empty.set)
            
          }
)


setMethod("initialize",
          signature(.Object = "FacebookPageset"),
          definition=function(.Object, pages=NULL, token=NULL, parameters=list(), fields=character(0)){
            
            # Validate parameters
            validObject(.Object)
            
            # Create an empty object
            if(is.null(pages) | is.null(token)){
              .Object@pages <- character(0)
              return(.Object)
            }
            
            # Get the pages
            pages.pagination.define <- 25
            
            page.fields <- paste0(unique(
              unlist(strsplit(fields, split = ","))),
              collapse = ","
            )
            pages.v <- unique(unlist(strsplit(pages, split = ",")))
            pages.f <- rep(seq_len(ceiling(length(pages.v) / pages.pagination.define)),each = pages.pagination.define,length.out = length(pages.v))
            pages.chunks <- split(pages.v, f = pages.f)
            
            if(length(pages.chunks) > 1){
              
              do.call(rbind,
                      lapply(c, function(single.chunk) {
                        new("FacebookPageset", pages = pages, token = token, parameters = parameters, fiels = fields )
                      })
              )
              
            }
            
            else {
              
              query.parameters <- sub("&$", "",
                                      sub('([[:punct:]])\\1+', '\\1',
                                          do.call(paste, list(
                                            lapply(seq_along(parameters), function(y, n, i) {
                                              if(is.null((y[[i]]))) return("")
                                              paste(n[[i]], y[[i]], sep="=")
                                            }
                                            , y=parameters, n=names(parameters)),
                                            collapse = "&"))
                                      )
              )
              url <- paste0(
                "https://graph.facebook.com/v2.3/?ids=", paste0(pages.v, collapse = ","),
                ifelse(length(parameters), paste0("&", query.parameters), ""),
                ifelse(length(page.fields), paste("&fields", page.fields, sep="="), "")
              )

              content <- callAPI(url=url, token=token)
              
              # Check for permission
              if (length(content)==0) stop("You're not authorized to get this information, Please check your permissions before retrying.")
              
              error <- 0
              while (length(content$error_code)>0){
                cat("Error!\n")
                Sys.sleep(0.5)
                error <- error + 1
                content <- callAPI(url=url, token=token)		
                if (error==3){ stop(content$error_msg) }
              }
              
              .Object@pages <- names(content)
              
              .Object@fields <- unique(
                do.call(c, lapply(
                  content, function(item){ names(item)[which(names(item) %in%  unlist(strsplit(page.fields, split = ",")))] }
                )
                )
              )
              
              .Object@data <- do.call(list, lapply(content, function(item){
                return(item[which(names(item) %in%  unlist(strsplit(page.fields, split = ",")))])
                
              }))
              
            }
            return(.Object)
          }
)

#' @export
#' @describeIn FacebookPageset-class Constructs a new \code{FacebookPageset} object.
FacebookPageset <- function(pages, 
                            token, 
                            parameters = list(), 
                            fields = character(0)){
  
  return(new("FacebookPageset", pages = pages, token = token, parameters = parameters, fields = fields))
  
}

#' @export
#' @describeIn FacebookPageset-class Checks whether \code{x} is a \code{FacebookPageSet}.
is.Pageset <- function(x) is(x, "FacebookPageset")

#' @export
#' @rdname facebookpageset-methods
as.list.FacebookPageset <- function (x, ...) 
{
  optional.elems <- list(...)
  lapply(optional.elems, function(list.elem) {
    stopifnot("FacebookPageset" %in% class(list.elem))
  })
  return(do.call(c, list(x@data,
                         do.call(c,lapply(optional.elems, slot, "data"))
  )
  ))
}
setAs("FacebookPageset", "list", function(from) as.list.FacebookPageset(from))

#' @rdname facebookpageset-methods
setMethod("as.list", signature(x = "FacebookPageset"), as.list.FacebookPageset)

#' @export
#' @rdname facebookpageset-methods
as.data.frame.FacebookPageset <- function (x, row.names = NULL, optional = FALSE, ...) {
  df <- detailsDataToDF(x@data, x@fields)
  if(row.names == TRUE){
    row.names(df) <- x@pages
  }
  return(df)
}
setAs("FacebookPageset", "data.frame", function(from) as.data.frame.FacebookPageset(from))

#' @rdname facebookpageset-methods
setMethod("as.data.frame", signature(x = "FacebookPageset"), as.data.frame.FacebookPageset)
