#' A class to represent a collection of valid Facebook Pages.
#'
#' @slot pages character vector with the ids/names of all the pages of the collection.
#' @slot fields character vector with the content field of each page for the collection 
#' @slot data named list representing the raw collection
#' 
setClass("FacebookPageset",
         
         # Define the slots
         slots = c(pages = "character",
                   fields= "character",
                   data="list"
         ),
         
         # The inspector
         validity = function(object){
           return(TRUE)
         }
)


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

setMethod("as.list",
          signature(x = "FacebookPageset"),
          function (x, ...) 
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
              
              # TODO: add parameters
              url <- paste0(
                "https://graph.facebook.com/v2.3/?ids=", paste0(pages.v, collapse = ","),
                ifelse(length(page.fields), "&fields=", page.fields, "")
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

FacebookPageset <- function(pages, 
                            token, 
                            parameters = list(), 
                            fields = character(0)){
  
  return(new("FacebookPageset", pages = pages, token = token, parameters = parameters, fields = fields))
  
}


