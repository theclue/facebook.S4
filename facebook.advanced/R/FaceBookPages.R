setClass("FacebookPageSet",
         
         # Define the slots
         slots = c(pages = "character",
                   parameters = "list",
                   fields= "character",
                   data="list"
         ),
         
         # The inspector
         validity = function(object){
           return(TRUE)
         }
)

setMethod("c",
          signature(x = "FacebookPageSet"),
          function (x, ..., recursive = FALSE) 
          {
            optional.elems <- list(...)
            empty.set <- new("FacebookPageSet", parameters = x@parameters, fields = x@fields)
            
            lapply(optional.elems, function(list.elem) {
              stopifnot("FacebookPageSet" %in% class(list.elem))
            })
            
            empty.set@data<- (do.call(c, list(x@data,
                                              do.call(c,lapply(optional.elems, slot, "data"))
            )
            ))
            
            empty.set@fields<- unique(do.call(c, list(x@fields,
                                                      do.call(c,lapply(optional.elems, slot, "fields"))
            )
            ))
            
            return(empty.set)
            
          }
)

setMethod("as.list",
          signature(x = "FacebookPageSet"),
          function (x, ...) 
          {
            optional.elems <- list(...)
            lapply(optional.elems, function(list.elem) {
              stopifnot("FacebookPageSet" %in% class(list.elem))
            })
            return(do.call(c, list(x@data,
                                   do.call(c,lapply(optional.elems, slot, "data"))
            )
            ))
          }
)


setMethod("initialize",
          signature(x = "FacebookPageSet"),
          definition=function(.Object, pages=NULL, token=NULL, parameters=list(), fields=character(0)){
            
            # Validate parameters
            validObject(.Object)
            
            # Create an empty object
            if(is.null(pages) | is.null(token)){
              .Object@pages <- character(0)
              .Object@parameters <- parameters
              return(.Object)
            }
            
            .Object@parameters <- parameters
            
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
                        new("FacebookPageSet", pages = pages, token = token, parameters = parameters, fiels = fields )
                      })
              )
              
            }
            
            else {
              
              url <- paste0(
                "https://graph.facebook.com/v2.3/?ids=", paste0(pages.v, collapse = ","),
                "&fields=", page.fields
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

# Commodity constructor
FacebookPageSet <- function(pages, 
                            token, 
                            parameters = list(), 
                            fields = character(0)){
  
  return(new("FacebookPageSet", pages = pages, token = token, parameters = parameters, fields = fields))
  
}

ciccio <- FacebookPageSet(pages="9thcirclegames", 
                          token="CAACEdEose0cBADvheGGZBEBtr8fhoxy12hltKpvykvBhFJni79PT9jPOxS6T5149lGR2k5kj13us6hkJyV67ozdezmu45amjnMdJV1FWItHCHYM1llAjxrvOkM55n4i3u913Juh6XpHmdgdGCFOeaWMYgdBjd0NidZASxI9GNWE425xuEoWLzbMaK0zt0Qa5VkkZCBzj5XhzAwZAy0Okeo2vUbdNLnAZD", 
                          fields = c("username", "name", "about", "category", "description", "likes", "link", "talking_about_count"))


pasticcio <- FacebookPageSet(pages="NathanNeverSergioBonelliEditore,DegenesisRebirth", 
                             token="CAACEdEose0cBADvheGGZBEBtr8fhoxy12hltKpvykvBhFJni79PT9jPOxS6T5149lGR2k5kj13us6hkJyV67ozdezmu45amjnMdJV1FWItHCHYM1llAjxrvOkM55n4i3u913Juh6XpHmdgdGCFOeaWMYgdBjd0NidZASxI9GNWE425xuEoWLzbMaK0zt0Qa5VkkZCBzj5XhzAwZAy0Okeo2vUbdNLnAZD", 
                             fields = c("username", "name", "about", "category", "description", "likes", "link", "talking_about_count"))


ciccio.bind <- c(ciccio, pasticcio, ciccio)

