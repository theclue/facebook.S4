#' @include generic-methods.R
#' 
#' @title
#' A generic collection of Facebook items
#' 
#' @name FacebookGenericCollection-class
#' @exportClass FacebookGenericCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @keywords internal
setClass("FacebookGenericCollection",
         slots = c(id = "ANY",
                   fields = "character",
                   data = "list",
                   token = "ANY",
                   parent = "character",
                   parent.type = "character"
         ),
         validity = function(object){
           # TBD
           return(TRUE)
         }
)

setMethod("initialize",
          signature(.Object = "FacebookGenericCollection"),
          definition=function(.Object, id=NULL, token=NULL, parameters=list(), fields=character(0), n){
            
            # Validate parameters
            validObject(.Object)
            
            .Object@token <- (function(){ 
              if(is.null(token) & is(id, "FacebookGenericCollection")){
                return(id@token)
              } else return(token)
            })()
            
            # Create an empty object if not ids has been specified
            if(is.null(id) | is.null(token)){
              .Object@id <- character(0)
              return(.Object)
            }

            parsed <- parse.input.fields(fields)
            
            .Object@fields <- unlist(strsplit(parsed$fields, split = ","))
            
            .Object@parent.type <- (function(){ 
              if(is(id, "FacebookGenericCollection")){
                return(class(id)[1])
              } else return(NA)
            })()
            
            elements.v <- (function(id){
              if(!is(id, "FacebookGenericCollection")) {
                return(unique(unlist(strsplit(id, split = ","))))
                }
              return(id)
            })(id)

            elements.f <- rep(seq_len(ceiling(length(elements.v) / getOption("facebook.pagination"))),each = getOption("facebook.pagination"),length.out = length(elements.v))
            elements.chunks <- split(elements.v, f = elements.f)

            if(length(elements.chunks) > 1){
              return(do.call(c, 
                             unname(
                               lapply(
                                 elements.chunks, function(single.chunk) {
                                   new(class(.Object), id = single.chunk, token = token, parameters = parameters, fields = fields, n = n)
                                 })
                             )
              )
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
                "https://graph.facebook.com/v", getOption("facebook.api"), "/?ids=", paste0(elements.v, collapse=","),
                ifelse(length(parameters), paste0("&", query.parameters), ""),
                ifelse(length(fields), paste("&fields", parsed$url, sep="="), "")
              )
              #print(url)
              content <- callAPI(url=url, token=token)
              
              # If ID is an atomic list or a collection of the same class, just push out the results
              if(!is(elements.v, "FacebookGenericCollection") | (is(elements.v, class(.Object)))){

                .Object@id <- names(content)
                
                .Object@data <- do.call(list, lapply(content, function(item){
                  return(item[which(names(item) %in%  parsed$fields)])
                }))
                
                .Object@parent <- as.character(rep(NA, length(id)))
                
              } else {
                # If id is a collection, iterate it and clean the results
                all.parents <- character(0)

                if (n > 0) {
                  
                  min.since <- ifelse(!is.null(parameters$since), as.Date(parameters$since, origin="1970-01-01"), as.Date('1970/01/01'))
                  
                  all.elements <- lapply(lapply(content, function(sublist) {
                    page <- 0
                    page.results <- list()
                    
                    total.posts <- 0
                    
                    repeat {
                      postdata <- NULL
                      if(page == 0){
                        
                        postdata <- sublist[[1]]
                      } else {
                        postdata <- callAPI(url=next.url, token=token)
                      }
                      next.url <- postdata$paging$`next`
                      
                      min.time <- Inf

                      if(length(postdata$data) > 0) {
                        page.results <- do.call(c, list(page.results,lapply(postdata$data, function(s){
                          ss <- list()
                          
                          min.time <<- min(min.time, ifelse(!is.null(s$created_time), formatFbDate(s$created_time, "date"), Inf))
                          ss[[s$id]] <- s
                          return(ss)
                        })))
                        
                        page <- page + 1
                        total.posts <- total.posts + length(postdata$data)
                      }
                      
                      # unregarding of since() query parameter, FB Graph sometimes
                      # brings back posts older than 'since', so here
                      # I'm also making sure the function stops when that happens
                      if(total.posts >= n |
                         is.null(next.url) |
                         ifelse(length(postdata$data) > 0, (("created_time" %in% parsed$fields) & (min.time < min.since)), FALSE)
                      )
                      {
                        # Quick warkaround for parent assignment. I'm not very proud of it...
                        all.parents <<- c(all.parents, as.character(rep(sublist$id, min(total.posts, n))))
                        return(head(page.results, n))
                      }
                      
                      # Graceful waiting before next call
                      Sys.sleep(0.5)
                      
                    }
                  }
                  
                  ), function(cont){do.call(c,lapply(cont, function(cc) {cc}))})
                  
                  names(all.elements) <- NULL
                  
                  .Object@parent <- all.parents
                  .Object@data <- do.call(c, all.elements)
                  .Object@id <- names(.Object@data)
                }
                
              }
              
            }
            return(.Object)
          }
)
