setClass("FacebookGenericCollection",
         slots = c(id = "character",
                   fields= "character",
                   data="list",
                   token="ANY"
         ),
         validity = function(object){
           # TBD
           return(TRUE)
         }
)

setMethod("initialize",
          signature(.Object = "FacebookGenericCollection"),
          definition=function(.Object, id=NULL, token=NULL, parameters=list(), fields=character(0)){
            
            # Validate parameters
            validObject(.Object)

            # Create an empty object if not ids has been specified
            if(is.null(id) | is.null(token)){
              .Object@id <- character(0)
              return(.Object)
            }

            if(is.null(token)){
              return(.Object)
            }

            .Object@token <- token
            
            elements.pagination.define <- getOption("facebook.pagination")
            
            parsed <- parse.input.fields(fields)
            
            .Object@fields <- unlist(strsplit(parsed$fields, split = ","))

            elements.v <- unique(unlist(strsplit((function(){
              if(is(id, "FacebookGenericCollection")) return(id@id)
              return(id)
            })(), split = ",")))
            elements.f <- rep(seq_len(ceiling(length(elements.v) / elements.pagination.define)),each = elements.pagination.define,length.out = length(elements.v))
            elements.chunks <- split(elements.v, f = elements.f)
            
            if(length(elements.chunks) > 1){
              
              do.call(rbind,
                      lapply(c, function(single.chunk) {
                        new(class(.Object), id = id, token = token, parameters = parameters, fiels = fields, feed = feed)
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
                "https://graph.facebook.com/v", getOption("facebook.api"), "/?ids=", paste0(elements.v, collapse = ","),
                ifelse(length(parameters), paste0("&", query.parameters), ""),
                ifelse(length(fields), paste("&fields", parsed$url, sep="="), "")
              )

              content <- callAPI(url=url, token=token)
              
              # If ID is an atomic list or a collection of the same class, just push out the results
              if(!is(id, "FacebookGenericCollection") | (class(.Object) == class(id))){
              
              .Object@id <- names(content)

              .Object@data <- do.call(list, lapply(content, function(item){
                return(item[which(names(item) %in%  unlist(strsplit(parsed$fields, split = ",")))])
              }))
              
              } else {
              # Iterate the collection and clean the results
                
                .Object@data <- content
                
                # TODO 
                n <- 0
                if (n > 0) {
                  
                  min.since <- ifelse(!is.null(since), as.Date(since, origin="1970-01-01"), as.Date('1970/01/01'))
                  
                  container.ids <- character(0)
                  
                  all.elements <- lapply(content, function(sublist) {
                                         page <- 0
                                         p <- list()

                                         total.posts <- 0

                                         repeat {
                                           postdata <- NULL
                                           if(page == 0){
                                             postdata <- sublist[[1]]
                                           } else {
                                             postdata <- callAPI(url=next.url, token=token)
                                           }
                                           next.url <- postdata$paging$`next`
                                           
                                           container.ids <- c(container.ids, sublist$id)
                                           
                                           if(length(postdata$data) > 0) {
                                             p <- c(p, postdata$data)
                                             
                                           }
                                           
                                           # unregarding of since() query parameter, FB Graph somethimes
                                           # brings back posts older than 'since', so here
                                           # I'm also making sure the function stops when that happens
                                           p.unlist <- unlist(p)
                                           if(total.posts >= n |
                                              is.null(next.url) |
                                              ifelse(length(postdata$data) > 0, (min(formatFbDate(p.unlist[which(names(p.unlist))=="created_time"], "date")) < min.since), FALSE)
                                           )
                                           {
                                             return(head(p, n))
                                           }
                                           
                                           # Graceful waiting before next call
                                           Sys.sleep(0.5)
                                           
                                         }
                                       }
                                       )
                  
                  
                }
                
              }

            }
            return(.Object)
          }
)
