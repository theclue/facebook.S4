#' @include generic-methods.R
#' 
#' @title
#' A generic collection of Facebook elements
#' 
#' @description
#' Connect to Facebook Graph API, get public information from a list of Facebook elements of any kind and build a \code{FacebookUsersCollection-class}
#' instance.
#' 
#' @details 
#' This collection should not be built directly, as it performs no sanity check on its content.
#' However, among all the available collections, it's the only one that can have mixed content inside, so
#' many commodity endpoint functions like \code{\link{facebook.search}} fill this as return value.
#' But, for the same reasons, it cannot hold fields different from id.
#' 
#' If you exactly know what you're doing you \emph{could} eventually build an instance of this class to perform generic queries to Graph API,
#' but it's not guaranteed to work and it probably won't, actually.
#' 
#' @name FacebookGenericCollection-class
#' @exportClass FacebookGenericCollection
#' 
#' @template collection-slots
#' 
#' @author Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @keywords internal
#' @importFrom utils URLencode
#' @importFrom plyr create_progress_bar progress_none
setClass("FacebookGenericCollection",
         slots = c(id = "ANY",
                   fields = "character",
                   data = "ANY",
                   token = "ANY",
                   parent = "character",
                   parent.collection = "ANY",
                   type = "character"
         )
)

setMethod("initialize",
          signature(.Object = "FacebookGenericCollection"),
          definition=function(.Object, 
                              id = NULL,
                              token = NULL,
                              parameters = list(),
                              fields = character(0),
                              n = getOption("facebook.maxitems"),
                              metadata = FALSE,
                              .progress = create_progress_bar(),
                              stop.condition = function(x){ FALSE }){
            
            if(metadata & is(id, "FacebookGenericCollection")){
              stop("Cannot inherit metadata from a collection. If you need to pull metadata, <id> must be an atomic character vector.")
            }
            
            token <- (function(){ 
              if(is.null(token) & is(id, "FacebookGenericCollection")){
                if(getOption("facebook.verbose")) message("No token specified. The token of the input collection will be used instead.")
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
            
            .Object@token <- token
            
            .Object@parent.collection <- (function(){ 
              if(is(id, "FacebookGenericCollection")){
                return(id)
              } else return(NULL)
            })()
            
            elements.v <- (function(id){
              if(!is(id, "FacebookGenericCollection")) {
                return(unique(unlist(strsplit(id, split = ","))))
              } else {
                return(id)
              }
            })(id)
            
            elements.f <- rep(seq_len(ceiling(length(elements.v) / getOption("facebook.pagination"))),
                              each = getOption("facebook.pagination"),
                              length.out = length(elements.v))
            
            elements.chunks <- split(elements.v,
                                     f = elements.f)
            
            if(length(elements.chunks) > 1){
              
              # Init the progress bar
              .progress$init(length(elements.chunks))
              .progress$step()
              
              return(do.call(c, 
                             unname(
                               lapply(
                                 elements.chunks, function(single.chunk) {
                                   new(class(.Object),
                                       id = single.chunk,
                                       token = token,
                                       parameters = parameters,
                                       fields = fields,
                                       n = n,
                                       metadata = metadata,
                                       .progress = .progress)
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
                "https://graph.facebook.com/", getOption("facebook.api"), "/",
                "?metadata=", as.numeric(ifelse(is.null(metadata), FALSE, metadata)),
                "&ids=", paste0(elements.v, collapse=","),
                ifelse(length(parameters), paste0("&", query.parameters), ""),
                ifelse(length(fields), paste("&fields", parsed$url, sep="="), "")
              )
              
              if(getOption("facebook.verbose")) message("Query URL: ", url)
              
              content <- callAPI(url=URLencode(url), token=token)
              
              # If ID is an atomic list, just push out the results
              if(!is(elements.v, "FacebookGenericCollection")){
                
                .Object@id <- names(content)
                
                .Object@data <- do.call(list, lapply(content, function(item){
                  return(item[which(names(item) %in%  parsed$fields)])
                }))

                .Object@parent <- as.character(rep(NA, length(elements.v)))
                
                .Object@type <- (unname(sapply(content, function(item){
                  return(ifelse(is.null(item$metadata$type), as.character(NA), item$metadata$type))
                })))
                
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
                        
                        if(length(which((names(sublist) != "id") & 
                                 (names(sublist) != "paging") &  
                                 (names(sublist) != "metadata"))) == 0){
                          return(page.results)
                        }
                        
                        postdata <- sublist[[which((names(sublist) != "id") & 
                                                     (names(sublist) != "paging") &  
                                                     (names(sublist) != "metadata"))
                                             ]]
                      } else {
                        postdata <- callAPI(url=next.url, token=token)
                      }
                      
                      next.url <- (function(p){
                        if(is(p, "list")) {
                          return(p$paging$`next`)
                        }
                        return(NULL)
                      })(postdata)
                      
                      min.time <- Inf
                      valid.posts <- 0
                      
                      length.data <- ifelse(is(postdata, "list"), length(postdata$data), 0)
                      
                      if(length.data > 0) {
                        page.results <- do.call(c, list(page.results,lapply(postdata$data, function(s){
                          ss <- list()
                          
                          min.time <<- min(min.time, ifelse(!is.null(s$created_time), formatFbDate(s$created_time, "date"), Inf))

                          if(min.time < min.since){
                            return(NULL)
                          }
                          
                          # This lambda function is used as a custom stopping condition for the pagination loop
                          # TODO:
                          # - The exit condition should immediately end after the matching item, not the current page
                          #
                          # KNOWN LIMITATIONS:
                          # - Setting next.url this way will have unpredictable results in parallel executions
                          if(stop.condition(s)){
                            next.url <<- NULL
                          }
                          
                          valid.posts <<- valid.posts + 1
                          ss[[s$id]] <- s
                          return(ss)
                        })))
                        page <- page + 1
                        total.posts <- total.posts + valid.posts
                      }
                      
                      # unregarding of since() query parameter, FB Graph sometimes
                      # brings back posts older than 'since', so here
                      # I'm also making sure the function stops when that happens
                      if(total.posts >= n |
                         is.null(next.url) |
                         ifelse(length.data > 0, (("created_time" %in% parsed$fields) & (min.time < min.since)), FALSE)
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
              
              # Advance the progress bar
              try(.progress$step(), silent=T) 
            }
           
            return(.Object)
          }
)
