setClass("FacebookGenericCollection",
         slots = c(id = "character",
                   fields= "character",
                   data="list"
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
            
            elements.pagination.define <- getOption("facebook.pagination")
            
            element.fields <- paste0(unique(
              unlist(strsplit(fields, split = ","))),
              collapse = ","
            )
            
            elements.v <- unique(unlist(strsplit((function(){
              if(class(id)==class(.Object)) return(id@id)
              return(id)
            })(), split = ",")))
            elements.f <- rep(seq_len(ceiling(length(elements.v) / elements.pagination.define)),each = elements.pagination.define,length.out = length(elements.v))
            elements.chunks <- split(elements.v, f = elements.f)
            
            if(length(elements.chunks) > 1){
              
              do.call(rbind,
                      lapply(c, function(single.chunk) {
                        new(class(.Object), id = id, token = token, parameters = parameters, fiels = fields)
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
                ifelse(length(element.fields), paste("&fields", element.fields, sep="="), "")
              )

              content <- callAPI(url=url, token=token)
              
              .Object@id <- names(content)
              
              .Object@fields <- unique(
                do.call(c, lapply(
                  content, function(item){ names(item)[which(names(item) %in%  unlist(strsplit(element.fields, split = ",")))] }
                )
                )
              )
              
              print(content)
              
              .Object@data <- do.call(list, lapply(content, function(item){
                return(item[which(names(item) %in%  unlist(strsplit(element.fields, split = ",")))])
                
              }))
              
            }
            return(.Object)
          }
)
