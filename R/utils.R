#' @importFrom plyr rbind.fill
detailsDataToDF <- function(json, fields = NULL){
  
  if(length(json) == 0 | is.null(fields)) return(NULL)
  
  do.call(rbind.fill, lapply(json, function(item) {
    data.frame(t(unlist(
      item[which(names(item) %in%  fields)]
    )
    ), stringsAsFactors = FALSE)
  }
  )
  )
}

#' @importFrom httr GET config content
#' @importFrom rjson fromJSON
callAPI <- function(url, token){
  if ((class(token)[1]=="Token2.0") & (class(token)[5]=="StandardToken")){
    url.data <- GET(url, config(token = token, accept_encoding = "UTF-8", verbose = getOption("facebook.verbose")))
  }
  if(((class(token)[1]=="Token2.0") & (class(token)[5]=="NonStandardToken"))){
    url <- paste0(url, "&access_token=", (fromJSON(names(token$credentials))$access_token))
    url <- gsub(" ", "%20", url)
    url.data <- GET(url, verbose = getOption("facebook.verbose"))
  }
  if (class(token)[1]=="character"){
    url <- paste0(url, "&access_token=", token)
    url <- gsub(" ", "%20", url)
    url.data <- GET(url, verbose = getOption("facebook.verbose"))
  }
  if (class(token)[1]!="character" & class(token)[1]!="Token2.0"){
    stop("Error in access token. See help for details.")
  }
  content <- fromJSON(content(url.data, as = "text"))
  if (length(content$error)>0){
    stop(content$error$message)
  }
  
  # Check for permission
  if (length(content)==0) {
    stop("empty response from Facebook. You're probably not authorized to perform this kind of query. Check your permissions and try again.")
  }
  
  return(content)
}

parse.input.fields <- function(fields){
  # sub("[^(]*\\(([^()]*+(?:\\((?1)\\)[^()]*)*+)\\).*", "\\1", url, perl=T)
  # sub("(sharedposts|posts|users|likes|comments|feed|participants|reactions)\\.fields\\((.*)\\)", "\\2", fields)
  list(url = paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  ),
  fields = unique(
    unlist(strsplit(gsub('\\.(fields|type|summary|limit)\\((.*?)\\)','', 
                         sub("[^.]*+(?:\\.(?!fields\\()[^.]*)*+(?<=\\bformat|\\bvideos|\\bmembers|\\bgroups|\\bimages|\\bphotos|\\balbums|\\bmessages|\\bsenders|\\bparticipants|\\bconversations|\\bsharedposts|\\bposts|\\bcomments|\\busers|\\blikes||\\breactions|\\bfeed|\\bfriends)\\.fields\\(([^()]*+(?:\\((?1)\\)[^()]*)*+)\\)(?s:.*)", "\\1", fields, perl=T)
                         , perl = TRUE), split = ",")))
  )
}

parseFbList <- function(
  id,
  token,
  parameters = list(),
  fields = character(0)){
  
  token <- (function(){ 
    if(is.null(token) & is(id, "FacebookGenericCollection")){
      if(getOption("facebook.verbose")) message("No token specified. The token of the input collection will be used.")
      return(id@token)
    } else return(token)
  })()
  
  parsed <- parse.input.fields(fields)
  
  elements.v <- id
  
  elements.f <- rep(seq_len(ceiling(length(elements.v) / getOption("facebook.pagination"))),each = getOption("facebook.pagination"),length.out = length(elements.v))
  elements.chunks <- split(elements.v, f = elements.f)
  
  if(length(elements.chunks) > 1){
    
    return(do.call(c, 
                   unname(
                     lapply(
                       elements.chunks, function(single.chunk) {
                         parseFbList(id = single.chunk, token = token, parameters = parameters, fields = fields)
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
      "https://graph.facebook.com/v", getOption("facebook.api"), "/",
      "?metadata=0",
      "&ids=", paste0(elements.v, collapse=","),
      ifelse(length(parameters), paste0("&", query.parameters), ""),
      ifelse(length(fields), paste("&fields", parsed$url, sep="="), "")
    )
    
    if(getOption("facebook.verbose")) message("Query URL: ", url)
    
    content <- callAPI(url=url, token=token)
    
    # If ID is an atomic list, just push out the results
    if(!is(elements.v, "FacebookGenericCollection")){
      
      return(do.call(list, lapply(content, function(item){
        return(item[which(names(item) %in%  parsed$fields)])
      })))
      
    } else {
      # If id is a collection, iterate it and clean the results
      all.parents <- character(0)
      
      min.since <- ifelse(!is.null(parameters$since), as.Date(parameters$since, origin="1970-01-01"), as.Date('1970/01/01'))
      
      all.elements <- lapply(content, function(sublist) {
        page <- 0
        page.results <- list()
        
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
          
          if(!is.null(postdata$data)){
            postdata <- postdata$data
          }
          
          length.data <- length(postdata)
          
          if(length.data > 0) {
            page.results[[sublist$id]] <- postdata
            page <- page + 1
          }
          
          if(is.null(next.url) | length.data == 0){
            
            return(page.results)
          }
          
          # Graceful waiting before next call
          Sys.sleep(0.5)
          
        }
      }
      
      )
      
      names(all.elements) <- NULL
      return(do.call(c, all.elements))
    }
    
  }
}
