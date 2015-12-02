# detailsDataToDF <- function(json, fields = NULL){
#   
#   if(length(json) == 0 | is.null(fields)) return(NULL)
#   
#   # remove duplicates, then collate
#   collate.fields <- paste0(unique(
#     unlist(strsplit(fields, split = ","))),
#     collapse = ","
#   )
#   
#   do.call(rbind.fill,
#           lapply(lapply(json, function(item) {
#             do.call(data.frame,
#                     list(lapply((item[which(names(item) %in%  unlist(strsplit(collate.fields, split = ",")))]), function(x){
#                       if(!is.list(x) | (is.list(x) & length(x) > 0)) {
#                         print(paste("prendo", names(x)))
#                         print(length(x))
#                         return(x)
#                       } else {
#                         print("salto")
#                         return(NULL)
#                       }
#                     }), stringsAsFactors = FALSE)
#             )
#           }), function(l) {
#             l
#           })
#   )
# }

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

callAPI <- function(url, token){
  if (class(token)[1]=="Token2.0"){
    url.data <- GET(url, config(token=token))
  }	
  if (class(token)[1]=="character"){
    url <- paste0(url, "&access_token=", token)
    url <- gsub(" ", "%20", url)
    url.data <- GET(url)
  }
  if (class(token)[1]!="character" & class(token)[1]!="Token2.0"){
    stop("Error in access token. See help for details.")
  }
  content <- fromJSON(rawToChar(url.data$content))
  if (length(content$error)>0){
    stop(content$error$message)
  }
  
  # Check for permission
  if (length(content)==0) {
    stop("Empty response from Facebook. You're probably not authorized to perform this kind of query. Check your permissions and try again.")
  }
  
  return(content)
}

#'@export
formatFbDate <- function(datestring, format="datetime") {
  
  if (format=="datetime"){
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")    
  }
  if (format=="date"){
    date <- as.Date(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT", origin="1970-01-01")   
  }
  return(date)
}

parse.input.fields <- function(fields){
  # sub("[^(]*\\(([^()]*+(?:\\((?1)\\)[^()]*)*+)\\).*", "\\1", url, perl=T)
  list(url = paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  ),
  fields = unique(
    unlist(strsplit(gsub('\\.(fields|type|summary|limit)\\((.*?)\\)','', 
                         sub("(posts|users|likes|comments|feed)\\.fields\\((.*)\\)", "\\2", fields)
                         , perl = TRUE), split = ",")))
  )
}
