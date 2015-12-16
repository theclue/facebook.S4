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

#' @importFrom httr GET config
#' @importFrom rjson fromJSON
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
    stop("empty response from Facebook. You're probably not authorized to perform this kind of query. Check your permissions and try again.")
  }
  
  return(content)
}

parse.input.fields <- function(fields){
  # sub("[^(]*\\(([^()]*+(?:\\((?1)\\)[^()]*)*+)\\).*", "\\1", url, perl=T)
  # sub("(sharedposts|posts|users|likes|comments|feed|participants)\\.fields\\((.*)\\)", "\\2", fields)
  list(url = paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  ),
  fields = unique(
    unlist(strsplit(gsub('\\.(fields|type|summary|limit)\\((.*?)\\)','', 
                         sub("[^.]*+(?:\\.(?!fields\\()[^.]*)*+(?<=\\bformat|\\bvideos|\\bmembers|\\bgroups|\\bimages|\\bphotos|\\balbums|\\bmessages|\\bsenders|\\bparticipants|\\bconversations|\\bsharedposts|\\bposts|\\bcomments|\\busers|\\blikes|\\bfeed|\\bfriends)\\.fields\\(([^()]*+(?:\\((?1)\\)[^()]*)*+)\\)(?s:.*)", "\\1", fields, perl=T)
                         , perl = TRUE), split = ",")))
  )
}
