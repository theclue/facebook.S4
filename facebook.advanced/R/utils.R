insightsDataToDF <- function(json, values, metric){
  if (metric!="page_fans_country"){
    df <- data.frame(
      id = unlistWithNA(json, 'id'),
      metric_name = unlistWithNA(json, 'name'),
      period = unlistWithNA(json, 'period'),
      values = unlistWithNA(values, 'value'),
      end_time = unlistWithNA(values, 'end_time'),
      stringsAsFactors=F)
  }
  if (metric=="page_fans_country"){
    # values for country-level variables
    countries <- lapply(json[[1]]$values, function(x) names(x$value))
    values <- lapply(json[[1]]$values, function(x) x$value)
    end_times <- unlist(lapply(json[[1]]$values, function(x) x$end_time))
    end_times <- unlist(lapply(1:length(countries), function(x) rep(end_times[[x]], length(countries[[x]]))))
    
    df <- data.frame(
      id = unlistWithNA(json, 'id'),
      metric_name = unlistWithNA(json, 'name'),
      period = unlistWithNA(json, 'period'),
      country = unlist(countries),
      values = unlist(values),
      end_time = unlist(end_times),
      stringsAsFactors=F)
  }
  return(df)
}

detailsDataToDF <- function(json, fields = NULL){
  
  if(length(json) == 0 | is.null(fields)) return(NULL)
  
  # remove duplicates, then collate
  collate.fields <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )
  
  do.call(rbind.fill,
          lapply(lapply(json, function(item) {
            do.call(data.frame,
                    list(item[which(names(item) %in%  unlist(strsplit(collate.fields, split = ",")))],
                         stringsAsFactors = FALSE),
            )
          }), function(l) {
            l
          })
  )
}


summaryDataToDF <- function(json, fields = NULL){
  
  if(length(json) == 0 | is.null(fields)) return(NULL)
  
  fields <- unique(
    unlist(strsplit(fields, split = ",")))
  
  s <- do.call(rbind.fill,
               lapply(lapply(json, function(item) {
                 data.frame(
                   likes.count = ifelse(is.list(item$likes),
                                        item$likes$summary$total_count, 0),
                   comments.count = ifelse(is.list(item$comments),
                                           item$comments$summary$total_count, 0),
                   shares.count = ifelse(!is.null(item$shares),
                                         item$shares$count, 0)           
                 )
               }), function(l) {
                 l
               })
  )
  
  return(s[,fields])
  
}

callAPI <- function(url, token){
  if (class(token)[1]=="config"){
    url.data <- GET(url, config=token)
  }
  if (class(token)[1]=="Token2.0"){
    url.data <- GET(url, config(token=token))
  }	
  if (class(token)[1]=="character"){
    url <- paste0(url, "&access_token=", token)
    url <- gsub(" ", "%20", url)
    url.data <- GET(url)
  }
  if (class(token)[1]!="character" & class(token)[1]!="config" & class(token)[1]!="Token2.0"){
    stop("Error in access token. See help for details.")
  }
  content <- fromJSON(rawToChar(url.data$content))
  if (length(content$error)>0){
    stop(content$error$message)
  }	
  return(content)
}

getTokenVersion <- function(token){
  
  if (!is.na(class(token)[4])){
    tkversion <- class(token)[4]
  }
  if (is.na(class(token)[4])){
    error <- tryCatch(callAPI('https://graph.facebook.com/pablobarbera', token),
                      error = function(e) e)
    if (inherits(error, 'error')){
      tkversion <- 'v2'
    }
    if (!inherits(error, 'error')){
      tkversion <- 'v1'
    }
  }
  return(tkversion)
  
}


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
  
  list(url = paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  ),
  fields = paste0(unique(
    unlist(strsplit(gsub('\\.fields\\((.*?)\\)','', 
                         gsub('\\.type\\((.*?)\\)','', fields, perl = TRUE)
                         , perl = TRUE), split = ","))),
    collapse = ","
  )
  )
}
