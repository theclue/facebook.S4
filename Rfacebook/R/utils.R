searchDataToDF <- function(json){
	df <- data.frame(
		from_id = unlistWithNA(json, c('from', 'id')),
		from_name = unlistWithNA(json, c('from', 'name')),
		message = unlistWithNA(json, 'message'),
		created_time = unlistWithNA(json, 'created_time'),
		type = unlistWithNA(json, 'type'),
		link = unlistWithNA(json, 'link'),
		id = unlistWithNA(json, 'id'),
		likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
		comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
		shares_count = unlistWithNA(json, c('shares', 'count')),
		stringsAsFactors=F)
	return(df)
}

newsDataToDF <- function(json){
	df <- data.frame(
		from_id = unlistWithNA(json, c('from', 'id')),
		from_name = unlistWithNA(json, c('from', 'name')),
		to_id = unlistWithNA(json, c('to', 'data', "1", 'id')),
		to_name = unlistWithNA(json, c('to', 'data', '1', 'name')),
		message = unlistWithNA(json, 'message'),
		created_time = unlistWithNA(json, 'created_time'),
		type = unlistWithNA(json, 'type'),
		link = unlistWithNA(json, 'link'),
		id = unlistWithNA(json, 'id'),
		likes_count = unlistWithNA(json, c('likes', 'summary', 'total_count')),
		comments_count = unlistWithNA(json, c('comments', 'summary', 'total_count')),
		shares_count = unlistWithNA(json, c('shares', 'count')),
		stringsAsFactors=F)
	return(df)
}

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

userDataToDF <- function(user_data, private_info){
	df <- data.frame(
		id = unlistWithNA(user_data, 'id'),
		name = unlistWithNA(user_data, 'name'),
		username = unlistWithNA(user_data, 'username'),
		first_name = unlistWithNA(user_data, 'first_name'),
		last_name = unlistWithNA(user_data, 'last_name'),
		gender = unlistWithNA(user_data, 'gender'),
		locale = unlistWithNA(user_data, 'locale'),
		category = unlistWithNA(user_data, 'category'),
		likes = unlistWithNA(user_data, 'likes'),
		picture = unlistWithNA(user_data, c('picture', 'data', 'url')),
		stringsAsFactors=F)
	if (private_info==TRUE){
		df$birthday <- unlistWithNA(user_data, 'birthday')
		df$location <- unlistWithNA(user_data, c('location', 'name'))
		df$hometown <- unlistWithNA(user_data, c('hometown', 'name'))
		df$relationship_status <- unlistWithNA(user_data, 'relationship_status')
	}
	return(df)
}

tagsDataToDF <- function(tags){
    tags <- lapply(tags, '[[', "tags")
    tags <- lapply(tags, '[[', 'data')
    tagsListToDF <- function(x){
    	if (!is.null(x)){
    	    values <- data.frame(matrix(unlist(x),ncol=2,byrow=TRUE),
    	    	stringsAsFactors=F)
    		names(values) <- c("id", "name")	
    	}
    	if (is.null(x)){
    		values <- NULL
    	}
    	return(values)
    }
    tags <- lapply(tags, tagsListToDF)
    return(tags)
}


unlistWithNA <- function(lst, field){
	if (length(field)==1){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst, '[[', field))
	}
	if (length(field)==2){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
	}
	if (field[1]=="shares"){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
		vect <- rep(0, length(lst))
		vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]]))
	}
	if (length(field)==3){
		notnulls <- unlist(lapply(lst, function(x) 
			tryCatch(!is.null(x[[field[1]]][[field[2]]][[field[3]]]), 
				error=function(e) FALSE)))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
	}
	if (length(field)==4 & field[1]=="to"){
		notnulls <- unlist(lapply(lst, function(x) 
			tryCatch(!is.null(x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]), 
				error=function(e) FALSE)))
		vect <- rep(NA, length(lst))
		vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
	}
	if (field[1] %in% c("comments", "likes") & !is.na(field[2])){
		notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
		vect <- rep(0, length(lst))
		vect[notnulls] <- unlist(lapply(lst, function(x) x[[field[1]]][[field[2]]][[field[3]]]))
	}
	return(vect)
}

searchPageDataToDF <- function(json){
  df <- data.frame(
    id = unlistWithNA(json, 'id'),
    about = unlistWithNA(json, 'about'),
    category = unlistWithNA(json, 'category'),
    description = unlistWithNA(json, 'description'),
    general_info = unlistWithNA(json, 'general_info'),
    likes = unlistWithNA(json, 'likes'),
    link = unlistWithNA(json, 'link'),
    city = unlistWithNA(json, c('location', 'city')),
    state = unlistWithNA(json, c('location', 'state')),
    country = unlistWithNA(json, c('location', 'country')),
    latitude = unlistWithNA(json, c('location', 'latitude')),
    longitude = unlistWithNA(json, c('location', 'longitude')),
    name = unlistWithNA(json, 'name'),
    talking_about_count = unlistWithNA(json, 'talking_about_count'),
    username = unlistWithNA(json, 'username'),
    website = unlistWithNA(json, 'website'),
    stringsAsFactors=F)
  return(df)
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



