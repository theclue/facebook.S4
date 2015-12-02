#' @export
FacebookPostsCollection <- function(id, 
                            token = NULL, 
                            parameters = list(), 
                            fields = "id,from.fields(id,name),message,created_time,type,link,name",
                            feed = TRUE,
                            n = getOption("facebook.maxitems")){
  
  return(new("FacebookPostsCollection", id = id, token = token, parameters = parameters, fields = fields, feed = feed, n = n))
}
