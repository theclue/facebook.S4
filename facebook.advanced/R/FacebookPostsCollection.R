#' @export
FacebookPostsCollection <- function(id, 
                            token, 
                            parameters = list(), 
                            fields = "id,from.fields(id,name),message,created_time,type,link,name"){
  
  return(new("FacebookPostsCollection", id = id, token = token, parameters = parameters, fields = fields))
}
