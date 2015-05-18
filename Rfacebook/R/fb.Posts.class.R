fb.Posts <- function(posts, likes, comments){
  if((is.null(posts) | !is.data.frame(posts)) | !is.data.frame(likes) | !is.data.frame(comments)){
    stop("Invalid arguments to Facebook Posts Corpus constructor")
  }
  
  me <- list(posts=posts, likes=likes, comments=comments)
  class(me) <- append(class(me),"fb.Posts")
  
  return(me)
}

#' @export
print.fb.Posts <- function(Posts) {
  cat(paste("\nA Facebook Post Posts made of", nrow(Posts$posts), "posts,", nrow(Posts$comments), "comments and", nrow(Posts$likes), "likes.\n"))
}

rbind.fb.Posts <- function(...){
  
  allargs <- list(...)
  n <- length(allargs)
  
  nms <- names(allargs)
  
  all.Posts <- do.call(rbind.fill, lapply(allargs, function(element) { 
    return(element$posts) }))
  all.Comments <- do.call(rbind.fill, lapply(allargs, function(element) { return(element$comments) }))
  all.Likes <- do.call(rbind.fill, lapply(allargs, function(element) { return(element$likes) }))
  
  return(fb.Posts(all.Posts, all.Likes, all.Comments))

}


# fb.Posts <- function(posts, likes, comments)
# {
#   
#   ## Get the environment for this
#   ## instance of the function.
#   thisEnv <- environment()
#   
#   posts <- posts
#   likes <- likes
#   comments <- comments
# 
#   me <- list(
# 
#     ## Define the accessors for the data fields.
#     getEnv = function(){ return(get("thisEnv",thisEnv))},
#     getPosts = function(){ return(get("posts",thisEnv))},
#     getLikes = function(){ return(get("likes",thisEnv))},
#     getComments = function(){return(get("comments",thisEnv))}
#     
#   )
#   
#   ## Define the value of the list within the current environment.
#   assign('this',me,envir=thisEnv)
#   
#   ## Set the name for the class
#   class(me) <- append(class(me),"fb.Posts")
#   return(me)
# }
