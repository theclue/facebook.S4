#' @rdname fb.Pages
fb.Pages <- function(pages, posts){
  if((is.null(pages) | !is.data.frame(pages)) | !is.data.frame(posts)){
    stop("Invalid arguments to Facebook Pages Corpus constructor")
  }
  
  me <- list(pages=pages, posts=posts)
  class(me) <- append(class(me),"fb.Pages")
  
  return(me)
}

#' @rdname print.fb.Pages
#' @export
print.fb.Pages <- function(Pages) {
  cat(paste("\nA Facebook Pages Corpus made of", nrow(Pages$pages), "pages and", nrow(Pages$posts), "posts.\n"))
}

rbind.fb.Pages <- function(...){
  
  allargs <- list(...)
  n <- length(allargs)
  
  nms <- names(allargs)
  
  all.Pages <- do.call(rbind.fill, lapply(allargs, function(element) { 
    return(element$pages) }))
  all.Posts <- do.call(rbind.fill, lapply(allargs, function(element) { return(element$posts) }))
  
  return(fb.Pages(pages=all.Pages, posts=all.Posts))

}