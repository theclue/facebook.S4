#' A collection of valid Facebook Pages.
#'
#' @slot pages character vector with the ids/names of the pages of the collection.
#' @slot fields character vector with the content field of each page for the collection 
#' @slot data named list representing the raw collection
#'
#' @family API Resources
#' @name FacebookPagesCollection-class
#' @exportClass FacebookPagesCollection
#'  
#' @author Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @export
setClass("FacebookPagesCollection",
         contains = "FacebookGenericCollection",
         validity = function(object){
           # TBD
           return(TRUE)
         }
)
