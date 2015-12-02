#' A collection of valid Facebook Pages.
#'
#' @family Facebook Collections
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
