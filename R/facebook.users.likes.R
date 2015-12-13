#' @rdname facebook.users.likes
#' @export
#'
#' @title 
#' Pull all the public likes of Facebook users
#'
#' @description
#' \code{facebook.users.likes} pull information about the likes from a list of Facebook IDs and/or names of users or pages and push them into a
#' \code{\link{FacebookGenericCollection-class}} instance.
#' 
#' @details
#' This function requires the use of a OAuth token with \code{user_likes}
#' permission granted. After the introduction of version 2.0 of the Graph API,
#' only likes from users who are using the application that you used to generate the 
#' token to query the API will be returned.
#' 
#' Only the \code{id} and the \code{type} is returned in a mixed collection.
#' Then, a proper collection for each type must be built accordingly.
#'
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' @seealso \code{\link{facebook.friends}}, \code{\link{facebook.search}}, \code{\link{fbOAuth}}
#'
#' @param id A character vector or a comma-delimited string of users/pages IDs or an existing \code{\link{FacebookUsersCollection}} or \code{\link{FacebookPagesCollection}}
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{\link{fbOAuth}}. If \code{NULL} and \code{id} is a Collection, get that one instead. Otherwise, no query is performed
#' to the Facebook Graph API and an empty Collection is returned
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints nothing to the console.
#'
#' @return A collection of mixed likes in a \code{\link{FacebookMixedCollection-class}} object with the \code{id} and the \code{type} for
#' contained element.
#'
#' @examples \dontrun{
#'  load("fb_oauth")
#'  me.likes <- facebook.users.likes(users="me", token=fb_oauth)
#' }
#'
facebook.users.likes <- function(id, 
                                 token = NULL,
                                 parameters = list(),
                                 n = getOption("facebook.maxitems"), 
                                 .progress = create_progress_bar()){
  
  if(!is(id, "FacebookPagesCollection") & !is(id, "FacebookUsersCollection")){
    if(is(id, "FacebookGenericCollection")){
      stop(paste0("you cannot build a likes collection from a ", class(id), "."))
    }
    stop("id must be a collection of one of the supported types.")
  }
  
  likes.idx <- new("FacebookMixedCollection", id = id, token = token, parameters = parameters, fields = "likes.fields(id)", n = n, metadata = FALSE)
  
  return(new("FacebookMixedCollection", id = likes.idx@id, token = token, fields="id", parameters = parameters, metadata = TRUE, .progress = .progress))
  
}
