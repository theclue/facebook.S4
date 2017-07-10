#' @include FacebookVideosCollection.R utils.R
#' @export
#' 
#' @title 
#' Pull the list of video formats available from a set of photos
#'
#' @description
#' \code{facebook.video.formats} pulls information about the formats of a set of photos 
#' in a \code{link{FacebookPhotosCollection-class}} and push into a named list.
#' 
#' @section Valid sources:
#' \itemize{
#'  \item{\code{\link{FacebookVideosCollection-class}} will build a collection with 
#'  the formats available for the videos in the source collection.}
#' }
#'
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookAlbumsCollection}}, \code{\link{FacebookPhotosCollection}}
#'
#' @param id An existing \code{\link{FacebookPhotosCollection}}.
#' 
#' @param token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{\link{fbOAuth}}. If it's \code{NULL}, the token from \code{id} is used.
#' 
#' @param fields A character vector with the fields to get for each \code{id}.
#' 
#' @param parameters A list of parameters to be added to the Facebook Graph API query. For more information on the
#' accepted parameters, see: \url{https://developers.facebook.com/docs/graph-api/using-graph-api}.
#' 
#' @param .progress progress_bar object as defined in the plyr package.
#' By default the \code{none} progress bar is used, which prints nothing to the console. See \link[plyr]{create_progress_bar} for details.
#'
#' @return A named list with all the formats of the given photos. The name of is set to the \code{id} of the photo.
#'
#' @importFrom plyr create_progress_bar progress_none
facebook.video.formats <- function(id, 
                                   token = NULL, 
                                   parameters = list(), 
                                   fields = c("embed_html",
                                              "picture",
                                              "filter",
                                              "height",
                                              "width"),
                                   .progress = create_progress_bar()){
  
  if(length(fields)==0){
    stop("no field specified.")
  }
  
  if(is(id, "FacebookVideosCollection")){
    videos.fields <- paste0("format.fields(", paste0(fields, collapse=",", sep=""), ")", sep="")
    return(parseFbList(id = id,
                       token = token,
                       parameters = parameters,
                       fields = videos.fields))
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a video formats list starting from a ", class(id), "."))
  }
  # Atomic IDs
  stop("video formats have no ID, so you must build starting from a videos collection")
}
