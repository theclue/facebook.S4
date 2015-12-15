#' @slot id A character vector with the \code{id} of the items included in the collection
#' @slot fields A character vector with the fields included for each ID in the collection. Ie. \code{id, name, created_time}...
#' @slot token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth} used to fill the collection.
#' @slot parent A character vector of the parent \code{id} of the items included in the collection or \code{NA} if the items have
#' no valid inheritance
#' @slot parent.collection The collection used to build this one, or \code{NA} if the collection was built from a character vectors of \code{ids}.
#' @slot type A character vector with the type of each element of the collection. It's set only if the collection has been built with \code{metadata=TRUE}
#' @family Facebook Collections
