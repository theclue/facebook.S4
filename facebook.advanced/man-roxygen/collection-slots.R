#' @slot id A character vector of the IDs of the item included in the Collection
#' @slot fields A character vectorwith the content of any ID in the Collection. Ie \code{id, name, created_time}...
#' @slot token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth} used to fill the Collection.
#' @slot parent A character vector of the parent IDs of the item included in the Collection or \code{NA} if the items have not inherited a parent
#' @slot parent.type A character string with the collection type of the parent ID, if available, or NA otherwise.
#' @family Facebook Collections
