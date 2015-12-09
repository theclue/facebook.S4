#' @slot id A character vector of the IDs of the item included in the Collection
#' @slot fields A character vectorwith the content of any ID in the Collection. Ie \code{id, name, created_time}...
#' @slot token Either a temporary access token created at
#' \url{https://developers.facebook.com/tools/explorer} or the OAuth token 
#' created with \code{fbOAuth} used to fill the Collection.
#' @slot parent A character vector of the parent IDs of the item included in the Collection or \code{NA} if the items have not inherited a parent
#' @slot parent.collection The collection used to build this one, or NA if the collection was built from atomic IDs.
#' @slot type A factor vector with the type of each element of the collection. It's fed only if the collection is built with \code{metadata=TRUE}
#' @family Facebook Collections
