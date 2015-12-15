#' @section Nesting fields:
#' Due to the network-graph nature of Facebook data model,
#' you can specify fields details for each field nesting \code{.fields()} clauses.
#'
#' For example, if you need only \code{id} and \code{source} for the \code{cover} field, this is valid among others:
#' \code{cover.fields(id,source)}.
#' 
#' Following the same philosophy, if you need only \code{id} and \code{name} for the \code{from} node
#' you can use \code{from.fields(id,name)}.
