% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FacebookGenericCollection-class.R
\docType{class}
\name{FacebookGenericCollection-class}
\alias{FacebookGenericCollection-class}
\title{A generic collection of Facebook elements}
\description{
Connect to Facebook Graph API, get public information from a list of Facebook elements of any kind and build a \code{FacebookUsersCollection-class}
instance.
}
\details{
This collection should not be built directly, as it performs no sanity check on its content.
However, among all the available collections, it's the only one that can have mixed content inside, so
many commodity endpoint functions like \code{\link{facebook.search}} fill this as return value.
But, for the same reasons, it cannot hold fields different from id.

If you exactly know what you're doing you \emph{could} eventually build an instance of this class to perform generic queries to Graph API,
but it's not guaranteed to work and it probably won't, actually.
}
\section{Slots}{

\describe{
\item{\code{data}}{The raw data in a list form and as pulled from Facebook}

\item{\code{id}}{A character vector with the \code{id} of the items included in the collection}

\item{\code{fields}}{A character vector with the fields included for each ID in the collection. Ie. \code{id, name, created_time}...}

\item{\code{token}}{Either a temporary access token created at
\url{https://developers.facebook.com/tools/explorer} or the OAuth token 
created with \code{fbOAuth} used to fill the collection.}

\item{\code{parent}}{A character vector of the parent \code{id} of the items included in the collection or \code{NA} if the items have
no valid inheritance}

\item{\code{parent.collection}}{The collection used to build this one, or \code{NA} if the collection was built from a character vectors of \code{ids}.}

\item{\code{type}}{A character vector with the type of each element of the collection. It's set only if the collection has been built with \code{metadata=TRUE}}
}}

\seealso{
Other Facebook Collections: \code{\link{FacebookAlbumsCollection-class}},
  \code{\link{FacebookCommentsCollection-class}},
  \code{\link{FacebookConversationsCollection-class}},
  \code{\link{FacebookEventsCollection-class}},
  \code{\link{FacebookGroupsCollection-class}},
  \code{\link{FacebookLikesCollection-class}},
  \code{\link{FacebookMessagesCollection-class}},
  \code{\link{FacebookMixedCollection-class}},
  \code{\link{FacebookPagesCollection-class}},
  \code{\link{FacebookPhotosCollection-class}},
  \code{\link{FacebookPostsCollection-class}},
  \code{\link{FacebookReactionsCollection-class}},
  \code{\link{FacebookVideosCollection-class}}
}
\author{
Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
}
\concept{Facebook Collections}
\keyword{internal}
