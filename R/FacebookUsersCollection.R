#' @include FacebookGenericCollection.R
#' 
#' @title 
#' Build a Collection of Facebook Users
#'
#' @description
#' Connect to Facebook Graph API, get public information from a list of Facebook users and build a \code{\link{FacebookUsersCollection-class}}
#' instance.
#' 
#' @details
#' \code{FacebookUsersCollection} is the constructor for the \code{\link{FacebookUsersCollection-class}} and it returns public data about users.
#' 
#' After version 2.0 of the Facebook API, only id, name, and picture are available
#' through the API as public informations. All the remaining fields will be missing unless the Application eventually asks
#' for extended permissions. 
#' 
#' Due to the network-graph nature of Facebook data model,
#' you can always specify fields details for each field eventually nesting \code{.fields()} clauses.
#'
#' For example, if you need only \code{id} and \code{source} for the \code{cover} node, this clause is valid among others:
#' \code{cover.fields(id,source)}.
#' 
#' You \emph{can} pass another \code{\link{FacebookUsersCollection}} as \code{id}. In such cases, a collection with the \strong{friends}
#' of the given users is pulled (assuming they give the \code{user_friends} permission to the App).
#' 
#' Be careful when binding this kind of collection starting from a \code{\link{FacebookPostsCollection}}, \code{\link{FacebookCommentsCollection}}
#' or a \code{\link{FacebookLikesCollection}}.
#' In Facebook, one can publish, comment or like acting as a user or as a page. But since users and pages have different sets of fields 
#' and you won't know in advance if a (commenting) user is a page or not, the constructor of this collection would fail due to inconsitent fields.
#' To avoid this, if \code{id} is an instance of one of the aforementioned collections, an pre-serialization query is performed
#' to eventually filter out the pages and retain only the users. Finally, the real collection is built on this valid subset of user IDs only.
#' This requires more queries and, usually, more time.
#'  
#' @author
#' Gabriele Baldassarre \email{gabriele@@gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPostsCollections}}, \code{\link{FacebookCommentsCollection}}, \code{\link{FacebookLikesCollection}}, \code{\link{fbOAuth}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is a Collection, then \code{n} is the maximum number of posts to be pulled for any element of the Collection in \code{id}.
#' Otherwise, the parameter is ignored. It can be set to \code{Inf} to pull out any available public post and assume the default value from the value
#' of \code{facebook.maxitems} global option if missing. 
#'
#' @return A collection of users in a \code{\link{FacebookUsersCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about 9th Circle Games' Facebook Page
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", "NathanNeverSergioBonelliEditore"), token = fb_oauth)
#'  
#' ## Getting the commenters of the latest 10 posts
#'  fb.comments <- fb.pages %>% FacebookPostsCollection(n = 10) %>% FacebookCommentsCollection(fields=c("id, "from.fields(id,name)"), n = Inf)
#'
#' ## Build a collection of users from who actually commented those posts  
#'  fb.commenting.users <- fb.comments -> FacebookUsersCollection()
#'  
#' ## Convert the collection to a data frame
#'  fb.commenting.df <- as.data.frame(fb.commenting.users)
#' }
#'
#' @export
FacebookUsersCollection <- function(id, 
                            token = NULL, 
                            parameters = list(), 
                            fields = c("id", 
                                       "name", 
                                       "first_name", 
                                       "last_name", 
                                       "gender", 
                                       "locale", 
                                       "picture.fields(url).type(large)"),
                            n = getOption("facebook.maxitems"),
                            metadata = FALSE,
                            .progress = create_progress_bar()){
  
  if(length(fields)==0 | all(nchar(fields)==0)){
    message("You've specified no fields. Only the ID will be pulled into the collection.")
    fields <- "id"
  }
  
  real.n <- (function(n, p.limit){
    if(n > p.limit) {
      return(p.limit)
    }
    else {
      return(n)
    }
  })(n, getOption("facebook.pagination"))
  
  e.fields <- paste(paste0(fields, collapse=","), "friends.summary(true).limit(0)", sep=",")
  
  if(is(id, "FacebookUsersCollection")){
    friends.fields <- paste0("friends.fields(", paste0(fields, collapse=",", sep=""), ").limit(", real.n , ").summary(true)", sep="")
    return(new("FacebookUsersCollection", id = id, token = token, parameters = parameters, fields = friends.fields, n = n, metadata = metadata, .progress = .progress))
  }
  
  # Supported Collection
  if(is(id, "FacebookPostsCollection") | is(id, "FacebookCommentsCollection") | is(id, "FacebookLikesCollection")){
    indexes.df <- as.data.frame(id)
   if(!("from.id" %in% colnames(indexes.df))){
     stop(paste0("you cannot build a Users Collection from a ", class(id), " if the latter has not the 'from.id' field inside."))
   } else {
     users.id <- FacebookGenericCollection(id = indexes.df$from.id, token = token, parameters = parameters, fields="id", metadata = TRUE)
     users <- new("FacebookUsersCollection", id = unique(users.id[which(users.id@type=="user")]@id), token = token, parameters = parameters, fields = e.fields, n = n, metadata = metadata, .progress = .progress)
     users@parent.collection <- id
     return(users)
   }
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
      stop(paste0("you cannot build a users Collection starting from a ", class(id), "."))
  }

  # Atomic IDs
  return(new("FacebookUsersCollection", id = id, token = token, parameters = parameters, fields = e.fields, metadata = metadata, .progress = .progress))
}
