#' @include FacebookGenericCollection.R
#' @export
#' 
#' @title 
#' Build a collection of Facebook users
#'
#' @description
#' Connect to Facebook Graph API, get public information from a list of Facebook users
#' and build a \code{\link{FacebookUsersCollection-class}} instance.
#' 
#' @details
#' \code{FacebookUsersCollection} is the constructor for the \code{\link{FacebookUsersCollection-class}}.
#' It returns data about users.
#' 
#' After version 2.0 of the Facebook API, only id, name, and picture are available
#' through the API as public informations. All the remaining fields will be missing unless the Application asks
#' for specific permissions. 
#' 
#' @template nesting-fields
#' 
#' @section Valid sources:
#' Instead of a character vector, one of these collections can also be passed as parameter in \code{id}:
#' \itemize{
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the friends of the users of the source collection. It assumes these users
#'  have granted the \code{user_friends} permission to the current application.}
#'  \item{\code{\link{FacebookPostsCollection-class}} will build a collection from 
#'  the authors of the posts of the source collection.}
#'  \item{\code{\link{FacebookCommentsCollection-class}} will build a collection from 
#'  the authors of the comments of the source collection.}
#'  \item{\code{\link{FacebookLikesCollection-class}} will build a collection from 
#'  the authors of the likes of the source collection.}
#'  \item{\code{\link{FacebookUsersCollection-class}} will build a collection with 
#'  the posts written on the walls of the users in the source collection.}
#'  \item{\code{\link{FacebookGroupsCollection-class}} will build a collection with 
#'  the members of the groups in the source collection.}
#'  \item{\code{\link{FacebookMixedCollection-class}} will build a collection with 
#'  only the user elements of the source collection.}
#' }
#' 
#' Be careful when binding this kind of collection starting from a \code{\link{FacebookPostsCollection}}, \code{\link{FacebookCommentsCollection}}
#' or a \code{\link{FacebookLikesCollection}}.
#' 
#' In Facebook, one can publish, comment or like acting as a user or as a page. But since users and pages have different sets of fields 
#' and you won't know in advance if the author is a page or not, the constructor of this collection would fail due to inconsitent fields.
#' 
#' To avoid this, if \code{id} is an instance of one of the aforementioned collections, an pre-serialization query is performed
#' to eventually filter out the pages and retain only the users. Finally, the real collection is built on this valid subset of user IDs only.
#' This requires more queries and, usually, more time.
#'  
#' @author
#' Gabriele Baldassarre \url{https://gabrielebaldassarre.com}
#' 
#' @seealso \code{\link{FacebookPostsCollection}},
#' \code{\link{FacebookCommentsCollection}},
#' \code{\link{FacebookLikesCollection}},
#' \code{\link{facebook.search}}
#'
#' @inheritParams FacebookGenericCollection
#' 
#' @param n If \code{id} is an iterable collection, then \code{n} is the maximum number of users to be pulled for each element of the source collection
#'  in \code{id}. It can be set to \code{Inf} to pull out any accessible user and assumes the default value from the value
#' of \code{facebook.maxitems} global option if missing. If \code{id} is not a collection or cannot be iterated, the parameter is ignored.
#'
#' @return A collection of users in a \code{\link{FacebookUsersCollection-class}} object.
#'
#' @examples \dontrun{
#' ## See examples for fbOAuth to know how token was created.
#'  load("fb_oauth")
#'  
#' ## Getting information about 9th Circle Games' Facebook Page
#'  fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", 
#'                                             "NathanNeverSergioBonelliEditore"),
#'                                      token = fb_oauth)
#'  
#' ## Getting the commenters of the latest 10 posts
#'  fb.comments <- fb.pages %>% FacebookPostsCollection(n = 10) %>%
#'      FacebookCommentsCollection(fields=c("id",
#'                                          "from.fields(id,name)"),
#'                                 n = Inf)
#'
#' ## Build a collection of users from who actually commented those posts  
#'  fb.commenting.users <- fb.comments -> FacebookUsersCollection()
#'  
#' ## Convert the collection to a data frame
#'  fb.commenting.df <- as.data.frame(fb.commenting.users)
#' }
#' 
#' @family Facebook Collection Costructors
#' @importFrom plyr create_progress_bar progress_none
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
  
  if(is(id, "FacebookGroupsCollection")){
    friends.fields <- paste0("members.fields(", paste0(fields, collapse=",", sep=""), ").limit(", real.n , ").summary(true)", sep="")
    return(new("FacebookUsersCollection", id = id, token = token, parameters = parameters, fields = friends.fields, n = n, metadata = metadata, .progress = .progress))
  }
  
  
  # Supported Collection
  if(is(id, "FacebookPostsCollection") | is(id, "FacebookCommentsCollection") | is(id, "FacebookLikesCollection")){
    indexes.df <- as.data.frame(id)
    if(!("from.id" %in% colnames(indexes.df))){
      stop(paste0("you cannot build a users collection from a ", class(id), " if the source has not the 'from.id' field inside."))
    } else {
      
      users.id <- new("FacebookMixedCollection",
                      id = unique(indexes.df$from.id),
                      token = token,
                      parameters = parameters,
                      fields = "id",
                      n = n,
                      metadata = TRUE)

      users <- new("FacebookUsersCollection",
                   id = unique(users.id[which(users.id@type=="user")]@id),
                   token = token,
                   parameters = parameters,
                   fields = e.fields,
                   n = n,
                   metadata = metadata,
                   .progress = .progress)
      
      users@parent.collection <- id
      return(users)
    }
  }
  
  # Unsupported Collections
  if(is(id, "FacebookGenericCollection")){
    stop(paste0("you cannot build a users collection starting from a ", class(id), "."))
  }
  
  # Atomic IDs
  return(new("FacebookUsersCollection",
             id = id,
             token = token,
             parameters = parameters,
             fields = e.fields,
             metadata = metadata,
             .progress = .progress))
}
