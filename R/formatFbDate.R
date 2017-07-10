#' @rdname facebook.formatFbDate
#' 
#' @title 
#' Convert a Facebook datetime string into a valid R datetime
#'
#' @description
#' \code{facebook.formatFbDate} converts the date-time string returned by Facebook in a valid POSIX format.
#' 
#' @param datestring The date to convert in the character format used by Facebook.
#' 
#' @param format Set to \code{"datetime"} to convert into a POSIX value or \code{"date"}
#' to convert into a Date. Any other values will make the function returns \code{NULL}. 
#' 
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' 
#'@export
formatFbDate <- function(datestring, format="datetime") {
  
  date <- NULL
  
  if (format=="datetime"){
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")    
  }
  if (format=="date"){
    date <- as.Date(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT", origin="1970-01-01")   
  }
  return(date)
}
