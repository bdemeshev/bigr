#' bigr
#'
#' @name bigr
#' @docType package
#' @author Boris Demeshev 



.onLoad <- function(libname = find.package("bigr"), pkgname = "bigr") {
  # warning("One bear with balalaika and vodka has joined your R session!")
  library("stringi")
  library("stringr")
  library("reshape2")
  library("dplyr")
}




#' Convert string with a number in Russian tradition in numeric
#' 
#' Russian standards prescribes to use comma as a decimal separator. 
#' This function removes spaces and converts string to number.
#' 
#' @param x the string with the number
#' @return numeric the number converted from the string
#' @export
#' @examples
#' rus2num("34 345,34")
rus2num <- function(x) {
  x <- gsub(",",".",x)
  x <- gsub(" ","",x)
  return(as.numeric(x))
}

#' Convert excel numeric date encoding to date
#'
#' While reading excel files dates are sometimes replaced by their numeric codes.
#' This function recovers original dates from these codes.
#' 
#' @param x the vector of numeric date codes
#' @return the date
#' @export
#' @examples
#' excel2date(12345)
excel2date <- function(x) {
  ans <- as.Date(as.POSIXct((bir-25569)*86400, tz="GMT", origin="1970-01-01"))
  return(ans)  
}

#' Replace two or more factor levels by the first one.
#'
#' In raw data the same level of a factor variable may be encoded with errors, i.e. misprints. 
#' In this case many levels may correspond actually to one true level.
#' This function replaces all mentioned levels by the first one.
#' @param x the factor variable with misprints in level names
#' @param levels the levels that should be replaced by one level, levels[1] is used as the replacement.
#' @return correctly encoded x
#' @export
#' @examples
#' x <- c("Male","Female","male","Mle","Female")
#' unite_factors(x,c("Male","male","Mle"))
one_factor <- function(x,levels) {
  x[x %in% levels] <- levels[1]
  x <- droplevels(x)
  return(x)
}
