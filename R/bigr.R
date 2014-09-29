#' bigr
#'
#' @name bigr
#' @docType package
#' @author Boris Demeshev 



.onLoad <- function(libname = find.package("bigr"), pkgname = "bigr") {
  # warning("One bear with balalaika and vodka has joined your R session!")
  library("stringi")
  library("stringr")
  library("stringdist")
  library("reshape2")
  library("dplyr")
  library("zoo")
  
  library("ggplot2")
  library("erer")
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




#' Standartize character vector 
#' 
#' Standartize means: trimming white space at the end and in the beginning,
#' elimination of punctiation, transliteration, removing capitalization
#' 
#' @param x the character vector
#' @return standartized character vector
#' @export
#' @examples
#' str_stand("пРивет!")
str_stand <- function(z) {
  z %>% tolower() %>% str_trim() %>% 
    stri_trans_general(id = "Russian-Latin/BGN" ) %>% 
    str_replace_all("[[:punct:]]"," ") %>%
    str_replace_all(" +"," ") %>% return()  
}




#' Transliterate cyrillic text
#'
#' This function uses the transliteration tradition where "й" goes to "y"
#' 
#' @param x the vector of cyrillic characters
#' @return transliterated vector
#' @export
#' @examples
#' translit("привет")
translit <- function(x) {
  return(stri_trans_general(x,"Russian-Latin/BGN"))
}


#' Create base correspondance table from vector of etalon cathegory names
#'
#' This function creates basic correspondance table from vector of etalon cathegory names
#' 
#' @param x the vector of etalon cathegory names
#' @return data.frame with basic correspondance table 
#' @export
#' @examples
#' ct_start(c("Iphone","Samsung","HTC"))
ct_start <- function(etal_cat) {
  ct <- data.frame(in_cat=etal_cat,out_cat=etal_cat)
  ct_add <- ct %>% mutate(in_cat=str_stand(in_cat))
  ct <- rbind_list(ct,ct_add) %>% unique() 
  return(ct)
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
