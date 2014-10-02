#' bigr
#'
#' @name bigr
#' @docType package
#' @author Boris Demeshev 
#' @import stringr stringdist reshape2 dplyr zoo ggplot2 erer


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
  options(stringsAsFactors = FALSE)
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
  z %>% tolower() %>% 
    stri_trans_general(id = "Russian-Latin/BGN" ) %>% 
    str_replace_all("[[:punct:]]"," ") %>%
    str_replace_all(" +"," ") %>% str_trim() %>% return()  
}

#' Get specific word from a vector of character sentences
#'
#' This function uses the transliteration tradition where "й" goes to "y"
#' 
#' @param x the vector of cyrillic characters
#' @param num the number of word, negative numbers mean "from the end of sentence"
#' @return data frame of words with numbers
#' @export
#' @examples
#' str_word(c("привет","Маша, это я, Дубровский"))
str_word <- function(x, num=1) {  
  # little clean up
  x <- x %>% str_replace_all("[[:punct:]]"," ") %>%
    str_replace_all(" +"," ") %>% str_trim()
  
  # split into words and transform to data frame:
  d <- x %>% str_split(pattern = " ") %>% melt() %>% 
    group_by(L1) %>% mutate(n=row_number())
  
  # filter the correct word
  if (num>0) semi <- d %>% filter(n==num)
  if (num<0) semi <- d %>% group_by(L1) %>% filter(n==max(n)+1+num)
  
  # if word is missing add NA
  ans <- d %>% select(L1) %>% unique() %>% left_join(semi %>% filter(!value==""))
  # filter is needed to take care of special case when x="" and 
  # split returns "" instead of NA
  return(as.character(ans$value))
}


#' Transliterate cyrillic text
#'
#' This function uses the transliteration tradition where "й" goes to "y"
#' 
#' @param x the vector of cyrillic characters
#' @return transliterated vector
#' @export
#' @examples
#' str_translit("привет")
str_translit <- function(x) {
  return(stri_trans_general(x,"Russian-Latin/BGN"))
}

#' Change encoding of cyrillic text from "utf8" to "cp1251"
#'
#' This function changes encoding of a vector from "utf8" to "cp1251"
#' This function applies changes only to character and factor variables
#' 
#' @param x the vector of cyrillic characters
#' @return reencoded vector
#' @export
#' @examples
#' utf2cp0("привет")
utf2cp0 <- function(x) {
  ans <- x
  if (is.character(x) | is.factor(x))
    ans <- iconv(x,from="utf8",to="cp1251")
  return(ans)
}

#' Change encoding of cyrillic text from "utf8" to "cp1251"
#'
#' This function changes encoding of a vector or data.frame from "utf8" to "cp1251"
#' 
#' @param x the vector or data.frame of cyrillic characters
#' @return reencoded vector or data.frame
#' @export
#' @examples
#' str_utf2cp("привет")
str_utf2cp <- function(x) {
  ans <- x
  if (is.character(x) | is.factor(x))
    ans <- utf2cp0(x)
  if (is.data.frame(x)) 
    for (i in 1:ncol(ans)) ans[,i] <- utf2cp0(ans[,i])        
  return(ans)
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
ct_start <- function (etal_cat, add_original=FALSE) {
    ct_trivial <- data.frame(in_cat = etal_cat, out_cat = etal_cat)
    ans <- ct_trivial %>% mutate(in_cat = str_stand(in_cat)) %>% unique()
    if (add_original) ans <- rbind_list(ct_trivial,ans) %>% unique()
    return(ans)
  }

#' Find unmatched user responses given correspondance table
#'
#' This function finds unmatched user responses given correspondance table
#' 
#' @param z the vector of user responces
#' @param ct actual correspondance table
#' @return vector of unmatched user responses
#' @export
#' @examples
#' ct_unmatched(z,ct)
ct_unmatched <- function(z,ct) {
  # z - вектор пользовательских ответов
  # ct - таблица соответствий со столбцами in_cat, out_cat
  return(z[!z %in% ct$in_cat])
}

#' Create additional correspondance table from user responses and actual correspondance table
#'
#' This function 
#' 
#' @param z the vector of user responces
#' @param ct actual correspondance table
#' @param max_dist maximum Levenstein distance
#' @return additional lines for correspondance table
#' @export
#' @examples
#' ct_new_block(z,ct)
ct_new_block <- function(z,ct,max_dist=1) {
  ct$erunda <- 0
  d <- data.frame(user_ans=z,erunda=0)
  
  d_ct <- left_join(d,ct,by="erunda") # формируем все возможные пары (user_ans,in_cat)
  ct_add <- d_ct %>% 
    mutate(dist=stringdist(str_stand(user_ans),in_cat)) %>% # считаем расстояние
    filter(dist<=max_dist) %>% # отбираем те строки, где расстояние меньше max_dist
    select(in_cat=user_ans,out_cat) %>% # отбираем переменные
    unique() # удаляем дубли строк
  ct_add <- anti_join(ct_add,ct,by="in_cat") # только новые соответствия
  
  return(ct_add)
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
