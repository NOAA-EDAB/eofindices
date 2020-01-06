#' Capitalize first letter of string
#' 
#' Convert a string to lower case except first letter of the first word
#' 
#' @param phrase Character string. Any string of characters
#' 
#' @return string
#' 
#' @export

capitalize_first_letter <- function(phrase) {
  s <- strsplit(tolower(phrase), "\\s+")[[1]]
  if (nchar(substring(s[1],2)) > 0 ) { # multiple letters
    if (length(s) == 1) { #single word
      str <- paste0(toupper(substring(s[1],1,1)),substring(s[1],2))
    } else {
      #  str <- paste(paste0(toupper(substring(s[1],1,1)),substring(s[1],2)),s[-1],sep=" ",collapse=" ")
      str <- capture.output(cat(paste0(toupper(substring(s[1],1,1)),substring(s[1],2)),s[-1]))
    }
  } else { # single letter
    
    str <- paste0(toupper(substring(s[1],1,1)),s[-1])
  }
  
  return(str)
}