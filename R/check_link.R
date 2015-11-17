#' @title Check link
#'
#' @description \code{check_link} check that a link is properly formatted .
#'
#' @param x \code{x} An object, nominally a website loadded with \code{readr::read_lines} and passed to \code{XML::htmlParse()}.
#' @param website \code{website} Where the root of website is not given in the link, append the link portion to the root given by \code{website}.
#'
#' @return A character vector of size n>=1 which represent url links.
#'
#' @examples
#'
#' #library(dplyr)
#' #library(readr)
#' #library(XML)
#'
#' #read_lines("http://www.google.co.uk") %>%
#' #htmlParse(encoding = "UTF8") %>%
#' #scrapeML::clean_string
#'
#' @export


check_link <- function(x, website) {
  ## Cycle through all of the links returned by priority match
  
  for (i in length(x)) {
    ## Name the link as y
    
    y <- x[i]
    
    ## First check that there is actually something there. If not, then skip
    ## to the next link. If just one NULL or NA, this should just drop out the
    ## end of the loop
    
    if (is.null(y) | is.na(y)) {
      y <- NA
      
      next
      
    } else {
      ## Check for http:// at the beginning of the string
      
      if (substr(1, 1, 4) != "http") {
        ## These might either be relative links with a / first, or without, so
        ## check for both possibilities.
        
        if (substr(y, 1, 1) == "/")  {
          y <- paste(website, y, sep = "")
          
          ## Other option is that there is not "/" at the beginning...
          
        } else {
          # So separate the two strings with a /
          
          y <- paste(website, y, sep = "/")
          
        }
      }
      
      x[i] <- y
      
    }
  }
  
  return(x)
}
