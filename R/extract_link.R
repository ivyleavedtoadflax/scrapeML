#' @title Extract link
#'
#' @description \code{extract_link}  .
#'
#' @param x \code{x} An object, nominally a website loadded with
#'   \code{readr::read_lines} and passed to \code{XML::htmlParse()}.
#' @param website \code{website} Where the root of website is not given in the
#'   link, append the link portion to the root given by \code{website}.
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


extract_link <- function(x, patterns, root) {
  
  ## Check the inputs. Might be good to roll this up into a tryCatch at some
  ## point.
  
  stopifnot(
    class(patterns) == "character",
    length(patterns) >= 1,
    is.na(patterns) == FALSE,
    class(x) == "character",
    length(x) == 1,
    is.na(x) == FALSE
  )
  
  ## Can take a list of x?
  ## For now, now, just one!
  
  ## If it is a link (contains http), or a file which exists and has a size
  ## greater than 0
  
  if (grepl("http[s]?://", x)|(file.exists(x) & file.size(x) > 0)) {
    
    link <- x %>%
      parse_html %>%
      priority_match(patterns)
    
    if (link[[1]] != "No matching links") {
      
      link <- check_link(x = link, website = root)
      
    } 
    
  } else {
    
    ## Expect this to be the only error!
    
    link <- "no website found"
    
  }
  
  link <- paste(link,collapse = ";")
  
  return(link)
}