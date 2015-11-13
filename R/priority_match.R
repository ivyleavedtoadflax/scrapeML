#' @title Priority match
#'   
#' @description \code{priority_match} This is a scraping function used to
#'   sequentially check for the presence of keywords in an links in an html 
#'   document. If the keyword of interest is found, then the function returns
#'   the link within which the keyword was found, otherwise it continues to 
#'   search for the next keyword in the vector.
#'   
#' @param x \code{x} A parsed html document. Nominally this will be the output
#' \code{XML::htmlParse(readr::read_Lines(x))}..
#' @param patterns \code{patterns} A character vector containing keywords in the
#' the order in which they should be searched for.
#'   
#' @return If a match is found, the link string is returned. If no match is found
#' then \code{NA} is returned.
#'   
#' @examples
#' 
#' library(scrapeML)
#' 
#' get_url("www.google.com",tempfile())
#' 
#' @export

priority_match <- function(x, patterns) {
  
  ## Pull the full <a>...</a> tags
  
  full_tags <- xpathSApply(x,'//a', saveXML)
  
  ## Pull just the links from the <a></a> tags
  
  href <- xpathSApply(x,'//a', fun = xmlGetAttr, "href")
  
  for (i in patterns) {
    
    ## Go sequentially through the full tags (therefore capturing both the text
    ## and the link itself)
    
    match_index <- grep(i, full_tags)
    
    ## Use a sum here because it is possible for match_index to be a vector of
    ## n > 1
    
    if (sum(match_index) > 0) {
      
      ## If there are more than one link with the keyword in, this will simply 
      ## choose the first one with match_index[1]! In future it would be better
      ## to have a second layer of matching which improves upon the initial
      ## sift.
      
      ## Number of matches could be output from here as it was in the original
      ## script.
      
      link <- href[match_index[1]][[1]]
      
      ## Return the link from the match, and then break out of the function
      
      return(link)
      
      break
      
    }
  } 
  
  ## We still need to handle the possibility that there were no matches 
  ## whatsoever. The following code will only execute if a match is not found,
  ## as if a match is found, this will jump out of the function.
  
  return(NA)
  
}
