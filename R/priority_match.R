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
  ## Wrap this function in a trycatch to elegantly handle errors...
  
  tryCatch({
    ## Pull the full <a>...</a> tags
    
    full_tags <- XML::xpathSApply(x,'//a', XML::saveXML)
    
    ## Pull just the links from the <a></a> tags
    
    href <- XML::xpathSApply(x,'//a', fun = XML::xmlGetAttr, "href")
    
    for (i in patterns) {
      ## Go sequentially through the full tags (therefore capturing both the text
      ## and the link itself)
      
      match_index <- grep(i, full_tags, ignore.case = TRUE)
      
      ## Use a sum here because it is possible for match_index to be a vector of
      ## n > 1
      
      if (sum(match_index) > 0) {
        ## If there are more than one link with the keyword in, this will 
        ## return the entire list. They can then be handled later in another
        ## function. This may not be optimal, but is certainly an improvement
        ## on the previous version which would just return the first number
        ## in the list.
        
        #link <- href[match_index[1]][[1]]
        link <- href[match_index]
        
        ## Return the link from the match, and then break out of the function
        
        return(link)
        
        break
        
      }
      
      link <- "No matching links"
    }
  },
  error = function(cond) {
    message(paste("Error in priority_match():"))
    message(cond, "\n")
    
    ## Choose a return value in case of error
    
    ## At present just return an unhandled error, in future as warnings become
    ## apparent include better error handling.
    
    link <- "Unhandled error"
    message(link)
  },
  warning = function(cond) {
    message(paste("Warning in get_Url():"))
    message(cond, "\n")
    
    ## Choose a return value in case of warning
    
    ## At present just return an unhandled warning, in future as warnings become
    ## apparent include better error handling.
    
    link <- "Unhandled warning"
    message(link)
    
  },
  finally = {
    return(link)
    
  })
  
  ## We still need to handle the possibility that there were no matches
  ## whatsoever. The following code will only execute if a match is not found,
  ## as if a match is found, this will jump out of the function.
  
  return(NA)
  
}
