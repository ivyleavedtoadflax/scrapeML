#' @title Clean String
#'
#' @description \code{clean_string} clean a string extracted using \code{XML::htmlParse()}
#'
#' @param x \code{x} An object, nominally a website loaded with \code{readr::read_lines} and passed to \code{XML::htmlParse()}.
#'
#' @return An object of class \code{character} with tags, etc removed, ready for building a document term matrix.
#'
#' @examples
#'
#' library(dplyr)
#' library(readr)
#'
#' read_lines("http://www.google.co.uk") %>% 
#' htmlParse(encoding = "UTF8") %>%
#' scrapeML::clean_string
#'
#' @export
#' @importFrom magrittr %>%

clean_string <- function(x) {
  
  ## Function uses XML::xpathApply to extract the p and div tags which are most
  ## likely to include details of interest.
  
  x %>% 
    XML::xpathApply("//p|//div", XML::saveXML) %>%
    unlist %>%
    paste(collapse = " ") %>%
    gsub("\\<(a|script|header)(.*?)\\>(.*?)\\<\\/\\1\\>", "", ., perl = TRUE) %>%
    gsub("\\<\\!(.*?)\\>(.*?)\\<\\!(.*?)\\>", "", ., perl = TRUE) %>%
    gsub("\\<\\!\\[CDATA(.*?)\\>", "", ., perl = TRUE) %>%
    gsub("\\<(.*?)\\>|\\n|\\t|nbsp;|&amp;", "", ., perl = TRUE) -> y
  
  return(y)
  
}