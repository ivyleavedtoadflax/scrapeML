#' @title Clean String
#'
#' @description \code{clean_string} clean a string extracted using \code{XML::htmlParse()}
#'
#' @param x \code{x} An object, nominally a website loadded with \code{readr::read_lines} and passed to \code{XML::htmlParse()}.
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

parse_html <- function(x) {
  
  ## For some reason call readr::read_lines will fail if it is piped. Although
  ## read_lines works..?
  
  x <- readr::read_lines(x)
  
  x %>%
    XML::htmlParse(encoding = "UTF8") %>%
    return

}
