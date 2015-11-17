#' @title Parse html
#'   
#' @description \code{parse_html} Parse a downloaded html file saved on local
#'   disk, or downloaded directly from a website.
#'   
#' @param x \code{x} Source directory in which downloaded html
#'   files are stored.
#'   
#' @return Nothing is returned from the fucntion, but for each input html file a
#'   parsed version will be produced in the dest_dir folder
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

parse_html <- function(x) {
  
  ## For some reason call readr::read_lines will fail if it is piped. Although
  ## read_lines works..?
  
  x <- readr::read_lines(x)
  
  x %>%
    XML::htmlParse(encoding = "UTF8") %>%
    return

}
