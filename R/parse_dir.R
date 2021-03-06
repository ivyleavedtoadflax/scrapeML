#' @title Parse dir
#'   
#' @description \code{clean_string} clean a string extracted using
#'   \code{XML::htmlParse()}
#'   
#' @param source_dir \code{source_dir} Source directory in which downloaded html
#'   files are stored.
#' @param dest_dir \code{dest_dir} Destination directory in which parsed
#'   websites will be stored.
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

parse_dir <- function(source_dir, dest_dir) {
  
  stopifnot(
    dir.exists(source_dir)
  )
  
  if (!dir.exists(dest_dir)) {
    
    dir.create(dest_dir)
    
  }
  
  files_to_parse <- list.files(
    source_dir, 
    pattern = "*.html"
  )
  
  for (i in files_to_parse) {
    
    source_file <- file.path(source_dir, i)
    dest_file <- file.path(dest_dir, i)
    
    try(
      source_file %>% 
        parse_html %>%
        clean_string %>%
        writeLines(dest_file)
    )
  }
  
}
