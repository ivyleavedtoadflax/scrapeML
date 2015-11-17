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
      readr::read_lines(source_file) %>%
        XML::htmlParse(encoding = "UTF8") %>%
        clean_string %>%
        writeLines(dest_file)
    )
  }
  
}
