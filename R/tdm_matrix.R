#' @title Get a Matrix of Terms
#'
#' @description \code{tdm_matrix}
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

tdm_matrix <- function(source_dir, stemming = FALSE) {
  
  
  mycorpus <- Corpus(DirSource(source_dir)) %>%
    tm_map(stripWhitespace) %>% 
    #tm_map(tolower) %>% ## breaks it!
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) 
  
  if (stemming == TRUE) {
   
    mycorpus <- mycorpus %>%
      tm_map(stemDocument)
     
  }
  
  tdm <- TermDocumentMatrix(mycorpus)
  m <- as.matrix(tdm)
  d <- m %>% 
    as.data.frame(row.names = row.names(m))
  
  return(d)
  
}