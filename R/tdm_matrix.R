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
#' #library(dplyr)
#' #library(readr)
#' #library(XML)
#'
#' #read_lines("http://www.google.co.uk") %>% 
#' #htmlParse(encoding = "UTF8") %>%
#' #scrapeML::clean_string
#'
#' @export

tdm_matrix <- function(source_dir, stemming = FALSE) {
  
  
  mycorpus <- tm::Corpus(tm::DirSource(source_dir)) %>%
    tm::tm_map(tm::stripWhitespace) %>% 
    #tm_map(tolower) %>% ## breaks it!
    tm::tm_map(tm::removeWords, tm::stopwords("english")) %>% 
    tm::tm_map(tm::removePunctuation) %>%
    tm::tm_map(tm::removeNumbers) 
  
  if (stemming == TRUE) {
   
    mycorpus <- mycorpus %>%
      tm::tm_map(stemDocument)
     
  }
  
  dtm <- tm::DocumentTermMatrix(mycorpus)
  m <- as.matrix(dtm)
  m_names <- row.names(m)
  d <- m %>%
    as.data.frame 
  
#   %>%
#     cbind(
#       document = m_names, .
#       )
#   
  return(d)
  
}