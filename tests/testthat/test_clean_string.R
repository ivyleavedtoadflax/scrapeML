
require(readr)
require(dplyr)
require(XML)
library(RCurl)

test_url <- "https://raw.githubusercontent.com/ivyleavedtoadflax/scrape_tests/master/test1.html"

test_that(
  "clean_string() works on a generic website (education.gov.uk) downloaded on the fly.",
  {
  
  test_url %>%
    read_lines %>%
    htmlParse(encoding = "UTF8") -> x
  
  foo <- clean_string(x)
  
  expect_is(foo, "character")
  expect_equal(foo, "Paragraph one test text")
  
  }
)

test_that(
  "clean_string() works on a website saved to a folder (education.gov.uk).",
  {
    
    # save to temporary files folder
    
    dest <- tempfile()
    
    data <- getURL(
      test_url, 
      ssl.verifypeer = 0L, 
      followlocation = 1L
    )
    
    writeLines(data, con = dest)
    
    file.exists(dest)
    ## Expect a warning
    
    x <- dest %>% 
      read_lines %>%
      htmlParse(encoding = "UTF8")
    
    foo <- clean_string(x)
    
    expect_is(foo, "character")
    expect_equal(foo, "Paragraph one test text")
    
  }
)
