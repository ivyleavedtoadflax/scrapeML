
library(dplyr)
library(readr)

file_loc <- "link_test.html"  
test_url <- "https://raw.githubusercontent.com/ivyleavedtoadflax/scrape_tests/master/link_test"

test_that(
  "get_url succeeds on known webpage",
  {
    
    foo <- get_url(test_url, file_loc) 
    
    expect_true(file.exists(file_loc))
    expect_equal(foo, "success")
  }
  
)

test_that(
  "priority_match works on known links",
  {
    
    fail <- file_loc %>% 
      read_lines %>%
      XML::htmlParse(encoding = "UTF8") %>%
      priority_match("bla")
    
    expect_match(fail, "No matching links")
    
    success <- file_loc %>% read_lines %>%
      XML::htmlParse(encoding = "UTF8") %>%
      priority_match("Keyword")
    
    expect_is(success, "character")
    expect_equal(length(success), 2)
    expect_match(success[1], "http://kw_in_link_text")
    expect_match(success[2], "http://keyword_in_href")
    
  }
  
)
