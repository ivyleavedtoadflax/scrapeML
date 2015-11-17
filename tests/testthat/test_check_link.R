
library(dplyr)
library(readr)

website <- "http://www.example.com"

test_url1 <- "http://www.example.com/dir/file.html"
test_url2 <- "/dir/file.html"
test_url3 <- "dir/file.html"

test_that("Check that urls are being properly corrected",
          {

            
            ## In the first case the output should not be different here from
            ## the input
                        
            expect_match(check_link(test_url1, website),
                         test_url1)
            
            ## Output in the seconds and third cases should match the input into
            ## the first case
            
            expect_match(check_link(test_url2, website),
                         test_url1)
            
            expect_match(check_link(test_url3, website),
                         test_url1)
            
          })
