



library(dplyr)
library(readr)

website <- "http://www.example.com"

test_that("Check that urls are being properly corrected",
          {
            ## In the first case the output should not be different here from
            ## the input
            
            test_url1 <- "http://www.example.com/dir/file.html"
            
            expect_match(check_link(test_url1, website),
                         test_url1)
            
            ## Output in the seconds and third cases should match the input into
            ## the first case
            
            test_url2 <- "/dir/file.html"
            
            expect_match(check_link(test_url2, website),
                         test_url1)
            
            test_url3 <- "dir/file.html"
            
            expect_match(check_link(test_url3, website),
                         test_url1)
            
            ## Test on a vector of characters which will return the same output
            
            test_url4 <- c("dir/file.html","dir/file.html")
            
            expect_match(check_link(test_url4, website),
                         test_url1)
            
            ## Test on a vector which will return a vector
            
            test_url5 <- c("dir/file.html","dir/file1.html")
            
            expect_equal(
              c(check_link(test_url5, website)),
              c(
                "http://www.example.com/dir/file.html","http://www.example.com/dir/file1.html"
              )
            )
            
            ## Test on a combination
            
            test_url6 <-
              c("http://www.example.com/dir/file2.html","dir/file.html","dir/file1.html")
            
            expect_equal(
              c(check_link(test_url6, website)),
              c(
                "http://www.example.com/dir/file2.html","http://www.example.com/dir/file.html","http://www.example.com/dir/file1.html"
              )
            )
            
            ## Test on a combination with duplication
            
            test_url7 <-
              c("http://www.example.com/dir/file2.html","dir/file.html","dir/file1.html","/dir/file2.html")
            
            expect_equal(
              check_link(test_url7, website),
              c(
                "http://www.example.com/dir/file2.html","http://www.example.com/dir/file.html","http://www.example.com/dir/file1.html"
              )
            )
            
          })
