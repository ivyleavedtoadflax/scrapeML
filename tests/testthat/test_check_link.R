
library(dplyr)
library(readr)

website <- "http://www.example.com"
test_url <- "http://www.example.com/dir/file.html"

test_that(
  "check_link does not correct a valid url",
  {
    ## In the first case the output should not be different here from
    ## the input
    
    expect_match(
      check_link(test_url, website),
      test_url
    )
  }
)

test_that(
  "check_link completes relative links",
  {
    ## Output in the seconds and third cases should match the input into
    ## the first case
    
    test_url2 <- "/dir/file.html"
    
    expect_match(
      check_link(test_url2, website),
      test_url
    )
    
    test_url3 <- "dir/file.html"
    
    expect_match(
      check_link(test_url3, website),
      test_url
    )
  }
)

test_that(
  "check_link removes duplicates",
  {  
    ## Test on a vector of characters which will return the same output
    
    test_url4 <- c(
      "dir/file.html",
      "dir/file.html"
    )
    
    expect_match(
      check_link(test_url4, website),
      test_url
    )
  }
)

test_that(
  "check_link works on a vector of links",
  {
    ## Test on a vector which will return a vector
    
    test_url5 <- c(
      "dir/file.html",
      "dir/file1.html"
    )
    
    expect_equal(
      c(check_link(test_url5, website)),
      c(
        "http://www.example.com/dir/file.html",
        "http://www.example.com/dir/file1.html"
      )
    )
  }
)

test_that(
  "check_link works on a combination of links with different actions",
  {            
    ## Test on a combination
    
    test_url6 <- c(
      "http://www.example.com/dir/file2.html",
      "dir/file.html",
      "dir/file1.html"
    )
    
    expect_equal(
      c(check_link(test_url6, website)),
      c(
        "http://www.example.com/dir/file2.html",
        "http://www.example.com/dir/file.html",
        "http://www.example.com/dir/file1.html"
      )
    )
  }
)


test_that(
  "check_link works on a combination of links with different actions and duplication",
  {            
    
    ## Test on a combination with duplication
    
    test_url7 <-
      c(
        "http://www.example.com/dir/file2.html",
        "dir/file.html",
        "dir/file1.html",
        "/dir/file2.html"
      )
    
    expect_equal(
      check_link(test_url7, website),
      c(
        "http://www.example.com/dir/file2.html",
        "http://www.example.com/dir/file.html",
        "http://www.example.com/dir/file1.html"
      )
    )
  }
)

test_that(
  "check_link works when given an https",
  {            
    
    ## Test on a combination with duplication
    
    test_url7 <-
      c(
        "https://www.example.com/dir/file2.html",
        "dir/file.html",
        "dir/file1.html",
        "/dir/file2.html"
      )
    
    expect_equal(
      check_link(test_url7, website),
      c(
        "https://www.example.com/dir/file2.html",
        "http://www.example.com/dir/file.html",
        "http://www.example.com/dir/file1.html",
        "http://www.example.com/dir/file2.html"
      )
    )
  }
)

test_that(
  "check_link handles no matches",
  {
    expect_equal(
    check_link("No matching links", website),
    "No matching links"
    )
    
    }
  )

test_that(
  "check_link handles no matches",
  {
    expect_equal(
      check_link("No matching links", website),
      "No matching links"
    )
    
  }
)

test_that(
  "check_link handles javascript",
  {
    expect_equal(
      check_link("javascript:void(0);", website),
      "No matching links"
    )
    
  }
)