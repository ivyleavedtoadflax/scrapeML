

test_that(
  "get_url successfully handles errors",
  {
    
    ## site_url is not properly formed
    
expect_equal(get_url("http:/www.google.com", tempfile()), "invalid URL")
    
    ## site_url is a vector
    
expect_error(get_url(c("http://www.google.com","http://www.google.com"), tempfile()))
    
    ## site_url is not character
    
expect_error(get_url(1, tempfile()))
    
    ## dest is character
    
expect_error(get_url("http://www.google.com", 1))

  ## dest is signular

expect_error(get_url("http://www.google.com", c(tempfile(),tempfile())))

    }
  )