library(dplyr)
library(readr)

temp_file <- tempfile()

github_test_url <-
  "https://raw.githubusercontent.com/ivyleavedtoadflax/scrape_tests/master/test_extract_link.html"
root <- "https://raw.githubusercontent.com/ivyleavedtoadflax/"

example_df <- data_frame(
  test_url = github_test_url,
  URN = "0001") %>%
  rowwise %>%
  mutate(
    status = NA,
    file_out = temp_file,
    status = try(get_url(test_url, file_out))
  )

test_that("extract_link returns expected link string",
          {
            
            expect_true(file.exists(temp_file))
            
            expect_match(example_df$status, "success")
            
            ## Test use case of saving out file to disk first
            
            downloaded_link <- extract_link(
              example_df$file_out,
              patterns = "example",
              root = root
            )
            
            
            ## Should return link from the webpage
            
            expect_is(downloaded_link,
                      "character")
            
            expect_equal(
              length(downloaded_link),
              1)
            
            expect_match(
              downloaded_link,
              "https://raw.githubusercontent.com/ivyleavedtoadflax/scrape_tests/master/test1.html"
            )
            
          })

test_that("Test use case of pulling link directly from site",
          {
            downloaded_link1 <-
              extract_link(test_url,
                            patterns = "Example"
              )
            
            expect_is(downloaded_link1,
                      "character")
            
            expect_true(length(downloaded_link1),
                        1)
            expect_match(
              downloaded_link1,
              "https://raw.githubusercontent.com/ivyleavedtoadflax/scrape_tests/master/test1.html"
            )
            
          })
