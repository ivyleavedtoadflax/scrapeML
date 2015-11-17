#' @title Get URL
#'   
#' @description \code{get_url} download requested url with elegant error 
#'   handling for output to a data.frame. This function is intended to populate 
#'   a data.frame with the status of the attempted download of each url, hence 
#'   will return the download statys as a character string with more friendly 
#'   error messages
#'   
#' @param x \code{x} A url as a character string.
#' @param dest \code{dest} A local file destination for the url to be saved to.
#' @param sleep \code{sleep} Time in seconds to wait at the end of each request.
#'   
#' @return A character string of either \code{"success"} or a simplified error
#'   or warning message, or \code{"unhandled error"}/\code{"unhandled warning"}.
#'   
#' @examples
#' 
#' library(scrapeML)
#' 
#' get_url("www.google.com",tempfile())
#' 
#' @export


get_url <- function(site_url, dest, sleep = 0.5) {
  
  ## Deal with any errors in the inputs
  ## Note that if this fails it causes problems!
  ## Perhaps should be handled in the tryCatch as will raise an error
  ## that can be handled elegantly.
  
  stopifnot(
    class(site_url) == "character",
    length(site_url) == 1,
    class(dest) == "character",
    length(dest) == 1
  )
  
  
  ## Now try the download with error handling
  
  tryCatch(
    {
      
      if (is.na(site_url) {
        return("No link")
      } else {
      
      ## Try to download the file
      
      download.file(
        url = site_url,
        destfile = dest,
        quiet = TRUE
      )
      
      message(paste("Successfully downloaded", site_url, "to", dest))
      
      ## Return success
      return("success")
      # write_log()
      }
    },
    error = function(cond) {
      message(paste("Error in get_Url():"))
      message(cond, "\n")
      ## Choose a return value in case of error
      
      warn <- "Unhandled error"
      
      if (grepl("unsupported URL scheme", cond)) {
        
        warn <- "invalid URL"
        
        message(warn)
      } 
      
      return(warn)
    },
    warning = function(cond) {
      message(paste("Warning in get_Url():"))
      message(cond, "\n")
      ## Choose a return value in case of warning
      
      warn <- "Unhandled warning"
      
      if (grepl("unable to resolve", cond)) {
        
        warn <- "internet not available"
        
        message(warn)
        message("Check connection and try again")
      }
      
      if (grepl("downloaded\\slength\\s\\d+\\s\\!\\=\\sreported\\slength\\s\\d+", cond)) {
        
        warn <- "success (length warning)"
        
        message(warn)
      }
      
      if (grepl("503 Service Unavailable", cond)) {
        
        warn <- "503 Error"
        
        message(warn)
      }
      
      if (grepl("404 Not Found", cond)) {
        
        warn <- "404 Error"
        
        message(warn)
      }
      
      if (grepl("The operation timed out", cond)) {
        
      warn <- "timed out"
        
      message(warn)
      }
      
      return(warn)
    },
    finally = {

      Sys.sleep(sleep)
      
    }
  )
}
