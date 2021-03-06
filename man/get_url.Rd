% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_url.R
\name{get_url}
\alias{get_url}
\title{Get URL}
\usage{
get_url(site_url, dest, sleep = 0.5)
}
\arguments{
\item{dest}{\code{dest} A local file destination for the url to be saved to.}

\item{sleep}{\code{sleep} Time in seconds to wait at the end of each request.}

\item{x}{\code{x} A url as a character string.}
}
\value{
A character string of either \code{"success"} or a simplified error
  or warning message, or \code{"unhandled error"}/\code{"unhandled warning"}.
}
\description{
\code{get_url} download requested url with elegant error 
  handling for output to a data.frame. This function is intended to populate 
  a data.frame with the status of the attempted download of each url, hence 
  will return the download statys as a character string with more friendly 
  error messages
}
\examples{

#library(scrapeML)

#get_url("www.google.com",tempfile())

}

