% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_html.R
\name{parse_html}
\alias{parse_html}
\title{Parse html}
\usage{
parse_html(x)
}
\arguments{
\item{x}{\code{x} Source directory in which downloaded html
files are stored.}
}
\value{
Nothing is returned from the fucntion, but for each input html file a
  parsed version will be produced in the dest_dir folder
}
\description{
\code{parse_html} Parse a downloaded html file saved on local
  disk, or downloaded directly from a website.
}
\examples{

#library(dplyr)
#library(readr)
#library(XML)

#read_lines("http://www.google.co.uk") \%>\% 
#htmlParse(encoding = "UTF8") \%>\%
#scrapeML::clean_string

}

