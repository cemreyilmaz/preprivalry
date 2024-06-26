% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_key.R
\name{extract_key}
\alias{extract_key}
\title{Extracting the data of key events}
\usage{
extract_key(rivdata)
}
\arguments{
\item{rivdata}{list -- the raw data}
}
\value{
data.frame -- the data of only the key-events
}
\description{
This function rearranges the binocular rivalry data which is the output of
\link{read_rivdata}, and creates a data frame including the key name, key code
and time for key-presses and key-releases.
}
\examples{
\dontrun{
key <- extract_key(read_rivdata('~/preprivalry/tests','RivalryGratings','s001','session1'))
}
}
