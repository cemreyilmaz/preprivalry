% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_keyevents.R
\name{plot_keyevents}
\alias{plot_keyevents}
\title{Plot all the key events}
\usage{
plot_keyevents(data, sessionNo, runNo, trialNo)
}
\arguments{
\item{data}{list or dataframe -- It contains all the information about the
experimental sessions. It can be also the data of one run or one trial
without defining the other parameters.}

\item{sessionNo}{numeric -- the session of which data you want to visual.}

\item{runNo}{numeric -- the experimental run of which data you want to visual.}

\item{trialNo}{numeric -- the trial of which data you want to visual.}
}
\value{
ggplot
}
\description{
Plot all the key events
}
\examples{
\dontrun{
data <- table(keyname,timeDown,timeUp,duration)
plot_keyevents(data,sessionNo,runNo,trialNo)}
}
