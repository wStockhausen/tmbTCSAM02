% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractTCSAM_ParamInfoTerminalMolt.R
\name{extractTCSAM_ParamInfoTerminalMolt}
\alias{extractTCSAM_ParamInfoTerminalMolt}
\title{Extract model terminal molt parameters info from a parsed file for a TCSAM02 model run}
\usage{
extractTCSAM_ParamInfoTerminalMolt(res, ks, verbose = FALSE)
}
\arguments{
\item{res}{- list with parsed file results}

\item{ks}{- start location index}

\item{verbose}{- flag to print info}
}
\value{
list with elements
k - index of next element to parse
lst - list of model parameters info for terminal molt parameters
}
\description{
Function to extract model terminal molt parameters info from a parsed file for a TCSAM02 model run.
}
\details{
None.
}
