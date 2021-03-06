% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_report.R
\name{get_report}
\alias{get_report}
\title{Title}
\usage{
get_report(key = Sys.getenv("apelex_api_key"), test = FALSE, report = NULL)
}
\arguments{
\item{key}{Supplied API key}

\item{test}{TRUE/FALSE. Whether to use test data (TRUE) or production data (FALSE). Defaults to FALSE.}

\item{report}{report id. Can be found at api.ap.org/v2/reports endpoint}
}
\value{
Returns dataframe with report
}
\description{
Title
}
