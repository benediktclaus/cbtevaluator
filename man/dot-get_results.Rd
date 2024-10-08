% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lime_call_api.R
\name{.get_results}
\alias{.get_results}
\title{Get results from a JSON formatted response}
\usage{
.get_results(response)
}
\arguments{
\item{response}{A response as generated by `lime_call_api`}
}
\value{
A response
}
\description{
Tidies the `httr2` response and pulls the results out of it.
}
