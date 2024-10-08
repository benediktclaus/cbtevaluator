% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lime_call_api.R
\name{lime_call_api}
\alias{lime_call_api}
\title{Call LimeSurvey API}
\usage{
lime_call_api(
  method,
  params = NULL,
  include_session_key = TRUE,
  ignore_status = FALSE
)
}
\arguments{
\item{method}{String, a method outlined in the API documentation}

\item{params}{List, needed parameters as outlined in the API documentation.}

\item{include_session_key}{Boolean, whether sesison key should be included.
This is virtually always the case, except when the session key is
generated, which is why this option is needed. If in doubt, don't touch it.}

\item{ignore_status}{Boolean, most of the time, a status indicates an error,
but sometimes, the status message is actually useful information. This
argument switches whether to stop if a status message was submitted or if
to keep it.}
}
\value{
A `httr2` response object.
}
\description{
This is the basic functio to call the LimeSurvey API for all documented
methods (see https://api.limesurvey.org/classes/remotecontrol-handle.html).
}
\details{
For this to work, one has to set the environment variable `LIM_URL`, which is
accessible in the global configuration site at LimeSurvey after enabling the
JSON-PRC interface.
}
