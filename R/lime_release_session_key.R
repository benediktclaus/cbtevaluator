#' Release LimeSurvey Session Key
#'
#' Open a LimeSurvey session to use the API. This needs to be calles before
#' everything else. A session key can only be obtained, if there are two
#' environment variables set: `LIM_USER` (the username for your account) and
#' `LIM_PW` (your password).
#'
#' The session key is then saved as an environment variable, which is referred
#' to "under the hood".
#'
#' @return Nothing, called for side effects only.
#' @export
lime_release_session_key <- function() {
  lime_call_api(
      "release_session_key"
  )

  Sys.unsetenv("LIM_SESSION_KEY")

  cli::cli_alert_success("Sitzung erfolgreich geschlossen.")
}