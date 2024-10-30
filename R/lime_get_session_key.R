#' Get LimeSurvey Session Key
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
lime_get_session_key <- function() {
    resp_session_key <- lime_call_api(
        "get_session_key",
        list(
            username = Sys.getenv("LIM_USER"),
            password = Sys.getenv("LIM_PW")
        ),
        include_session_key = FALSE
    )

    lim_session_key <- resp_session_key |>
        .get_results()

    Sys.setenv(LIM_SESSION_KEY = lim_session_key)

    cli::cli_alert_success("Session Key erfolgreich abgerufen.")
}
