#' Invite participants to survey
#'
#' @param survey_id Integer, the survey ID as is retrieved by
#'   `lime_list_surveys()`
#' @param initial_invitation Boolean, is this the initial (very first)
#'   invitation?
#'
#' @return Nothing, called for side-effects only
#' @export
lime_invite_participants <- function(survey_id, initial_invitation = FALSE) {
    resp_invitation <- lime_call_api(
        "invite_participants",
        list(
            iSurveyID = survey_id,
            bEmail = initial_invitation
        ),
        ignore_status = TRUE
    )

    status_invitation <- resp_invitation |>
        .get_results() |>
        purrr::pluck("status")

    if (status_invitation == "-1 left to send") {
        cli::cli_alert_success("Einladung erfolgreich versendet.")
    } else {
        cli::cli_abort(status_invitation)
    }
}
