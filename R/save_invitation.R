#' Write a script that invites a certain patient
#'
#' @param survey_id Numeric, the survey ID for a given patient
#'
#' @return String
#' @export
write_invitation_script <- function(survey_id) {
    stringr::str_glue(
        "
        cbtevaluator::lime_get_session_key()
        cbtevaluator::lime_invite_participants({survey_id})
        "
    )
}


#' Save an invitation script to disk
#'
#' @param script_path String, the patient folder path
#' @inheritParams write_invitation_script
#'
#' @return Nothing, called for side effects only
#' @export
save_invitation_script <- function(script_path, survey_id) {
    # Write file as txt
    writeLines(
        write_invitation_script(survey_id),
        fs::path(script_path, "einladung-script.txt")
    )


    # Make an R out of a txt file
    file.rename(
        fs::path(script_path, "einladung-script.txt"),
        fs::path(script_path, "einladung-script.R")
    )
}
