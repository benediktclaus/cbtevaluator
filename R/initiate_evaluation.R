#' Initiate evaluation folder
#'
#' Needed for correct file paths and script copying.
#'
#' @return Nothing, called for side effects only
#' @export
initiate_evaluation <- function() {
    # Get current directory
    evaluation_directory <- fs::path_wd()
    config_directory <- fs::path(evaluation_directory, "~config")

    fs::dir_create(config_directory)
    readr::write_rds(evaluation_directory, fs::path(config_directory, "path.rds"))


    # Copy evaluation R script to config folder
    fs::file_copy(
        fs::path(fs::path_package("cbtevaluator"), "scripts", "evaluate-all-patients.R"),
        fs::path(fs::path_wd(), "~config", "evaluate-all-patients.R"),
        overwrite = TRUE
    )


    # Copy batch file to evaluation folder
    fs::file_copy(
        fs::path(fs::path_package("cbtevaluator"), "scripts", "evaluation-aktualisieren.bat"),
        fs::path(fs::path_wd(), "evaluation-aktualisieren.bat"),
        overwrite = TRUE
    )


}
