#' Export results for a survey
#'
#' Export results given a survey ID as retrieved by `lime_list_surveys()`.
#'
#' @param survey_id Integer, the survey ID
#' @param completion_status String, which responses should be downloaded?
#'      - `"complete"` (default)
#'      - `"incomplete"`
#'      - `"all"`
#'
#' @return A tibble with responses
#' @export
lime_export_results <- function(survey_id, completion_status = "complete") {
    req_export <- lime_call_api(
        "export_responses",
        list(
            iSurveyID = survey_id,
            sDocumentType = "csv",
            sCompletionStatus = completion_status
        )
    )

    instruments <- .get_instruments(survey_id)


    exported_tibble <- req_export |>
        .get_results() |>
        base64enc::base64decode() |>
        rawToChar() |>
        readr::read_csv2(
            name_repair = janitor::make_clean_names,
            show_col_types = FALSE,
            locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
        ) |>
        dplyr::select(-c(lastpage, startlanguage, seed, startdate, datestamp, token)) |>
        dplyr::rename(date = submitdate) |>
        dplyr::mutate(date = lubridate::as_date(date)) |>
        dplyr::arrange(date)

    list(
        data = exported_tibble,
        instruments = instruments
    )
}
