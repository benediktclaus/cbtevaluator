#' List all available surveys
#'
#' @return A tibble with all surveys and corresponding IDs.
#' @export
lime_list_surveys <- function() {
    resp_surveys <- lime_call_api(
        "list_surveys"
    ) |>
        .get_results()

    tibble::tibble(
        survey_id = purrr::map_chr(resp_surveys, "sid"),
        title = purrr::map_chr(resp_surveys, "surveyls_title")
    )
}
