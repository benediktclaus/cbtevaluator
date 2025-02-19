#' Evaluate the MoM-DI
#'
#' Items must be named `momdi_<item-number>`. Calculated is the total sum score
#' in variable `momdi_total`
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_mom_di <- function(data) {
  .eval_sum_score(data, identifier = "momdi")
}


#' Evaluate the MoM-AI
#'
#' Items must be named `momai_<item-number>`. Calculated is the total sum score
#' in variable `momai_total`
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_mom_ai <- function(data) {
  .eval_sum_score(data, identifier = "momai")
}


#' Evaluate the WHO-5
#'
#' Items must be named `who5_<item-number>`. Calculated is the total sum score
#' in variable `who5_total`
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_who_5 <- function(data) {
  .eval_sum_score(data, identifier = "who5", multiplicator = 4)
}


#' Evaluate the OCI-R
#'
#' Items must be named `ocir_<item-number>`. Calculated is the total sum score
#' in variable `ocir_total`
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_oci_r <- function(data) {
  .eval_sum_score(data, identifier = "ocir")
}


#' Evaluate the BSL-23
#'
#' Items must be named `bsl23_<item-number>`. Calculated is the total sum score
#' in variable `bsl23_total`
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_bsl_23 <- function(data) {
  .eval_sum_score(data, identifier = "bsl23")
}


#' Evaluate the BAI
#'
#' Items must be named `bai_<item-number>`. Calculated is the total sum score
#' in variable `bai_total`
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_bai <- function(data) {
  .eval_sum_score(data, identifier = "bai")
}


#' Generic function to evaluate sums cores
#'
#' Individual items must be named `<identifier>_<item-number>` in order to
#' correctly select and sum up all relevant items. Results are appended as
#' column `<identifier>_total`
#'
#' @param data A tibble containing columns named as stated above
#' @param identifier String, the instrument identifier
#' @param multiplicator Double, multiplication factor for the raw total score
#'
#' @importFrom rlang `:=`
#'
#' @return A tibble
.eval_sum_score <- function(data, identifier, multiplicator = 1) {
  data |>
    dplyr::select(dplyr::starts_with(identifier)) |>
    dplyr::mutate(
      "{identifier}_total" := rowSums(dplyr::across(dplyr::everything())) * multiplicator
    )
}


#' Evaluate the Therapieerfolg questions
#'
#' Items must be named `therapieerfolg_1`, `arbeitausbildung_1`,
#' `freizeitsozialleben_1`, `familienleben_1`. These variables are essentially
#' just renamed and returned: That is to work in the automated evaluation of all
#' instruments.
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_therapieerfolg <- function(data) {
  data |>
    dplyr::select(therapieerfolg_1:familienleben_1) |>
    dplyr::rename_with(\(a) stringr::str_remove(a, "_1$")) |>
    dplyr::rename_with(\(a) stringr::str_glue("belastung_{a}"), .cols = arbeitausbildung:familienleben)
}


#' Evaluate the IES-R
#'
#' Items must be named `iesr_<item_number>` in order to evaluate the instrument. Subscales are calculated (intrusion, avoidance, hyperarousal).
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_ies_r <- function(data) {
  data |>
    dplyr::select(dplyr::starts_with("iesr")) |>
    dplyr::mutate(
      iesr_intrusion = rowSums(dplyr::across(dplyr::num_range("iesr_", c(1, 3, 6, 9, 14, 16, 20)))),
      iesr_vermeidung = rowSums(dplyr::across(dplyr::num_range("iesr_", c(5, 7, 8, 11, 12, 13, 17, 22)))),
      iesr_hyperarousal = rowSums(dplyr::across(dplyr::num_range("iesr_", c(2, 4, 10, 15, 18, 19, 21))))
    )
}


#' Evaluate the WAI-SR
#'
#' Items must be named `waisr_<item_number>` in order to evaluate the instrument. Subscales are calculated (bond, tasks, goals).
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_wai_sr <- function(data) {
  data |>
    dplyr::select(dplyr::starts_with("waisr_")) |>
    dplyr::mutate(
      waisr_bond = rowSums(dplyr::across(dplyr::num_range("waisr_", c(3, 5, 7, 9)))),
      waisr_tasks = rowSums(dplyr::across(dplyr::num_range("waisr_", c(1, 2, 10, 12)))),
      waisr_goals = rowSums(dplyr::across(dplyr::num_range("waisr_", c(4, 6, 8, 11))))
    )
}


#' Evaluate the BDI-II
#'
#' Items must be named `bdi_<item_number>` in order to evaluate the instrument. A total score is calculated as the item sum.
#'
#' @param data A tibble
#'
#' @family evaluators
#'
#' @return A tibble
#' @export
lime_eval_bdi_ii <- function(data) {
  data |>
    dplyr::select(dplyr::starts_with("bdi")) |>
    dplyr::rename_with(\(a) stringr::str_replace(a, "(bdi)(\\d*)", "\\1_ii_\\2")) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), \(a) readr::parse_number(a)),
      bdi_ii_total = rowSums(dplyr::across(dplyr::everything()))
    )
}



#' Get dates column from a dataframe
#'
#' @param data A tibble
#'
#' @return A tibble with ID and date
.get_dates <- function(data) {
  data |>
    dplyr::select(id, date)
}


#' Get instrument names from data
#'
#' This function searches for question groups and determines the number and
#' names of instruments based on that. Consequently, it is essential to not
#' change group names in LimeSurvey for established instruments.
#'
#' @param survey_id Survey ID provided by LimeSurvey
#'
#' @return A vector with all distinct instruments
#' @export
.get_instruments <- function(survey_id) {
  resp_groups <- lime_call_api(
    "list_groups",
    list(
      iSurveyID = survey_id
    )
  ) |>
    .get_results()


  tibble::tibble(
    group_name = purrr::map_chr(resp_groups, "group_name"),
    instrument = snakecase::to_snake_case(stringr::str_to_lower(group_name))
  )
}


#' Get a list with all necessary evaluators for a dataframe
#'
#' @param instruments A tibble with column `instrument`, preferably obtained
#'   with `.get_instruments()`
#'
#' @return A list with all evaluator function names
.get_evaluator_list <- function(instruments) {
  evaluators <- instruments |>
    dplyr::mutate(
      fun_name = paste0("lime_eval_", instrument)
    ) |>
    dplyr::pull(fun_name) |>
    as.list()

  c(
    ".get_dates",
    evaluators
  )
}


#' Evaluate all instruments automatically
#'
#' For this to work, every instrument in a dataframe that can be identified by
#' `.get_instruments()` has to have an associated evaluator function named
#' `lime_eval_<instrument-name>`
#'
#' @param exported_results A list with exported results as obtained by
#'   `lime_export_results()`
#'
#' @return A tibble with all evaluated instruments
#' @export
lime_evaluate_instruments <- function(exported_results) {
  evaluator_list <- exported_results[["instruments"]] |>
    .get_evaluator_list()

  data <- exported_results[["data"]]

  results <- purrr::map2(list(data), evaluator_list, \(a, b) purrr::exec(b, a)) |>
    purrr::list_cbind()

  list(
    data = data,
    instruments = exported_results[["instruments"]],
    results = results
  )
}
