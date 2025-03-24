#' Plot evaluated results
#'
#' The supplied data must contain the column that is selected by the `variable`
#' argument.
#'
#' @param data A tibble
#' @param variable The variable to be plotted
#' @param ylab String, y-axis label
#' @param title String, plot tile
#' @param xlab String, x-axis label
#' @param subtitle String, subtitle
#' @param ymax Double, the maximum y-axis value to include in the axis
#'
#' @return A `ggplot2` object
.plot_single_results <- function(data,
                                 variable,
                                 ylab = "Wert",
                                 title = "Instrumentenname",
                                 subtitle,
                                 xlab = "Datum",
                                 ymax = NULL) {
  ggplot2::ggplot(data, ggplot2::aes(date, {{ variable }})) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_date(labels = scales::label_date_short()) +
    ggplot2::expand_limits(y = c(0, ymax)) +
    ggplot2::labs(x = xlab, y = ylab, title = title, subtitle = subtitle)
}


#' Plot results of the MoM-DI
#'
#' The data supplied must contain the column `momdi_total`.
#'
#' @inheritParams .plot_single_results
#' @param ... Additional arguments passed to .plot_single_results
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_mom_di <- function(data, ...) {
  .plot_single_results(
    data,
    variable = momdi_total,
    ylab = "Summenwert",
    title = "Mind over Mood-Depression Inventory",
    subtitle = "Depressivit\u00e4t",
    ...
  )
}


#' Plot results of the MoM-AI
#'
#' The data supplied must contain the column `momai_total`.
#'
#' @inheritParams .plot_single_results
#' @param ... Additional arguments passed to .plot_single_results
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_mom_ai <- function(data, ...) {
  .plot_single_results(
    data,
    variable = momai_total,
    ylab = "Summenwert",
    title = "Mind over Mood-Anxiety Inventory",
    subtitle = "Angst",
    ...
  )
}


#' Plot results of the WHO-5
#'
#' The data supplied must contain the column `who5_total`.
#'
#' @inheritParams .plot_single_results
#' @param ... Additional arguments passed to .plot_single_results
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_who_5 <- function(data, ...) {
  .plot_single_results(
    data,
    variable = who5_total,
    ylab = "Gesamtwert",
    title = "WHO-5",
    subtitle = "Fragebogen zum Wohlbefinden",
    ymax = 100,
    ...
  )
}


#' Plot results of the OCI-R
#'
#' The data supplied must contain the column `ocir_total`.
#'
#' @inheritParams .plot_single_results
#' @param ... Additional arguments passed to .plot_single_results
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_oci_r <- function(data, ...) {
  .plot_single_results(
    data,
    variable = ocir_total,
    ylab = "Gesamtwert",
    title = "OCI-R",
    subtitle = "Fragebogen zur Erfassung von Zwangssymptomen",
    ...
  )
}


#' Plot results of the BSL-23
#'
#' The data supplied must contain the column `bsl23_total`.
#'
#' @inheritParams .plot_single_results
#' @param ... Additional arguments passed to .plot_single_results
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_bsl_23 <- function(data, ...) {
  .plot_single_results(
    data,
    variable = bsl23_total,
    ylab = "Gesamtwert",
    title = "BSL-23",
    subtitle = "Borderline Symptom Liste 23",
    ...
  )
}


#' Plot results of the BSL-23
#'
#' The data supplied must contain the column `bai_total`.
#'
#' @inheritParams .plot_single_results
#' @param ... Additional arguments passed to .plot_single_results
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_bai <- function(data, ...) {
  .plot_single_results(
    data,
    variable = bai_total,
    ylab = "Gesamtwert",
    title = "BAI",
    subtitle = "Fragebogen zur Erfassung von Angstsymptomen",
    ...
  )
}


#' Plot results of the EDE-Q
#'
#' The data supplied must contain the columns `Restraint`, `Eating Concern`,
#' `Weight Concern`, `Shape Concern`, `Global`, `Häufigkeit viel Nahrung`,
#' `Häufigkeit Essanfälle`, `Tage mit Essanfällen`, `Häufigkeit Erbrechen`,
#' `Häufigkeit Abführmittel`, `Häufigkeit Sport`
#'
#' @inheritParams .plot_single_results
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_ede_q <- function(data) {
    data_subscale <- data |>
        dplyr::select(date, "Restraint":"Global") |>
        tidyr::pivot_longer(
            cols = -date,
            names_to = "subscale"
        )

    data_item <- data |>
        dplyr::select(date, "H\u00e4ufigkeit viel Nahrung":"H\u00e4ufigkeit Sport") |>
        tidyr::pivot_longer(
            cols = "H\u00e4ufigkeit viel Nahrung":"H\u00e4ufigkeit Sport",
            names_to = "item"
        )

    plot_subscale <- ggplot2::ggplot(data_subscale, ggplot2::aes(date, value, color = subscale)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::expand_limits(y = c(0, 6)) +
        ggplot2::labs(x = "Datum", y = "Mittelwert", color = "Subskala")

    plot_item <- ggplot2::ggplot(data_item, ggplot2::aes(date, value)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(~ item, ncol = 2) +
        ggplot2::labs(x = "Datum", y = "Häufigkeit")

    plot_list <- list(
        plot_subscale,
        plot_item
    )

    patchwork::wrap_plots(plot_list) +
        patchwork::plot_layout(axes = "collect", guides = "collect", ncol = 1)
}


#' Plot Therapieerfolg
#'
#' The data supplied must contain columns `therapieerfolg`,
#' `belastung_arbeitausbildung`, `belastung_freizeitsozialleben`,
#' `belastung_familienleben`.
#'
#' @param data A tibble
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_therapieerfolg <- function(data) {
  tidied_data <- data |>
    dplyr::select(date, therapieerfolg:belastung_familienleben) |>
    tidyr::pivot_longer(
      cols = -date,
      names_to = "outcome"
    ) |>
    dplyr::mutate(
      facet = dplyr::case_when(
        outcome == "therapieerfolg" ~ "therapieerfolg",
        stringr::str_detect(outcome, "belastung") ~ "belastung"
      ),
      outcome = stringr::str_remove(outcome, "belastung_"),
      outcome = snakecase::to_title_case(outcome),
      outcome = dplyr::case_match(
        outcome,
        "Arbeitausbildung" ~ "Arbeit/Ausbildung",
        "Freizeitsozialleben" ~ "Freizeit/Sozialleben",
        "Familienleben" ~ "Familienleben/H\u00e4uslich"
      )
    )


  # Filter data for different patches
  data_belastung <- tidied_data |>
    dplyr::filter(facet == "belastung")

  data_therapieerfolg <- tidied_data |>
    dplyr::filter(facet == "therapieerfolg")


  # Create both patches
  plot_belastung <- ggplot2::ggplot(data_belastung, ggplot2::aes(date, value, color = outcome)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    ggplot2::scale_x_date(labels = scales::label_date_short()) +
    ggplot2::expand_limits(y = c(0, 100)) +
    ggplot2::labs(x = "Datum", y = "Belastung", color = NULL, title = "Eingesch\u00e4tzte Belastung")


  plot_therapieerfolg <- ggplot2::ggplot(data_therapieerfolg, ggplot2::aes(date, value)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%")) +
    ggplot2::scale_x_date(labels = scales::label_date_short()) +
    ggplot2::expand_limits(y = c(-100, 100)) +
    ggplot2::labs(x = "Datum", y = "Therapieerfolg", title = "Eingesch\u00e4tzter Therapieerfolg")

  plot_list <- list(
    plot_belastung,
    plot_therapieerfolg
  )

  patchwork::wrap_plots(plot_list) +
    patchwork::plot_layout(axes = "collect", guides = "collect")
}


#' Plot results of the IES-R
#'
#' The data supplied must contain the columns `iesr_intrusion`, `iesr_vermeidung`, and `iesr_hyperarousal`.
#'
#' @param data A tibble
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_ies_r <- function(data) {
  tidied_data <- data |>
    dplyr::select(date, iesr_intrusion, iesr_vermeidung, iesr_hyperarousal) |>
    tidyr::pivot_longer(
      cols = -date,
      names_to = "subscale",
      values_to = "score",
      names_prefix = "iesr_"
    ) |>
    dplyr::mutate(
      subscale = snakecase::to_title_case(subscale)
    )

  ggplot2::ggplot(tidied_data, ggplot2::aes(date, score)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ subscale) +
    ggplot2::labs(x = "Datum", y = "Summenwert", title = "Impact of Event Scale - Revised", subtitle = "Reaktion auf belastende Ereignisse")
}


#' Plot results of the WAI-SR
#'
#' The data supplied must contain the columns `waisr_bond`, `waisr_tasks`, `waisr_goals`.
#'
#' @param data A tibble
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_wai_sr <- function(data) {
  tidied_data <- data |>
    dplyr::select(date, waisr_bond, waisr_tasks, waisr_goals) |>
    tidyr::pivot_longer(
      cols = -date,
      names_to = "subscale",
      values_to = "score",
      names_prefix = "waisr_"
    ) |>
    dplyr::mutate(
      subscale = snakecase::to_title_case(subscale),
      subscale = forcats::fct_relevel(subscale, "Bond", "Tasks")
    )

  ggplot2::ggplot(tidied_data, ggplot2::aes(date, score)) +
    ggplot2::geom_hline(yintercept = c(4, 20), lty = "dashed") +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ subscale) +
    ggplot2::expand_limits(y = c(0, 20)) +
    ggplot2::labs(x = "Datum", y = "Summenwert", title = "Working Alliance Inventory - Short Revised", subtitle = "Einsch\u00e4tzung der Therapiebeziehung")
}


#' Plot results of the BDI-II
#'
#' The data supplied must contain the column `bdi_ii_total`.
#'
#' @inheritParams .plot_single_results
#' @param ... Additional arguments passed to .plot_single_results
#'
#' @family plotters
#'
#' @return A `ggplot2` object
#' @export
lime_plot_bdi_ii <- function(data, ...) {
  .plot_single_results(
    data,
    variable = bdi_ii_total,
    ylab = "Summenwert",
    title = "Beck Depression Inventory - II",
    subtitle = "Depressivit\u00e4t",
    ...
  )
}


.get_plotter_list <- function(instruments) {
  plotters <- instruments |>
    dplyr::mutate(
      fun_name = paste0("lime_plot_", instrument)
    ) |>
    dplyr::pull(fun_name) |>
    as.list()

  plotters
}


#' Plot all instruments
#'
#' @inheritParams lime_evaluate_instruments
#' @param path String, path to save individual figures to
#'
#' @return A list with `ggplot2` plots
#' @export
lime_plot_instruments <- function(exported_results, path) {
  plotter_list <- exported_results[["instruments"]] |>
    .get_plotter_list()

  data <- exported_results[["results"]]

  purrr::map2(list(data), plotter_list, \(a, b) purrr::exec(b, a))
}



#' Get filenames for each plot
#'
#' @param results_object The result object created by `lime_export_results()`
#'
#' @return A list of strings
#'
#' @export
get_plot_filenames <- function(results_object) {
  results_object |>
    purrr::pluck("instruments") |>
    dplyr::mutate(
      filename = stringr::str_glue("{ instrument }.png")
    ) |>
    dplyr::pull(filename)
}


#' Pluck data from an evaluated object
#'
#' @param evaluated_object The evaluated object created by
#'   `lime_evaluate_instruments()`
#'
#' @return A tibble
#'
#' @export
get_evaluated_results <- function(evaluated_object) {
  evaluated_object |>
    purrr::pluck("results")
}
