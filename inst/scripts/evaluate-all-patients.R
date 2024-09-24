rlang::check_installed("fs")
rlang::check_installed("showtext")
if (!rlang::is_installed("benelib")) {
    pak::pak("benediktclaus/benelib")
}


library(dplyr)
library(purrr)
library(fs)
library(stringr)
library(tidyr)
library(cbtevaluator)

# Create session key and get all active patient questionnaires. Commented line
# is for testing use only!
lime_get_session_key()
patient_list <- lime_list_surveys() |>
    filter(str_detect(title, ".*, .*"))
    # filter(str_detect(title, "test"))


# Extract names of all patients
patients <- patient_list |>
    pull(title)


# Get directory of the script
evaluation_directory <- readRDS("~config/path.rds")


# Create a folder for each patient an a folder for raw data
walk(patients, \(a) dir_create(path(evaluation_directory, a)))
walk(patients, \(a) dir_create(path(evaluation_directory, a, "01 Rohdaten")))
walk(patients, \(a) dir_create(path(evaluation_directory, a, "02 Abbildungen")))
walk(patients, \(a) dir_create(path(evaluation_directory, a, "03 Einladung")))


# Get patients, paths, and corresponding survey IDs
invitation_data <- patient_list |>
    mutate(
        invitation_script_path = path(evaluation_directory, title, "03 Einladung"),
        invitation_path = path(evaluation_directory, title)
    )


# Save the invitation R script file to the folder "03 Einladung"
walk2(
  invitation_data$invitation_script_path,
  invitation_data$survey_id,
  \(a, b) save_invitation_script(a, b)
)


# Copy batch scripts for invitations to patient folders
walk(
  invitation_data$invitation_path,
  \(a) file_copy(
    path_package("cbtevaluator", "scripts", "einladung-versenden.bat"),
    path(a, "einladung-versenden.bat"),
    overwrite = TRUE
  )
)


# Download data and make sure that data are available (otherwise no responses
# yet).
downloaded_data <- patient_list |>
    mutate(
        results = map(
            survey_id,
            possibly(\(a) lime_export_results(a), otherwise = "No Data"),
            .progress = "Daten werden heruntergeladen"
        ),
        data_available = map_lgl(results, is_list)
    )


# Use custom styling
benelib::use_carto_theme()


# Do the magic
evaluated_data <- downloaded_data |>
    filter(data_available) |>
    mutate(
        evaluated = map(results, lime_evaluate_instruments),
        plots = map(evaluated, lime_plot_instruments),
        filenames = map(results, get_plot_filenames)
    )


# Export raw data to "01 Rohdaten" folder
walk2(
    evaluated_data$title,
    evaluated_data$evaluated,
    \(a, b) readr::write_excel_csv2(
        get_evaluated_results(b),
        file = path(evaluation_directory, a, "01 Rohdaten", "Rohdaten.csv")
    )
)



# Get data with multiple measurements for plotting. Plots with just one point
# don't make sense.
evaluated_data_multiple <- evaluated_data |>
    mutate(
        multiple_measurements = map_lgl(results, \(a) nrow(pluck(a, "data")) > 1)
    ) |>
    filter(multiple_measurements)



# Save all plots into folder "02 Abbildungen"
evaluated_data_unnested <- evaluated_data_multiple |>
    unnest(c("plots", "filenames"))


pwalk(
    list(
        evaluated_data_unnested$title,
        evaluated_data_unnested$plots,
        evaluated_data_unnested$filenames
    ),
    \(a, b, c) benelib::save_custom_plot(
        c,
        b,
        path = path(evaluation_directory, a, "02 Abbildungen", c),
        width = 18,
        dpi = 500
    ),
    .progress = "Erstelle Abbildungen"
)



