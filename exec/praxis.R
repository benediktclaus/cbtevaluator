#!/usr/bin/env Rapp
#| name: praxis
#| title: Praxis CLI
#| description: CLI zur Steuerung der LimeSurvey-Auswertungen und Einladungen.

# Lade die benötigten Pakete (suppressMessages hält die Konsole sauber)
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(cbtevaluator))
suppressMessages(library(insight))
suppressMessages(library(purrr))
suppressMessages(library(fs))
suppressMessages(library(tidyr))

# Verhindere die automatische Erstellung von Rplots.pdf
grDevices::pdf(NULL)

# Pfad für die Auswertung auf das aktuelle Arbeitsverzeichnis setzen
evaluation_directory <- fs::path_wd()


# --- HILFSFUNKTION 2: PATIENT SUCHEN ---
find_unique_patient <- function(patient_list, search_name) {
    filtered_list <- patient_list |>
        filter(str_detect(title, regex(search_name, ignore_case = TRUE)))

    if (nrow(filtered_list) == 1) {
        return(filtered_list) # Gibt den 1-Zeilen-Dataframe zurück
    } else if (nrow(filtered_list) > 1) {
        cli::cli_alert_danger(
            "Der Suchbegriff '{search_name}' passt zu mehreren Einträgen. Bitte werde spezifischer.\n\n"
        )
        filtered_list |>
            rename("Survey ID" = survey_id, "Patient" = title) |>
            select(`Survey ID`, Patient) |>
            export_table(align = "left") |>
            cli::cli_verbatim()
        return(NULL)
    } else {
        cli::cli_alert_danger(
            "Nicht gefunden: '{search_name}' passt zu keinem Namen in der Patientenliste."
        )
        cli::cli_alert_info(
            "Tipp: Achte auf die Schreibweise oder lege eine neue Umfrage in LimeSurvey an."
        )
        return(NULL)
    }
}


# ==============================================================================
# HAUPTLOGIK: BEFEHLE DEFINIEREN
# ==============================================================================
switch(
    "",

    # --------------------------------------------------------------------------
    # BEFEHL 1: AUSWERTEN
    # --------------------------------------------------------------------------
    #| title: Patienten auswerten
    #| description: Lädt Daten herunter und erstellt Abbildungen für einen oder alle Patienten.
    auswerten = {
        #| required: false
        #| description: Ein Teil des Namens (z.B. "Mustermann"). Wenn leer, werden alle ausgewertet.
        name <- NULL

        if (!is.null(name)) {
            cli::cli_alert_info(
                "Verbinde mit LimeSurvey und suche nach '{name}'..."
            )
        } else {
            cli::cli_alert_info(
                "Verbinde mit LimeSurvey. Bereite Auswertung für ALLE Patienten vor..."
            )
        }
        cli::cli_alert_info(
            "Auswertungen werden ausgegeben in: {evaluation_directory}"
        )

        lime_get_session_key()
        on.exit(lime_release_session_key()) # Wird am Ende dieses Blocks garantiert ausgeführt

        patient_list <- lime_list_surveys() |>
            filter(str_detect(title, ".*, .*")) |>
            arrange(title)

        if (!is.null(name)) {
            # FILTERN AUF EINEN PATIENTEN
            patient_list <- find_unique_patient(patient_list, name)
            if (is.null(patient_list)) {
                return(invisible()) # Beenden, da Name nicht gefunden / nicht eindeutig
            }
            cli::cli_alert_success(
                "Eindeutiger Treffer: '{ patient_list$title }' gefunden!"
            )
        } else {
            cli::cli_alert_info(
                "Werte alle {nrow(patient_list)} Patienten aus..."
            )
        }

        patients <- patient_list |> pull(title)

        # 1. Ordnerstrukturen erstellen
        walk(patients, \(a) dir_create(path(evaluation_directory, a)))
        walk(patients, \(a) {
            dir_create(path(evaluation_directory, a, "01-rohdaten"))
        })
        walk(patients, \(a) {
            dir_create(path(evaluation_directory, a, "02-abbildungen"))
        })

        # 2. Daten herunterladen
        downloaded_data <- patient_list |>
            mutate(
                results = map(
                    survey_id,
                    possibly(
                        function(survey_id) lime_export_results(survey_id),
                        otherwise = "No Data"
                    ),
                    .progress = "Daten werden heruntergeladen"
                ),
                data_available = map_lgl(results, is_list)
            )

        # 3. Auswerten & Plotten
        benelib::use_carto_theme()

        evaluated_data <- downloaded_data |>
            filter(data_available) |>
            mutate(
                evaluated = map(results, lime_evaluate_instruments),
                plots = map(evaluated, lime_plot_instruments),
                filenames = map(results, get_plot_filenames)
            )

        cli::cli_alert_success("Daten heruntergeladen und ausgewertet.")

        # 4. Rohdaten speichern (via klassischer for-Schleife statt walk2)
        for (i in seq_len(nrow(evaluated_data))) {
            current_title <- evaluated_data$title[i]
            current_evaluated <- evaluated_data$evaluated[[i]]

            readr::write_excel_csv2(
                get_evaluated_results(current_evaluated),
                file = path(
                    evaluation_directory,
                    current_title,
                    "01-rohdaten",
                    "rohdaten.csv"
                )
            )
        }

        cli::cli_alert_success("Rohdaten erfolgreich abgespeichert.")

        # 5. Plots speichern (nur bei >1 Messzeitpunkt)
        evaluated_data_multiple <- evaluated_data |>
            mutate(
                multiple_measurements = map_lgl(results, \(a) {
                    nrow(pluck(a, "data")) > 1
                })
            ) |>
            filter(multiple_measurements)

        if (nrow(evaluated_data_multiple) > 0) {
            evaluated_data_unnested <- evaluated_data_multiple |>
                unnest(c("plots", "filenames"))

            # Komplett von pwalk auf for-Schleife umgestellt
            for (i in seq_len(nrow(evaluated_data_unnested))) {
                current_title <- evaluated_data_unnested$title[i]
                current_plot <- evaluated_data_unnested$plots[[i]]
                current_filename <- evaluated_data_unnested$filenames[i]

                # DER WORKAROUND:
                # Wir legen die Variable 'title' für den Moment der Speicherung
                # explizit in den globalen Workspace. So findet ggplot2/thematic
                # die fehlende Variable bei der Lazy Evaluation auf jeden Fall.
                assign("title", current_title, envir = globalenv())

                benelib::save_custom_plot(
                    current_filename,
                    current_plot,
                    path = path(
                        evaluation_directory,
                        current_title,
                        "02-abbildungen",
                        current_filename
                    ),
                    width = 18,
                    dpi = 500
                )

                # Den Workspace direkt wieder sauber machen
                suppressWarnings(rm("title", envir = globalenv()))
            }
            cli::cli_alert_success("Abbildungen erfolgreich gespeichert.")
        } else {
            cli::cli_alert_info(
                "Keine Verlaufs-Abbildungen erstellt (nur ein Messzeitpunkt vorhanden)."
            )
        }

        cli::cli_alert_success("Alle Auswertungen vollständig abgeschlossen!")
    },

    # --------------------------------------------------------------------------
    # BEFEHL 2: EINLADEN
    # --------------------------------------------------------------------------
    #| title: Einladung versenden
    #| description: Versendet eine LimeSurvey-Einladung an einen spezifischen Patienten.
    einladen = {
        #| description: Ein Teil des Namens des Patienten, der eingeladen werden soll.
        name <- NULL

        cli::cli_alert_info(
            "Verbinde mit LimeSurvey und suche nach '{name}' für die Einladung..."
        )

        lime_get_session_key()
        on.exit(lime_release_session_key()) # Session absichern

        patient_list <- lime_list_surveys() |>
            filter(str_detect(title, ".*, .*")) |>
            arrange(title)

        match <- find_unique_patient(patient_list, name)
        if (!is.null(match)) {
            cli::cli_alert_success(
                "Eindeutiger Treffer: '{ match$title }' gefunden!"
            )

            lime_invite_participants(match$survey_id)
        }
    }
)
