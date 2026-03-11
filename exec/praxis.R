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

# Verhindere die automatische Erstellung von Rplots.pdf
pdf(NULL)

# Pfad für die Auswertung auf das aktuelle Arbeitsverzeichnis setzen
evaluation_directory <- fs::path_wd()


# --- HILFSFUNKTION 1: AUSWERTUNG ---
evaluate_single_patient <- function(p_name, p_survey_id, eval_dir) {
    cli::cli_alert_info("Starte Evaluation für '{p_name}'...")

    # 1. Ordnerstruktur erstellen
    dir_create(path(eval_dir, p_name))
    dir_create(path(eval_dir, p_name, "01-rohdaten"))
    dir_create(path(eval_dir, p_name, "02-abbildungen"))

    # 2. Daten herunterladen
    results <- possibly(
        \(a) lime_export_results(a),
        otherwise = "No Data"
    )(p_survey_id)

    if (is.list(results)) {
        benelib::use_carto_theme()

        # 3. Auswerten
        evaluated <- lime_evaluate_instruments(results)
        plots <- lime_plot_instruments(evaluated)
        filenames <- get_plot_filenames(results)

        # 4. Rohdaten speichern
        readr::write_excel_csv2(
            get_evaluated_results(evaluated),
            file = path(eval_dir, p_name, "01-rohdaten", "rohdaten.csv")
        )

        # 5. Plots speichern (nur wenn >1 Messzeitpunkt)
        if (nrow(pluck(results, "data")) > 1) {
            walk2(
                plots,
                filenames,
                \(p, f) {
                    benelib::save_custom_plot(
                        f,
                        p,
                        path = path(eval_dir, p_name, "02-abbildungen", f),
                        width = 18,
                        dpi = 500
                    )
                }
            )
        } else {
            cli::cli_alert_info(
                "Nur ein Messzeitpunkt für '{p_name}'. Es werden keine Verlaufs-Abbildungen erstellt."
            )
        }

        cli::cli_alert_success(
            "Evaluation für '{p_name}' erfolgreich abgeschlossen!"
        )
    } else {
        cli::cli_alert_warning(
            "Bisher keine ausgefüllten Fragebögen für '{p_name}' vorhanden."
        )
    }
}


# --- HILFSFUNKTION 2: PATIENT SUCHEN ---
# Diese Funktion übernimmt die if/else Logik für die Namenssuche,
# da wir diese nun bei 'auswerten' UND bei 'einladen' benötigen.
find_unique_patient <- function(patient_list, search_name) {
    filtered_list <- patient_list |>
        filter(str_detect(title, regex(search_name, ignore_case = TRUE)))

    if (nrow(filtered_list) == 1) {
        return(filtered_list) # Gibt die Zeile mit dem eindeutigen Treffer zurück
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

        if (is.null(name)) {
            # ALLE AUSWERTEN
            cli::cli_alert_info(
                "Werte alle {nrow(patient_list)} Patienten aus..."
            )
            walk2(
                patient_list$title,
                patient_list$survey_id,
                \(n, id) evaluate_single_patient(n, id, evaluation_directory),
                .progress = TRUE
            )
            cli::cli_alert_success(
                "Alle Auswertungen vollständig abgeschlossen!"
            )
        } else {
            # EINZELNEN AUSWERTEN
            match <- find_unique_patient(patient_list, name)
            if (!is.null(match)) {
                cli::cli_alert_success(
                    "Eindeutiger Treffer: '{ match$title }' gefunden!"
                )
                evaluate_single_patient(
                    match$title,
                    match$survey_id,
                    evaluation_directory
                )
            }
        }
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
            cli::cli_alert_info("Versende Einladung...")

            lime_invite_participants(match$survey_id)
        }
    }
)
