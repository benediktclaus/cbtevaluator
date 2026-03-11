#' Installiere cbtevaluator CLI-Anwendungen
#'
#' @inheritDotParams Rapp::install_pkg_cli_apps -package -lib.loc
#' @export
install_cbtevaluator_cli <- function(...) {
  Rapp::install_pkg_cli_apps(package = "cbtevaluator", lib.loc = NULL, ...)
}
