#' @export

runQcExplorer <- function () {
  appDir <- system.file("apps", "qcExplorer", package = "samplyzer")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `samplyzer`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
