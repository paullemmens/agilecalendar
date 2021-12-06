#' Run agilecalendar Dashboard
#'
#' @export
run_app <- function() {
  rmarkdown::run(pkgload::package_file('inst/shiny-apps', 'app.Rmd'))
}
