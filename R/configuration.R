#' @title Parse Agile Events Overview
#'
#' @description The agile events are part of the configuration file that
#'    is parsed using the r-yaml package. Because (eventually) the agile
#'    events are to be calculated relative to the agile cadence, this
#'    separate function is used.
#'
#' @param event_list Part of a structure from parsing the configuration
#'    file using [`yaml::yaml.load_file()`].
#'
#' @importFrom dplyr "%>%"
#'
parse_agile_events <- function(event_list) {
  d <- stringr::str_sub(deparse(substitute(event_list)), 5, -1)

  res <- purrr::map2_dfr(event_list, names(event_list),
                  ~ create_marker_dfr(ev = .x, ev_name = .y, type = d))

  if (!'dstamp' %in% names(res)) {
    res <- res %>% dplyr::mutate(dstamp = NA_integer_)
  }
  res <- res %>% dplyr::mutate(dstamp = lubridate::ymd(dstamp))

  return(res)
}

#' @title Load Configuration File
#'
#' @description Load the configuration file for the generator.
#'
#' @param cfgfile A path to a configuration file.
#' @param cfg_content A string that contains the actual content of the
#'    calendar configuration. This is mostly to support the dashboard.
#'    Defaults to NULL and overrides `cfgfile` if both are specified
#'    simultaneously.
#'
#' @importFrom dplyr "%>%"
#'
#' @export
read_config <- function(cfgfile = NULL, cfg_content = NULL) {

  if (is.null(cfgfile) && is.null(cfg_content)) stop('cfgfile and cfg_content cannot both be null')

  if (!is.null(cfg_content)) {
    cfg <- yaml::yaml.load(cfg_content)
  } else {
    cfg <- yaml::yaml.load_file(cfgfile)
  }

  cfg$agile_events <- parse_agile_events(cfg$agile_events)
  cfg$markers <- parse_agile_events(cfg$markers)

  return(cfg)
}


#' @title Copy Default Configuration
#'
#' @description Make a copy of the default configuration to a location
#'    outside of the package.
#'
#' @param path Path where to store the copy.
#'
#' @return Invisibly, checking whether copying was successful.
#'
#' @export
copy_default_config <- function(path) {

  val <- file.copy(from = pkgload::package_file('inst/default_config.yml'),
                   to = path,
                   copy.mode = FALSE)

  if (!any(val)) stop('Unable to create copy')

  return(invisible())
}


#' @title Agile Calendar Configuration
#'
#' @description The Agile calendar is defined and constructed using the
#'    values inside the configuration YAML. A personal copy can be made
#'    using the [`copy_default_config()`] function.
#'
#' @format
#'
#' The YAML (file) with the configuration and markers comprises
#' three sections: `configuration`, `agile_events`, and `markers` of
#' which the latter two have a somewhat interchangeable role.
#'
#' `configuration` should have the following items below it (without
#' deeper hierarchies).
#'
#' \describe{
#'   \item{`project`}{A simple label to mark the name of a project or
#'     something else that might be relevant to display. Currently
#'     used more as mnemonic that actually used in the calendar.}
#'   \item{`year`}{Simple number that is used to prefix the descriptive
#'     labels for the increments and iterations (e.g. PI 22.x.y and
#'     22 here then is the value for `year`.}
#'   \item{`year_start`}{Date of the first Monday of the (next) Agile
#'     year. This value is converted to a date stamp that is used to
#'     offset all other calculations for laying out the Agile year
#'     and calendar. Should be presented as YYYY-mm-dd.}
#'   \item{`increment_length`, `iteration_length`}{The duration in
#'     weeks of an increment (a quarter) and an iteration (a sprint).}
#'   \item{`ip_length`}{The duration of the Innovation and Planning
#'     iteration (in weeks).}
#' }
#'
#' The `agile_events` section is used to add markers on the Agile
#' calendar that directly relate to the Agile cadence. This contrasts
#' (a little) with the `markers` section that is/could be used for
#' markers for (more personal) reminders about, for instance,
#' deadlines. Both sections comprise any number of elements which
#' then have one or more dates where to add markers. The name of
#' each subsection is used for the marker label.
#'
#' The date where these markers need to be plotted can be expressed in
#' two ways: as an absolute date or as a "relative" date expressed as,
#' for instance, the *n*th week in the increment or iteration. See the
#' default configuration for examples.
#'
#' @docType data
#' @name agile_configuration
NULL

#' Create Marker Data Frame / Tibble
#'
#' The YAML configuration enables specify an absolute date as place for
#' a marker as well as using a relative date/location, for instance,
#' referring to the *n*'th week in the increment. This function takes
#' care of the processing.
#'
#' @param ev One event from a list of events
#' @param ev_name Name of `ev` in the top level object.
#' @param ... Further parameters to use in creating tibble.
#'
#' @return A one-row tibble with either a dstamp column for an absolute
#'    date or, for instance, an `increment_wk` column for a relative
#'    date.
#'
#' @importFrom dplyr '%>%'
#'
create_marker_dfr <- function(ev, ev_name, ...) {

  ## TODO: add check if ev has multiple elements/rows. Decide on whether
  ## this should (not) occur.

  if (is.list(ev) && is_nested(ev)) {
    uev <- rlang::sym(names(unlist(ev)))
    res <- tibble::tibble(event = ev_name, '{{ uev }}' := unlist(ev), ...)
  } else {
    res <- tibble::tibble(event = ev_name, dstamp = ev, ...)
  }

  return(res)
}

#' Check If List Is Nested
#'
#' @param l A list.
#'
#' @return Boolean whether `l` has nested lists.
#'
is_nested <- function(l) {
  stopifnot(is.list(l))

  for (i in l) {
    if (is.list(l)) return(TRUE)
  }

  return(FALSE)
}
