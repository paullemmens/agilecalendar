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

  purrr::map2_dfr(event_list, names(event_list),
                  ~ tibble::tibble(event = .y, dstamp = .x,
                                   type = d)) %>%
    dplyr::mutate(dstamp = lubridate::ymd(dstamp))
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
