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
  tibble::as_tibble(event_list) %>%
    tidyr::pivot_longer(cols = tidyr::everything(),
                        names_to = 'event',
                        values_to = 'dstamp')
}

#' @title Load Configuration File
#'
#' @description Load the configuration file for the generator.
#'
#' @param cfgfile A path to a configuration file.
#'
#' @importFrom dplyr "%>%"
#'
#' @export
read_config <- function(cfgfile) {

  cfg <- yaml::yaml.load_file(cfgfile)
  cfg$agile_events <- parse_agile_events(cfg$agile_events)
  cfg$markers <- parse_agile_events(cfg$markers)

  return(cfg)
}
