## Generate a data frame with a row for each week and variables for
## calendar week number, agile week number, date of the monday of each week,
## iteration name, increment label.

#' @title Generate an Agile Calendar
#'
#' @description Based on the `configuration` item in the YAML configuration
#'    this function generates a dataframe that can be used to visualize
#'    a calendar with default and additional markers from the remainder
#'    of the configuration file.
#'
#' @param cfg Configuration element from a YAML project configuration.
#'
#' @return A data frame / tibble with a row for each week and variables
#'    useful for visualizing the calendar.
#'
#' @importFrom dplyr "%>%"
#'
#' @export
generate_calendar <- function(cfg) {

  n_increments <- 52 / increment_length
  iteration_repeats <- (cfg$increment_length - cfg$ip_length) / cfg$iteration_length

  res <- tibble::tibble(agile_wk = 1:52) %>%
    dplyr::mutate(dstamp = lubridate::ymd(cfg$year_start) + lubridate::weeks(0:51),
                  calendar_wk = lubridate::isoweek(dstamp),
                  increment = gl(n_increments, cfg$increment_length,
                                 labels = paste0('PI ', cfg$year, '.', 1:4)),
                  ## FIXME: concept; labeling doesn't work
                  iteration = factor(c(gl(iteration_repeats, cfg$iteration_length),
                                       iteration_repeats + 1),
                                     labels = paste0('PI ', cfg$year, '.' 1:4,
                                                     '.' 1:iteration_repeats)))

  return(res)
}
