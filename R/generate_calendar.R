#' @title Generate an Agile Calendar
#'
#' @description Based on the `configuration` item in the YAML configuration
#'    this function generates a dataframe that can be used to visualize
#'    a calendar with default and additional markers from the remainder
#'    of the configuration file.
#'
#' @param cfg Configuration settings from a YAML project configuration.
#'
#' @return A data frame / tibble with a row for each week and variables
#'    useful for visualizing the calendar.
#'
#' @importFrom dplyr "%>%"
#'
#' @export
generate_calendar <- function(cfg) {

  .cfg <- cfg$configuration

  ## Convenience variables that capture some agile cadence related scores.
  n_increments <- 52 / .cfg$increment_length
  n_iterations <- (.cfg$increment_length - .cfg$ip_length) / .cfg$iteration_length

  ## Construct basic calendar.
  res <- tibble::tibble(agile_wk = 1:52) %>%
    dplyr::mutate(dstamp       = lubridate::ymd(.cfg$year_start) + lubridate::weeks(0:51),
                  calendar_wk  = lubridate::isoweek(dstamp),
                  increment_wk = rep(1:.cfg$increment_length, n_increments),
                  increment_no = rep_len(rep.int(seq_len(n_increments),
                                                 rep.int(.cfg$increment_length,
                                                         n_increments)),
                                         52),
                  iteration_no = rep(c('ip',
                                     rep.int(seq_len(n_iterations),
                                             rep.int(.cfg$iteration_length,
                                                     n_iterations))),
                                     n_increments)) %>%
    dplyr::mutate(increment    = paste0('PI ', .cfg$year, '.', increment_no),
                  iteration    = paste0(increment, '.', iteration_no)) %>%
    dplyr::mutate(iteration_no = factor(iteration_no, levels = c('ip', 1, 2, 3)))

  ## Construct temp. calendar with events and markers.
  tmp <- dplyr::bind_rows(cfg$agile_events, cfg$markers) %>%
    dplyr::mutate(calendar_wk = lubridate::isoweek(dstamp)) %>%
    dplyr::left_join(y = res %>% dplyr::select(-dstamp),
                     by = 'calendar_wk')

  ## Join markers and events into main calendar
  cal <- dplyr::bind_rows(x = res, y = tmp)

  return(cal)
}