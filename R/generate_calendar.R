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
#' \description{
#'   \item{`dstamp`, `calendar_yr`, `calendar_wk`}{Date of the Monday of
#'     a particular week and the week number that belongs to that week
#'     (based on the ISO standard).}
#'   \item{`agile_wk`, `increment_wk`, `iteration_wk`}{Running week
#'     number of, respectively, the Agile year, increment, and iteration.}
#'   \item{`increment`, `iteration`}{Text labels for increments and
#'     iterations.}
#'   \item{`event`, `type`}{Markers to be mapped to weeks or dates in the
#'     Agile calendar. Two types exist: markers related to Agile events
#'     and rituals and (more personal) markers to, for instance, not
#'     forget important deadlines.}}
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
                  calendar_yr  = lubridate::year(dstamp),
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
    dplyr::mutate(iteration_no = factor(iteration_no, levels = c('ip', 1, 2, 3))) %>%
    dplyr::group_by(increment) %>%
      dplyr::mutate(iteration_wk = c(0, rep(1:.cfg$iteration_length, n_iterations))) %>%
    dplyr::ungroup()

  ## Construct temp. calendar with events and markers.
  tmp <- dplyr::bind_rows(cfg$agile_events, cfg$markers) %>%
    dplyr::mutate(calendar_wk = lubridate::isoweek(dstamp),
                  calendar_yr  = lubridate::year(dstamp)) %>%
    dplyr::left_join(y = res %>% dplyr::select(-dstamp),
                     by = c('calendar_yr', 'calendar_wk'))

  ## Join markers and events into main calendar
  cal <- dplyr::bind_rows(x = res, y = tmp)

  return(cal)
}
