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

  n_increments <- 52 / cfg$increment_length
  n_iterations <- (cfg$increment_length - cfg$ip_length) / cfg$iteration_length

  res <- tibble::tibble(agile_wk = 1:52) %>%
    dplyr::mutate(dstamp = lubridate::ymd(cfg$year_start) + lubridate::weeks(0:51),
                  calendar_wk = lubridate::isoweek(dstamp),
                  increment = rep_len(rep.int(seq_len(n_increments),
                                              rep.int(cfg$increment_length,
                                                      n_increments)),
                                      52),
                  iteration = rep(c(rep.int(seq_len(n_iterations),
                                            rep.int(cfg$iteration_length,
                                                    n_iterations)),
                                    'ip'),
                                  n_increments)) %>%
    dplyr::mutate(increment = paste0('PI ', cfg$year, '.', increment),
                  iteration = paste0(increment, '.', iteration))

  return(res)
}
