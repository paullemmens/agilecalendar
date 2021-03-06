#' @title Plot Agile Cadence Calendar
#'
#' @description Plots an agile cadence calendar generated by
#'    [`generate_calendar()`] with a YAML configuration.
#'
#' @param cal Dataframe with an agile calendar generated by the
#'    respective function.
#'
#' @return A ggplot object
#'
#' @importFrom dplyr "%>%"
#'
#' @export
plot_calendar <- function(cal) {
  ## FIXME presumes cal is correctly formatted.
  cal %>%
    ggplot2::ggplot(ggplot2::aes(x = dstamp, colour = forcats::as_factor(iteration_no), y = 1)) +
      ggplot2::geom_line(ggplot2::aes(group = 1), size = 6) +
      ggplot2::facet_wrap(~ increment, scales = 'free_x', ncol = 1) +
      ggplot2::scale_x_date(date_breaks = '1 week', date_labels = '%V') +
      ggplot2::labs(x = 'Week number', y = '', colour = 'Iteration') +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position  = 'bottom',
        text             = ggplot2::element_text(size    = 18),
        axis.title.x     = ggplot2::element_text(vjust   = -0.2),
        plot.caption     = ggplot2::element_text(size    = 11 * 0.8),
        strip.background = ggplot2::element_rect(colour =  NA, fill =  NA),
        strip.text       = ggplot2::element_text(size   =  ggplot2::rel(1.1)),
        panel.grid       = ggplot2::element_blank(),
        axis.text.y      = ggplot2::element_blank(),
        axis.ticks.y     = ggplot2::element_blank()
      )
}

#' @title Mark Agile Cadence Related Events
#'
#' @description Add markers to the calendar for a set of predefined
#'    markers related to events in the agile cadence.
#'
#' @param cal An agile calendar tibble generated by [`generate_calenar()`].
#' @param markers String to indicate which markers to draw. Should be one
#'    of "agile_events" or "markers".
#'
#' @return A list of ggplot2 geoms for drawing markers and adding labels
#'    using ggrepel.
#'
plot_markers <- function(cal, markers) {
  d <- cal %>%
    dplyr::filter(!is.na(event), type == markers)

  list(ggplot2::geom_vline(data    = d,
                           mapping = ggplot2::aes(xintercept = dstamp),
                           color   = 'darkslateblue',
                           size    = 2.0),
       ggrepel::geom_label_repel(data              = d,
                                 mapping           = ggplot2::aes(label = event),
                                 colour            = 'darkslateblue',
                                 nudge_x           = 0.15,
                                 nudge_y           = 1,
                                 box.padding       = 0.5,
                                 segment.curvature = -0.1,
                                 segment.ncp       = 3,
                                 segment.angle     = 20,
                                 max.overlaps      = Inf)
      )
}

#' @title Draw Marker For Today
#'
#' @description Adds a marker to indicate today's date in the agile
#'    calender visualization.
#'
#' @return A list with a ggplot2 geom_vline geom that can be added to an
#'    existing ggplot object.
#'
today_marker <- function() {
  list(ggplot2::geom_vline(data = tibble::tibble(dstamp = lubridate::as_date(Sys.time())),
                           mapping = ggplot2::aes(xintercept = dstamp),
                           color = 'red', size = 2.0))
}
