plot_calendar <- function(cal) {
  ## FIXME presumes cal is correctly formatted.
  cal %>%
    ggplot2::ggplot(aes(x = dstamp, colour = forcats::as_factor(iteration_no), y = 1)) +
      ggplot2::geom_line(aes(group = 1), size = 6) +
      ggplot2::facet_wrap(~ increment, scales = 'free_x', ncol = 1) +
      ggplot2::scale_x_date(date_breaks = '1 week', date_labels = '%V') +
      ## theme_void() +
      ## geom_vline(aes(xintercept = as.Date(cfg$markers[[2]]$`deadline for VSRR`)), color = 'black') +
      ## geom_label_repel(aes(x = as.Date(cfg$markers[[2]]$`deadline for VSRR`),
      ##                     y = 1,
      ##                     label = names(cfg$markers[[2]])),
      ##                 nudge_x = 0.15,
      ##                 nudge_y = 1,
      ##                 box.padding = 0.5,
      ##                 segment.curvature = -0.1,
      ##                 segment.ncp = 3,
      ##                 segment.angle = 20,
      ##                 max.overlaps = Inf) +
      ## scale_y_continuous(expand = expansion(mult = 2.5)) +
      NULL
}

d %>%
  ggplot(aes(x = increment_wk, colour = forcats::as_factor(iteration_no), y = 1)) +
  geom_line(aes(group = 1), size = 6) +
  facet_wrap(~ increment, ncol = 1) +
  scale_x_continuous(n.breaks = 13) +
  coord_fixed(ratio = 4) +
  theme_void()

d %>%
  ggplot(aes(x = dstamp, colour = forcats::as_factor(iteration_no), y = 1)) +
  geom_line(aes(group = 1), size = 6) +
  facet_wrap(~ increment, scales = 'free_x', ncol = 1) +
  scale_x_date(date_breaks = '1 week', date_labels = '%V') +
  ## theme_void() +
  geom_vline(aes(xintercept = as.Date(cfg$markers[[2]]$`deadline for VSRR`)), color = 'black') +
  ## geom_label_repel(aes(x = as.Date(cfg$markers[[2]]$`deadline for VSRR`),
  ##                      label = names(cfg$markers[[2]])),
  ##                  nudge_x = 0.15,
  ##                  nudge_y = 1,
  ##                  box.padding = 0.5,
  ##                  segment.curvature = -0.1,
  ##                  segment.ncp = 3,
  ##                  segment.angle = 20,
  ##                  max.overlaps = Inf)
  NULL
