
# Drop top and bottom 2.5% within each period, then add rank
ranked_dat <- period_dat |>
  group_by(period) |>
  filter(                                                        # filter(): keep rows inside quantile bounds
    gamma_avg >= quantile(gamma_avg, 0.025, na.rm = TRUE),      # quantile(): computes distributional cut-points
    gamma_avg <= quantile(gamma_avg, 0.975, na.rm = TRUE)
  ) |>
  mutate(x_pos = rank(gamma_avg, ties.method = "first")) |>     # rank(): ordinal position within period
  ungroup()

periods <- unique(ranked_dat$period)

walk(periods, function(p) {                                      # walk(): loop for side effects
  dat_p    <- filter(ranked_dat, period == p)
  ev_xpos  <- dat_p |> filter(ind_3f == 364) |> pull(x_pos)    # pull(): extracts a column as a vector

  plot_p <- ggplot(dat_p, aes(x = x_pos, y = gamma_avg, fill = highlight)) +
    geom_col(width = 0.9) +                                      # geom_col(): bar heights from data values
    geom_vline(                                                   # geom_vline(): draws a vertical reference line
      xintercept = ev_xpos,
      linetype   = "dashed",
      color      = "black",
      linewidth  = 0.7
    ) +
    scale_fill_manual(
      values = c("EV (364)" = "#F4A0B0", "Other" = "#4472C4"),
      guide  = guide_legend(title = NULL)
    ) +
    coord_cartesian(ylim = c(-0.06, 0.02)) +                     # coord_cartesian(): clips view without dropping data
    labs(
      title    = sprintf("Average Geographic Concentration (γ) by Industry, %s", p),
      subtitle = "Middle 95% of industries shown; dashed line = ind 364 (EV); pink bar = EV",
      x        = "Industry rank (by γ)",
      y        = "Average γ (EG index, stock of firms)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom"
    )

  out_file <- paste0(figure, sprintf("/eg_bar_%s.png", p)) 
  ggsave(out_file, plot = plot_p, width = 8, height = 5, dpi = 150)
  cat(sprintf("Plot saved: %s\n", out_file))
})
cat("Script completed successfully.\n")
