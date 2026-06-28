
# ── Step 3: Plot 1 — gamma time series for ind 361 vs 364 ────────────────────
# Wrapped in a function so the y variable can be swapped and the plot/filename
# labels update automatically. Called in a walk() over all gamma columns.

plot_line_361_364 <- function(eg_wide, y_var) {

  dat_361 <- eg_wide |> filter(ind_3f == 361, year >= 1995, year <= 2019)
  dat_364 <- eg_wide |> filter(ind_3f == 364, year >= 1995, year <= 2019)

  # End-of-series coordinates for direct inline labels
  label_361 <- dat_361 |> filter(year == max(year))
  label_364 <- dat_364 |> filter(year == max(year))

  ggplot(mapping = aes(x = year, y = .data[[y_var]])) +
    # -- geometry --
    geom_line(data = dat_361, color = "#4472C4", linewidth = 0.9) +  # geom_line(): connects observations as a line
    geom_point(data = dat_361, color = "#4472C4", size = 1.8) +      # geom_point(): dot at each year
    geom_line(data = dat_364, color = "#E84646", linewidth = 0.9) +
    geom_point(data = dat_364, color = "#E84646", size = 1.8) +
    # -- annotation --
    annotate("text",                                                   # annotate(): places text at fixed coordinates
      x = label_361$year + 0.3, y = label_361[[y_var]],
      label = "Car manufacturing (361)",
      hjust = 0, color = "#4472C4", size = 3.2
    ) +
    annotate("text",
      x = label_364$year + 0.3, y = label_364[[y_var]],
      label = "EV manufacturing (364)",
      hjust = 0, color = "#E84646", size = 3.2
    ) +
    # -- aesthetics --
    scale_x_continuous(                                                # scale_x_continuous(): x-axis tick control
      breaks = seq(1995, 2019, by = 5),
      expand = expansion(mult = c(0.02, 0.22))
    ) +
    labs(
      title = sprintf("Geographic Concentration (γ) — Car vs EV Manufacturing, %s", y_var),
      x     = "Year",
      y     = sprintf("γ (EG index, %s)", y_var)
    ) +
    theme_minimal() +
    theme(legend.position = "none", panel.grid.minor = element_blank())
}

# Identify all gamma columns in eg_wide and loop over them
y_vars <- c("gamma_stock_num", "gamma_reg_num", "gamma_reg_capital")  # grep(): returns matching column names

walk(y_vars, function(y) {                                # walk(): loop for side effects
  p <- plot_line_361_364(eg_wide, y)
  out_file <- paste0(figure, sprintf("eg_line_361_364_%s.png", y))
  ggsave(out_file, plot = p, width = 8, height = 5, dpi = 150)
  cat(sprintf("Plot saved: %s\n", out_file))
})


# ── Step 4: Compute 5-year average gamma per industry ────────────────────────
# Assign each year to a 5-year window and collapse to window means.

period_dat <- eg_wide |>
  filter(year >= 1995, year <= 2019) |>
  mutate(
    period = case_when(            # bin years into labelled 5-year windows
      year <= 1999              ~ "95to99",
      year > 1999 & year <= 2004 ~ "00to04",
      year <= 2009              ~ "05to09",
      year <= 2014              ~ "10to14",
      TRUE                      ~ "15to19"
    )
  ) |>
  group_by(period, ind_3f) |>
  summarise(
    gamma_avg = mean(gamma_stock_num, na.rm = TRUE),   # mean(): arithmetic mean, ignoring NA
    .groups   = "drop"
  ) |>
  mutate(highlight = ifelse(ind_3f == 364, "EV (364)", "Other"))
