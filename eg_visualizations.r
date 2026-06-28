# ── Package installation ──────────────────────────────────────────────────────
if (!require(dplyr))   install.packages("dplyr",   quiet = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", quiet = TRUE)
if (!require(tidyr))   install.packages("tidyr",   quiet = TRUE)

library(dplyr)
library(ggplot2)
library(tidyr)


# ── Plot 1 — gamma time series for ind 361 vs 364 ────────────────────
# Build separate data slices per industry, add two geom_lines, then label and
# style — keeping data, geometry, annotation, and aesthetics as distinct layers.

dat_361 <- eg_wide |> filter(ind_3f == 361, year >= 1995, year <= 2019)
dat_364 <- eg_wide |> filter(ind_3f == 364, year >= 1995, year <= 2019)

# End-of-series x/y positions for direct line labels
label_361 <- dat_361 |> filter(year == max(year))
label_364 <- dat_364 |> filter(year == max(year))

p1 <- ggplot(mapping = aes(x = year, y = gamma_stock_num)) +
  # -- geometry: one line + point series per industry --
  geom_line(data = dat_361, color = "#4472C4", linewidth = 0.9) +  # geom_line(): connects observations as a line
  geom_point(data = dat_361, color = "#4472C4", size = 1.8) +      # geom_point(): dot at each year
  geom_line(data = dat_364, color = "#E84646", linewidth = 0.9) +
  geom_point(data = dat_364, color = "#E84646", size = 1.8) +
  # -- annotation: direct labels at the right end of each line --
  annotate("text",                                                   # annotate(): places text at fixed coordinates
    x = label_361$year + 0.3, y = label_361$gamma_stock_num,
    label = "Car manufacturing (361)",
    hjust = 0, color = "#4472C4", size = 3.2
  ) +
  annotate("text",
    x = label_364$year + 0.3, y = label_364$gamma_stock_num,
    label = "EV manufacturing (364)",
    hjust = 0, color = "#E84646", size = 3.2
  ) +
  # -- aesthetics --
  scale_x_continuous(                                                # scale_x_continuous(): x-axis tick control
    breaks = seq(1995, 2019, by = 5),
    expand = expansion(mult = c(0.02, 0.22))  # expansion(): extra right margin for inline labels
  ) +
  labs(
    title = "Geographic Concentration (γ) — Car vs EV Manufacturing",
    x     = "Year",
    y     = "γ (EG index, stock of firms)"
  ) +
  theme_minimal() +
  theme(                                                             # theme(): fine-grained appearance control
    legend.position  = "none",
    panel.grid.minor = element_blank()
  )

ggsave(
  paste0(table, "/eg_line_361_364.png"),
  plot = p1, width = 8, height = 5, dpi = 150
)
cat("Plot 1 saved: eg_line_361_364.png\n")


# ── Plot 2 — 5-year average gamma per industry ────────────────────────
# Assign each year to a 5-year window and collapse to window means.

period_dat <- eg_wide |>
  filter(year >= 1995, year <= 2019) |>
  mutate(
    period = case_when(            # bin years into labelled 5-year windows
      year <= 1999 ~ "95to99",
      year > 1999 & year <= 2004 ~ "00to04",
      year > 2004 & year <= 2009 ~ "05to09",
      year > 2009 & year <= 2014 ~ "10to14",
      year > 2014 & year <= 2019 ~ "15to19"
    )
  ) |>
  group_by(period, ind_3f) |>
  summarise(
    gamma_avg = mean(gamma_stock_num, na.rm = TRUE),   # mean(): arithmetic mean, ignoring NA
    .groups   = "drop"
  ) |>
  mutate(highlight = ifelse(ind_3f == 364, "EV (364)", "Other"))


# ── Step 5: Plot 2 — bar plot per 5-year window, ranked low → high ───────────
# Within each facet, industries are independently re-ranked by gamma_avg.
# ind 364 is highlighted in pink; all others in blue.

# Compute within-period rank order for x-axis sorting
ranked_dat <- period_dat |>
  group_by(period) |>
  mutate(
    rank_in_period = rank(gamma_avg, ties.method = "first"),   # rank(): assigns ordinal position
    # ggplot needs a *unique* x value per bar; encode as "rank.period" string
    x_pos = rank_in_period
  ) |>
  ungroup()
periods <- unique(ranked_dat$period)

walk(periods, function(p) {                                    # walk(): like map() but called for side effects
  dat_p <- filter(ranked_dat, period == p)

  plot_p <- ggplot(dat_p, aes(x = x_pos, y = gamma_avg, fill = highlight)) +
    geom_col(width = 0.9) +                                    # geom_col(): bar heights from data values
    scale_fill_manual(
      values = c("EV (364)" = "#F4A0B0", "Other" = "#4472C4"),
      guide  = guide_legend(title = NULL)
    ) +
    labs(
      title    = sprintf("Average Geographic Concentration (γ) by Industry, %s", p),
      subtitle = "Industries ranked lowest → highest; pink = ind 364 (EV)",
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

  out_file <- sprintf("/mnt/user-data/outputs/eg_bar_%s.png", p)
  ggsave(out_file, plot = plot_p, width = 8, height = 5, dpi = 150)
  cat(sprintf("Plot saved: %s\n", out_file))
})
cat("Script completed successfully.\n")