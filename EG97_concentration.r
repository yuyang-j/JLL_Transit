# === compute excess geographic concentration (Ellison Glaeser 1997) === 
# idea: pick up excessive geographic concentration at industry level
# in particular control for within-industry concentration driven by lumpy plants


# ── Package installation ──────────────────────────────────────────────────────
# Install required packages only if not already present.
if (!require(dplyr))   install.packages("dplyr",   quiet = TRUE)
if (!require(tibble))  install.packages("tibble",  quiet = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", quiet = TRUE)
if (!require(tidyr))   install.packages("tidyr",   quiet = TRUE)
if (!require(haven))   install.packages("haven",   quiet = TRUE)

library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(haven)

wd <- "F:/policy_competition"
datadir <- "F:/policy_competition/Data"
data_out <- "F:/policy_competition/Data/Intermediates"
table <- "F:/policy_competition/Tables"
figure <- "F:/policy_competition/Figures" 

setwd(wd)
# ── Step 1: Load data  ─────────────


stock_data <- paste0(datadir, "/Stock_entry_exit/city_ind_stock.dta" )  

raw <- 
  read_dta(stock_data)         
 

# ── Step 3: Collapse across years ─────────────────────────────────────────────
# Sum firm counts and capital over all years; EG is a cross-sectional index.
panel <- raw |>
  filter(!is.na(stock_num)) |> 
  #!is.na(stock_capital)) |>   # filter(): removes NA rows
  group_by(year,cityid, ind_3f) |>
    summarise(                                             # summarise(): collapses to one row per group
        firms   = sum(stock_num,     na.rm = TRUE),
    # capital = sum(stock_capital, na.rm = TRUE),
        .groups = "drop"
    )
  

# ── Step 4: Economy-wide city shares (the reference distribution x_i) ─────────
# x_i = city i's share of total firms (or capital) across ALL industries —
# this is the "dartboard" against which industry concentration is compared.
city_totals <- raw |>
  group_by(cityid,year) |>
  summarise(
    city_firms   = sum(stock_num),
    .groups      = "drop"
  ) |> 
  group_by(year) |>
  mutate(
    x_firms   = city_firms   / sum(city_firms),    # x_i for the firm-count index
  ) |> 
  ungroup() 

# Scalar denominators for G: 1 - Σ x_i²
denoms <- city_totals |> 
    group_by(year) |>
    summarise(denom_firms  = 1-sum(x_firms^2),
            .groups = "drop")

# ── Step 5: Industry-level city shares (s_i) and plant Herfindahl (H) ─────────
# For each industry, compute s_i = city's share of the industry's own total;
# H = Σ z_j² where z_j is each "plant" (city-level unit) share — used to
# correct for lumpy concentration that would arise even under random location.
ind_city <- raw |>
  group_by(ind_3f, year) |>
  mutate(
    ind_total_firms   = sum(stock_num)
  ) |>
  ungroup() |>
  mutate(
    s_firms   = stock_num   / ind_total_firms    # s_i: industry city share (firms)
  ) |>
  left_join(city_totals, by = c("cityid","year"))       # left_join(): merges city x_i columns

# ── Step 6: Compute G and H per industry, then γ ─────────────────────────────
# Equation (2) from Ellison & Glaeser (1997):
#   G   = Σ_i (s_i - x_i)² / (1 - Σ_i x_i²)
#   H   = Σ_i z_i²  (Herfindahl of "plants" = city units here)
#   γ   = (G - H) / (1 - H)
eg_index <- ind_city |>
  group_by(ind_3f,year) |>
  summarise(
    # Raw G numerator (firms-based and capital-based)
    G_num_firms   = sum((s_firms   - x_firms)^2),

    # Plant Herfindahl H (city-unit shares within the industry)
    H_firms   = sum(s_firms^2),

    .groups = "drop"
  ) |>
  left_join(denoms, by = "year") |>
  mutate(
    # Raw geographic concentration G  (Eq. 2 numerator / denominator)
    G_firms   = G_num_firms   / denom_firms,

    # EG gamma index: excess concentration beyond random agglomeration
    gamma_firms   = (G_firms   - H_firms)   / (1 - H_firms)
  ) |>
  group_by(year) |>
  # Rank industries: higher γ = more geographically concentrated
  arrange(desc(gamma_firms), .by_group = TRUE) |>
  mutate(rank_firms   = row_number()) |>
  ungroup() |>
  arrange(year, ind_3f)

# ── Step 7: Report industry 364 ───────────────────────────────────────────────
# Show where ind_3f == 364 stands relative to all other 3-digit industries.
n_total <- nrow(eg_index)

ind364 <- eg_index |>
  filter(ind_3f == 364)     # filter(): selects only the row for industry 364

cat("\n── Ellison-Glaeser γ Index Results ─────────────────────────────────────\n")
cat(sprintf("Total industries computed: %d\n\n", n_total))

cat("Industry 364 — firm-count γ:\n")
cat(sprintf("  γ (firms)   = %.4f  |  Rank = %d / %d\n",
            ind364$gamma_firms, ind364$rank_firms, n_total))

cat("\nIndustry 364 — registered capital γ:\n")
cat(sprintf("  γ (capital) = %.4f  |  Rank = %d / %d\n",
            ind364$gamma_capital, ind364$rank_capital, n_total))

cat("\n── Top 10 most concentrated industries (firm-count γ) ──────────────────\n")
eg_index |>
  arrange(rank_firms) |>
  slice_head(n = 10) |>     # slice_head(): returns the first n rows
  select(ind_3f, gamma_firms, rank_firms, gamma_capital, rank_capital) |>
  mutate(
    gamma_firms   = round(gamma_firms,   4),
    gamma_capital = round(gamma_capital, 4)
  ) |>
  as.data.frame() |>
  print()

# ── Step 8: Export results table ──────────────────────────────────────────────
# Write the full ranking to CSV for downstream use.
out_tbl <- eg_index |>
  select(ind_3f, gamma_firms, rank_firms, gamma_capital, rank_capital,
         G_firms, H_firms, G_capital, H_capital) |>
  mutate(across(where(is.double), \(x) round(x, 6)))

write.csv(out_tbl, "/mnt/user-data/outputs/eg_concentration_results.csv",
          row.names = FALSE)   # write.csv(): writes a data frame to a CSV file

cat("\nFull results saved to: /mnt/user-data/outputs/eg_concentration_results.csv\n")

# ── Step 9: Plot γ distribution and highlight industry 364 ───────────────────
# Visualise the distribution of γ and flag where ind 364 sits.
p <- ggplot(eg_index, aes(x = gamma_firms)) +
  geom_histogram(binwidth = 0.01, fill = "#4472C4", color = "white",  # geom_histogram(): draws histogram bars
                 alpha = 0.8) +
  geom_vline(                                 # geom_vline(): draws a vertical reference line
    xintercept = ind364$gamma_firms,
    color      = "#E84646",
    linewidth  = 1.2,
    linetype   = "dashed"
  ) +
  annotate(                                   # annotate(): adds a text label at given coordinates
    "text",
    x     = ind364$gamma_firms + 0.005,
    y     = Inf,
    label = paste0("ind 364\n(rank ", ind364$rank_firms, "/", n_total, ")"),
    vjust = 1.5, hjust = 0, size = 3.5, color = "#E84646"
  ) +
  labs(
    title    = "Distribution of EG Geographic Concentration Index (γ)",
    subtitle = "Firm-count based; red line = industry 364",
    x        = "γ (excess geographic concentration)",
    y        = "Number of industries"
  ) +
  theme_minimal()

ggsave(
  "/mnt/user-data/outputs/eg_concentration_plot.png",
  plot   = p,
  width  = 8,
  height = 5,
  dpi    = 150
)

cat("Plot saved to: /mnt/user-data/outputs/eg_concentration_plot.png\n")
cat("Script completed successfully.\n")



# obtain the code from Github 
download.file("https://raw.githubusercontent.com/yuyang-j/JLL_Transit/refs/heads/main/eg_concentration.R",destfile = "eg_concentration.R")

