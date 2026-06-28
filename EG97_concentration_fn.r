# === compute excess geographic concentration (Ellison Glaeser 1997) ===
# idea: pick up excessive geographic concentration at industry level
# in particular control for within-industry concentration driven by lumpy plants
# refactor: wrap index calculation in a function; apply over 5 flow variables


# ── Package installation ──────────────────────────────────────────────────────
# Install required packages only if not already present.
if (!require(dplyr))  install.packages("dplyr",  quiet = TRUE)
if (!require(tibble)) install.packages("tibble", quiet = TRUE)
if (!require(tidyr))  install.packages("tidyr",  quiet = TRUE)
if (!require(haven))  install.packages("haven",  quiet = TRUE)
if (!require(purrr))  install.packages("purrr",  quiet = TRUE)

library(dplyr)
library(tibble)
library(tidyr)
library(haven)
library(purrr)      # map(): apply a function over a list and collect results

wd       <- "F:/policy_competition"
datadir  <- "F:/policy_competition/Data"
data_out <- "F:/policy_competition/Data/Intermediates"

setwd(wd)

# ── Step 1: Load data ─────────────────────────────────────────────────────────
# Read the Stata file; panel is assumed to contain all 5 variables below.
stock_data <- paste0(datadir, "/Stock_entry_exit/city_ind_stock.dta")

raw <- tryCatch(
  read_dta(stock_data),   # read_dta(): reads a Stata .dta file into a tibble
  error = function(e) {
    message("Real data not found — using pseudo data for testing.")
    NULL
  }
)

# ── Step 2: Pseudo data (used when real data is absent) ───────────────────────
# Synthetic year × cityid × ind_3f panel covering all 5 variables.
if (is.null(raw)) {
  set.seed(42)
  n_cities <- 50
  n_inds   <- 30
  n_years  <- 5
  ind_codes <- c(364, sample(300:399, n_inds - 1, replace = FALSE))

  raw <- expand_grid(                    # expand_grid(): all combinations of inputs
    year   = 2015:(2015 + n_years - 1),
    cityid = 1:n_cities,
    ind_3f = ind_codes
  ) |>
    mutate(
      pull       = ifelse(ind_3f == 364 & cityid <= 5, 8, 1),
      stock_num  = rpois(n(), lambda = pull * 10) + 1L,
      stock_cap  = round(stock_num * runif(n(), 500, 5000), 0),
      entry_num  = rpois(n(), lambda = pull * 4)  + 0L,
      entry_cap  = round(entry_num * runif(n(), 400, 4000), 0),
      exit_num   = rpois(n(), lambda = pull * 2)  + 0L,
      exit_cap   = round(exit_num  * runif(n(), 300, 3000), 0)
    ) |>
    select(-pull)
}

# ── Step 3: Pre-build panel (year × cityid × ind_3f) ─────────────────────────
# Collapse raw data to the observation unit the EG function expects.
# NA rows for the variable being analysed are dropped inside the function,
# so we keep all rows here and pass the full panel through.
panel <- raw |>
  group_by(year, cityid, ind_3f) |>
  summarise(                              # summarise(): collapses to one row per group
    across(
      c(stock_num, stock_cap, entry_num, entry_cap, exit_num, exit_cap),
      \(x) sum(x, na.rm = TRUE)
    ),
    .groups = "drop"
  )


# ── Function: compute_eg() ────────────────────────────────────────────────────
# Computes the Ellison-Glaeser (1997) excess-concentration index (γ) for one
# variable across all years and industries in the supplied panel.
#
# Arguments
#   panel    : tibble with columns year, cityid, ind_3f, and <var_name>
#   var_name : character string naming the flow/stock variable to use
#              (e.g. "stock_num", "entry_cap")
#
# Returns
#   tibble with columns: variable, year, ind_3f, G, H, gamma, rank
#
compute_eg <- function(panel, var_name) {

  # -- (a) Drop NA rows for this variable and pull values into a plain column --
  dat <- panel |>
    filter(!is.na(.data[[var_name]])) |>   # .data[[]] lets us index by string name
    mutate(val = .data[[var_name]])         # rename to generic 'val' for the pipeline

  # -- (b) Economy-wide city shares (x_i) per year ---------------------------
  # x_i = city i's share of total activity across ALL industries in that year;
  # this is the "dartboard" reference against which industry concentration is judged.
  city_totals <- dat |>
    group_by(year, cityid) |>
    summarise(city_val = sum(val), .groups = "drop") |>
    group_by(year) |>
    mutate(x = city_val / sum(city_val)) |>   # mutate(): adds x_i share column
    ungroup()

  # -- (c) Per-year scalar denominators: 1 - Σ x_i² -------------------------
  denoms <- city_totals |>
    group_by(year) |>
    summarise(denom = 1 - sum(x^2), .groups = "drop")

  # -- (d) Industry-level city shares (s_i) and join x_i --------------------
  ind_city <- dat |>
    group_by(year, ind_3f) |>
    mutate(ind_total = sum(val)) |>
    ungroup() |>
    mutate(s = val / ind_total) |>            # s_i: this city's share of the industry
    left_join(city_totals, by = c("year", "cityid"))

  # -- (e) G, H, γ per year × industry --------------------------------------
  # G   = Σ_i (s_i - x_i)² / (1 - Σ_i x_i²)     [Eq. 2, EG 1997]
  # H   = Σ_i s_i²   (within-industry HHI; corrects for lumpy plants)
  # γ   = (G - H) / (1 - H)                        [excess concentration]
  result <- ind_city |>
    group_by(year, ind_3f) |>
    summarise(
      G_num = sum((s - x)^2),   # numerator of G before dividing by 1-Σx²
      H     = sum(s^2),         # plant-level Herfindahl (city units as "plants")
      .groups = "drop"
    ) |>
    left_join(denoms, by = "year") |>
    mutate(
      G     = G_num / denom,
      gamma = (G - H) / (1 - H)
    ) |>
    group_by(year) |>
    arrange(desc(gamma), .by_group = TRUE) |>
    mutate(rank = row_number()) |>   # within-year rank: 1 = most concentrated
    ungroup() |>
    arrange(year, ind_3f) |>
    mutate(variable = var_name) |>   # tag which variable this result came from
    select(variable, year, ind_3f, G, H, gamma, rank)

  result
}


# ── Step 4: Apply compute_eg() over all five variables ───────────────────────
# Run the function once per variable and stack results into a long tibble.
vars_to_run <- c("stock_num", "stock_cap", "entry_num", "entry_cap",
                 "exit_num",  "exit_cap")

eg_all <- map(vars_to_run, \(v) compute_eg(panel, v)) |>
  list_rbind()     # list_rbind(): stacks a list of tibbles row-wise into one tibble


# ── Step 5: Report industry 364 across all variables ─────────────────────────
# Show γ and rank for ind_3f == 364 for every variable and year.
cat("\n── EG γ Index — Industry 364 across all variables ──────────────────────\n")
eg_all |>
  filter(ind_3f == 364) |>
  mutate(gamma = round(gamma, 4)) |>
  select(variable, year, gamma, rank) |>
  as.data.frame() |>
  print()


# ── Step 6: Pivot to wide format (one gamma column per variable) ──────────────
# Produces a year × ind_3f table with columns gamma_stock_num, gamma_entry_cap, …
# suitable for merging into a regression panel as a set of covariates.
eg_wide <- eg_all |>
  select(variable, year, ind_3f, gamma) |>
  pivot_wider(                                   # pivot_wider(): long → wide reshape
    names_from   = variable,
    values_from  = gamma,
    names_prefix = "gamma_"                      # e.g. gamma_stock_num, gamma_entry_cap
  ) |>
  mutate(across(where(is.double), \(x) round(x, 6))) |>
  arrange(year, ind_3f)

cat("\n── Wide-format γ (first 3 rows) ─────────────────────────────────────────\n")
print(head(eg_wide, 3))

# ── Step 7: Export both formats ───────────────────────────────────────────────
# Wide table: ready to merge into a regression panel.
out_wide <- paste0(data_out, "/eg_concentration_wide.csv")

write.csv(eg_wide, out_wide, row.names = FALSE)

cat(sprintf("Wide table saved to: %s\n", out_wide))
