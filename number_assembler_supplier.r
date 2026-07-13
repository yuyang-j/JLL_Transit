# compute number of assemblers and suppliers per city per year from assembler list and supplier_1718 list 


library(dplyr)
library(tidyr)

# ── Pseudo data ───────────────────────────────────────────────────────────────
# Stand-ins for Euros's real "assembler" and "supplier_1718" data frames, which
# are assumed to already exist in the environment with columns: city (4-digit
# code), establish_time, from_time (both dates/years), reg_capital_num.
set.seed(42)

make_fake_firms <- function(n, city_codes) {
  tibble(
    city            = sample(city_codes, n, replace = TRUE),
    establish_time  = sample(seq(as.Date("1998-01-01"), as.Date("2017-12-31"),
                                  by = "day"), n, replace = TRUE),
    # from_time is usually establish_time plus a short lag (registration delay),
    # occasionally missing, occasionally earlier (data-entry noise)
    from_time       = establish_time + sample(c(0, 30, 90, 180, 365, -60),
                                               n, replace = TRUE,
                                               prob = c(.5, .2, .1, .1, .05, .05)),
    to_time  = establish_time + sample(c(365, 730, 1095, 1460), n, replace = TRUE),
    reg_capital_num = round(rlnorm(n, meanlog = 7, sdlog = 1), 1)
  ) %>%
    mutate(from_time = as.Date(ifelse(runif(n) < 0.05, NA, from_time),
                                origin = "1970-01-01")) # 5% missing from_time
}

city_codes <- sprintf("%04d", sample(1000:9999, 25))
assembler       <- make_fake_firms(300, city_codes)
supplier_1718   <- make_fake_firms(400, city_codes)

# ── Step 1: Tag each dataset by firm type and stack them ─────────────────────
# Combining lets us build one panel instead of duplicating logic twice.
firms <- bind_rows(
  assembler     %>% mutate(firm_type = "assembler"),
  supplier_1718 %>% mutate(firm_type = "supplier")
)

# ── Step 2: Extract entry year under each timing definition ──────────────────
# Primary definition: from_time. Robustness definition: establish_time.
# We keep both so every downstream panel can be built either way, and so we
# can directly compare how much the choice of variable matters.
firms <- firms %>%
  mutate(
    from_time      = as.POSIXct(from_time, format = "%d/%m/%Y %H:%M:%S"),
    to_time      = as.POSIXct(to_time, format = "%d/%m/%Y %H:%M:%S"),
    establish_time = as.POSIXct(establish_time, format = "%d/%m/%Y %H:%M:%S"),
    year_from      = as.integer(format(from_time, "%Y")),
    year_to       = as.integer(format(to_time, "%Y")),
    year_establish = as.integer(format(establish_time, "%Y")), 
    from_missing   = is.na(from_time),
    year_mismatch  = !from_missing & (year_from != year_establish)
  )

cat("Share of firms with missing from_time:",
    round(mean(firms$from_missing), 3), "\n")
cat("Share of firms where from_time and establish_time imply",
    "a different entry year:", round(mean(firms$year_mismatch), 3), "\n")

# For the main panel, use from_time as entry year, falling back to
# establish_time only when from_time is missing.
firms <- firms %>%
  mutate(entry_year_main = ifelse(from_missing, year_establish, year_from))

# ── Step 3: Build a firm-year panel (one row per firm per year it exists) ────
# A firm is "existent" in its city from its entry year through the end of the
# sample period (no exit/closure variable is available, so firms are assumed
# to remain active). END_YEAR should be set to match the true end of coverage
# of the underlying data (e.g. the last year "supplier_1718" was updated).

build_firm_year_panel <- function(df, entry_year_col, exit_year_col) {
  df %>%
    filter(!is.na(.data[[entry_year_col]]),
           .data[[entry_year_col]] <= .data[[exit_year_col]]) %>%
    mutate(entry_year = .data[[entry_year_col]], 
           exit_year = .data[[exit_year_col]]
    ) %>%
    rowwise() %>%
    mutate(year = list(seq(entry_year, exit_year))) %>% # list(): builds the
                                                         # sequence of years
                                                         # the firm is active
    ungroup() %>%
    unnest(year) # unnest(): expands the year list into one row per firm-year
}

firm_year_main <- build_firm_year_panel(firms, "entry_year_main", "year_to")

# ── Step 4: Aggregate to city x year x firm_type ──────────────────────────────
# Count of firms and accumulated registered capital (sum of reg_capital_num
# across all firms active in that city-year) in one summarise call.
city_year_panel <- firm_year_main %>%
  group_by(city, year, firm_type) %>%
  summarise(
    n_firms          = n(),
    reg_capital_total = sum(reg_capital_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Widen so assembler/supplier counts and capital sit side by side per
  # city-year, filling city-years with no firms of a type as 0.
  pivot_wider(
    names_from  = firm_type,
    values_from = c(n_firms, reg_capital_total),
    values_fill = 0
  ) %>%
  arrange(city, year)

# ── Step 5: Robustness panel using establish_time instead of from_time ───────
# Same construction, but entry year comes purely from establish_time. Compare
# against the main panel to see how sensitive city-year counts/capital are to
# the choice of timing variable.
firm_year_robust <- build_firm_year_panel(firms, "year_establish", "year_to")

city_year_panel_robust <- firm_year_robust %>%
  group_by(city, year, firm_type) %>%
  summarise(
    n_firms          = n(),
    reg_capital_total = sum(reg_capital_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = firm_type,
    values_from = c(n_firms, reg_capital_total),
    values_fill = 0
  ) %>%
  arrange(city, year)

# ── Step 6: Compare main vs. robustness panel ────────────────────────────────
comparison <- full_join(
  city_year_panel,
  city_year_panel_robust,
  by = c("city", "year"),
  suffix = c("_fromtime", "_establish")
) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% # replace_na():
                                                             # fills NA with 0
                                                             # after the join
  mutate(
    diff_n_assembler = n_firms_assembler_fromtime - n_firms_assembler_establish,
    diff_n_supplier  = n_firms_supplier_fromtime  - n_firms_supplier_establish
  )

cat("\nRows where assembler counts differ between from_time and",
    "establish_time definitions:",
    sum(comparison$diff_n_assembler != 0), "out of", nrow(comparison), "\n")

# ── Step 7: Inspect and save ──────────────────────────────────────────────────
print(head(city_year_panel, 10))

write.csv(city_year_panel, "F:/policy_competition_Data_Intermediates/assembler_supplier_17_18.csv",
          row.names = FALSE)


write.csv(city_year_panel_robust,
          "F:/policy_competition_Data_Intermediates/assembler_supplier_17_18_crosscheck.csv",
          row.names = FALSE)