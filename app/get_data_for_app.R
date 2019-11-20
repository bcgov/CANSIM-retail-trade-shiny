# tidyverse
library(tidyverse)
library(lubridate)
# utilities
library(here)
library(janitor)
# cansim
library(cansim)

# functions ----

get_mom_stats <- function(df) {

  df %>%
    arrange(ref_date) %>%
    mutate(mom_val = lag(value),
           mom_pct = ((value / lag(value, n = 1)) - 1) * 100,
           mom_chg = (value - lag(value, n = 1)))
}

get_yoy_stats <- function(df) {

  df %>%
    arrange(ref_date) %>%
    mutate(yoy_val = lag(value, n = 12),
           yoy_pct = ((value / lag(value, n = 12)) - 1) * 100,
           yoy_chg = (value - lag(value, n = 12)))
}

# Read Data ----
data_20_10_0008_source <- get_cansim("20-10-0008-01")

data_20_10_0008 <- data_20_10_0008_source %>%
  mutate(REF_DATE = ymd(REF_DATE, truncated = 2))

# Clean Data ----
data_20_10_0008 <- janitor::clean_names(data_20_10_0008)


# Filter province data for app ----
# Seasonally adjusted - month over month
provinces <- data_20_10_0008 %>%
  ## Extract Canada and provinces (not territories), Seasonally Adjusted Retail Trade
  filter(vector %in% c("v52367097", "v52367155", "v52367185", "v52367215", "v52367245",
                       "v52367394", "v52367424", "v52367454", "v52367484", "v52367514",
                       "v52367573"),
         as.Date(ref_date) %in% c((max(ref_date) - years(5)):max(ref_date))) %>%
  group_by(geo) %>%
  get_mom_stats() %>%
  select(ref_date, geo,
         value, mom_pct) %>%
  ungroup()

write_rds(provinces, here("app","data","provinces.rds"))

# Filter sector data for app ----
# unadjusted - year over year
sectors <-
  data_20_10_0008 %>%
  filter(geo == "British Columbia",
         adjustments %in% c("Unadjusted"),
         as.Date(ref_date) %in% c((max(ref_date) - years(5)):max(ref_date)),
         str_detect(
           classification_code_for_north_american_industry_classification_system_naics,
           "\\[4..\\]|\\[44-45\\]")) %>%
  group_by(classification_code_for_north_american_industry_classification_system_naics) %>%
  get_yoy_stats() %>%
  mutate(sector = str_remove(north_american_industry_classification_system_naics,
                      "\\[4..\\]|\\[44-45\\]")) %>%
  mutate(sector = ifelse(sector == "Retail trade ", "All Retail Trade",sector)) %>%
  ungroup() %>%
  select(ref_date, sector,
         value, yoy_pct)

write_rds(sectors, here("app","data","sectors.rds"))

