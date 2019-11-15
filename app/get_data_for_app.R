# tidyverse
library(tidyverse)
#library(readr)
#library(ggplot2)
#library(dplyr)
library(stringr)
library(glue)
library(lubridate)

# extending ggplot2
library(scales)

# utilities
library(here)
library(janitor)

# cansim
#install.packages("devtools")
#devtools::install_github("mountainmath/cansim")
library(cansim)


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


# Get Static Variables ----
# set table number as variable
tablenum <- "data_20_10_0008"
print(glue("TABLE:  ", tablenum))

# set date variables
latest_month <- max(data_20_10_0008$ref_date)
last_month <- latest_month - months(1)
last_year <- latest_month - years(1)


# Filter province data for app ----
# Seasonally adjusted - month over month
provinces <- data_20_10_0008 %>%
  ## Extract Canada and provinces (not territories), Seasonally Adjusted Retail Trade
  filter(vector %in% c("v52367097", "v52367155", "v52367185", "v52367215", "v52367245",
                       "v52367394", "v52367424", "v52367454", "v52367484", "v52367514",
                       "v52367573")) %>%
  group_by(geo) %>%
  get_mom_stats() %>%
  select(ref_date, geo,
         value, mom_pct, yoy_pct) %>%
  ungroup()

write_rds(provinces, here("data","provinces.rds"))


# Filter sector data for app ----
# unadjusted - year over year
sectors <-
  data_20_10_0008 %>%
  filter(geo == "British Columbia",
         adjustments %in% c("Unadjusted"),
         str_detect(
           classification_code_for_north_american_industry_classification_system_naics, "\\[4..\\]")) %>%
  group_by(classification_code_for_north_american_industry_classification_system_naics) %>%
  get_yoy_stats() %>%
  mutate(Subsector =
           str_remove(north_american_industry_classification_system_naics,
                      "\\[4..\\]") %>%
           strwrap(width = 8) %>% paste0(collapse = "\n")) %>%
  ungroup() %>%
  select(ref_date, Subsector,
         value, mom_pct, yoy_pct)

write_rds(sectors, here("data","sectors.rds"))

