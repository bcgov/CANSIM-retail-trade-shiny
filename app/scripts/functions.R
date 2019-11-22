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
