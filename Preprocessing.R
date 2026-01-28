

#### Pre-processing of raw data (saved to all_countries_br_data.csv)
library(dplyr)
library(purrr)
library(stringr)

################ BIRTH ################ 
df <- read.csv("UNdata_Export_20250125_084105150.csv", stringsAsFactors = FALSE)

################ DEATH ################ 
# df <- read.csv("UNdata_Export_20250125_062813029.csv", stringsAsFactors = FALSE)

valid_months <- month.name  # January...December

# ============================================================
# 1) Split by country correctly (fixes your issue)
# ============================================================
df_grouped <- df %>% group_by(`Country.or.Area`)

country_keys <- df_grouped %>% group_keys()        # correct order of countries
country_dfs <- df_grouped %>% group_split()        # list of tibbles in same order
names(country_dfs) <- country_keys$Country.or.Area # <-- correctly assigns names

# ============================================================
# 2) Deduplicate and filter each country
# ============================================================
country_dfs <- map(country_dfs, function(x) {
  
  x %>%
    mutate(pref_final = ifelse(Reliability == "Final figure, complete", 1, 0)) %>%
    arrange(desc(pref_final)) %>%
    distinct(`Country.or.Area`, Year, Month, .keep_all = TRUE) %>%
    filter(Month %in% valid_months) %>%
    mutate(row_id = row_number())
})


# ============================================================
# 2) Filter countries by year criteria
# ============================================================

countries_to_remove <- c()

for (country in names(country_dfs)) {
  
  data <- country_dfs[[country]]
  
  # Step 1: keep only years with exactly 12 months
  data_full <- data %>%
    group_by(Year) %>%
    filter(n() == 12) %>%
    ungroup()
  
  # Step 2: compute yearly avg
  yearly_avg <- data_full %>%
    group_by(Year) %>%
    summarise(avg = mean(as.numeric(Value)), .groups = "drop")
  
  # Step 3: count thresholds
  years_over_300 <- sum(yearly_avg$avg >= 300)
  years_over_100 <- sum(yearly_avg$avg >= 100)
  
  # Step 4: keep or remove
  if (years_over_300 >= 3 || years_over_100 >= 8) {
    
    valid_years <- yearly_avg %>%
      filter(avg >= 300 | avg >= 100) %>%
      pull(Year)
    
    if (nrow(yearly_avg) != length(valid_years)) {
      print(country)
      print(nrow(yearly_avg))
      print(length(valid_years))
    }
    
    country_dfs[[country]] <- data_full %>%
      filter(Year %in% valid_years)
    
  } else {
    countries_to_remove <- c(countries_to_remove, country)
  }
}

# Step 5: remove excluded countries
country_dfs <- country_dfs[!(names(country_dfs) %in% countries_to_remove)]

# ============================================================
# 3) Normalize values by month length and yearly totals
# ============================================================

days_in_month <- function(year, month_name) {
  month_num <- match(month_name, month.name)
  # leap year handling
  if (month_num == 2) {
    return(ifelse((year %% 400 == 0) | (year %% 4 == 0 & year %% 100 != 0), 29, 28))
  }
  return(c(31,28,31,30,31,30,31,31,30,31,30,31)[month_num])
}

country_dfs <- map(country_dfs, function(df) {
  
  df <- df %>%
    mutate(
      Year = as.integer(Year),
      Value = as.numeric(Value),
      Month_num = match(Month, month.name)
    ) %>%
    arrange(Year, Month_num)
  
  # Step 1: normalize by days in month
  df$days <- mapply(days_in_month, df$Year, df$Month)
  df$NormValue <- df$Value / df$days
  
  # Step 2: normalize by yearly total
  yearly_total <- df %>%
    group_by(Year) %>%
    summarise(total = sum(NormValue), .groups = "drop")
  
  df <- df %>%
    left_join(yearly_total, by = "Year") %>%
    mutate(NormValue = NormValue / total)
  
  # Step 3: subtract 1/12 and scale by 100/(1/12)
  df <- df %>%
    mutate(
      NormValue = NormValue - (1/12),
      NormValue = (100 * NormValue) / (1/12)
    ) %>%
    select(-days, -total)
  
  df
})

# ============================================================
# 4) Combine all countries
# ============================================================

combined_df <- bind_rows(country_dfs, .id = "Country")


### Save as all_countries_br_data.csv or all_countries_mr_data.csv









