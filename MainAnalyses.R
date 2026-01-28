library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(readxl)              
library(ggplot2)
library(patchwork)
library(ggrepel)
library(grid)
library(scales)

# Assumes these are already loaded
br_data <- read.csv("all_countries_br_data.csv", header = TRUE)
all_br_data <- br_data
mr_data <- read.csv("all_countries_mr_data.csv", header = TRUE)
all_mr_data <- mr_data
latitude_data <- read.delim("country_metadata.tsv", header=TRUE)
gdp <- read.csv("economy data/GDP DATA - MAY 2025/gdpALL.csv", skip = 4)
population <- read.csv("population data/API_SP.POP.TOTL_DS2_en_csv_v2_2590.csv", skip = 4)
contraception <- read.csv("unpopulation_dataportal_20250626191701.csv", header = TRUE)
periodogram <- read.csv("country_metadata_plomb_SL.csv", header = TRUE)

# Preprocess GDP
gdp$Country.Name <- recode(gdp$Country.Name,
                           "Bahamas, The" = "Bahamas",
                           "Hong Kong SAR, China" = "China, Hong Kong SAR",
                           "Macao SAR, China" = "China, Macao SAR",
                           "Egypt, Arab Rep." = "Egypt",
                           "Iran, Islamic Rep." = "Iran (Islamic Republic of)",
                           "Kyrgyz Republic" = "Kyrgyzstan",
                           "Netherlands" = "Netherlands (Kingdom of the)",
                           "Korea, Dem. People's Rep." = "Republic of Korea",
                           "Moldova" = "Republic of Moldova",
                           "St. Lucia" = "Saint Lucia",
                           "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
                           "São Tomé and Principe" = "Sao Tome and Principe",
                           "Slovak Republic" = "Slovakia",
                           "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
                           "United States" = "United States of America",
                           "Virgin Islands (U.S.)" = "United States Virgin Islands",
                           "Venezuela, RB" = "Venezuela (Bolivarian Republic of)",
                           "Turkiye" = "Türkiye",
                           "Curacao" = "Curaçao",
                           "Korea, Rep." = "Republic of Korea"
)

gdp_long <- gdp %>%
  dplyr::select(Country.Name, starts_with("X")) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year", names_prefix = "X",
               values_to = "GDP", values_drop_na = TRUE) %>%
  mutate(Year = as.integer(Year))


# Population to get GDP per capita
population$Country.Name <- recode(population$Country.Name,
                                  "Bahamas, The" = "Bahamas",
                                  "Hong Kong SAR, China" = "China, Hong Kong SAR",
                                  "Macao SAR, China" = "China, Macao SAR",
                                  "Egypt, Arab Rep." = "Egypt",
                                  "Iran, Islamic Rep." = "Iran (Islamic Republic of)",
                                  "Kyrgyz Republic" = "Kyrgyzstan",
                                  "Netherlands" = "Netherlands (Kingdom of the)",
                                  "Korea, Dem. People's Rep." = "Republic of Korea",
                                  "Moldova" = "Republic of Moldova",
                                  "St. Lucia" = "Saint Lucia",
                                  "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
                                  "São Tomé and Principe" = "Sao Tome and Principe",
                                  "Slovak Republic" = "Slovakia",
                                  "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
                                  "United States" = "United States of America",
                                  "Virgin Islands (U.S.)" = "United States Virgin Islands",
                                  "Venezuela, RB" = "Venezuela (Bolivarian Republic of)",
                                  "Turkiye" = "Türkiye",
                                  "Curacao" = "Curaçao"
)

pop_long <- population %>%
  dplyr::select(Country.Name, starts_with("X")) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year", names_prefix = "X",
               values_to = "Population", values_drop_na = TRUE) %>%
  mutate(Year = as.integer(Year))

gdp_pc_long <- inner_join(gdp_long, pop_long, by = c("Country.Name", "Year")) %>%
  mutate(GDP_PC = GDP / Population)


contraception <- contraception[contraception$EstimateMethod == "Interpolation", ]
contraception <- contraception[contraception$Category == "Married or in a union women", ]

contraception$Location[contraception$Location == "United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland"
contraception$Location[contraception$Location == "Netherlands"] <- "Netherlands (Kingdom of the)"

contraception <- contraception %>%
  rename(
    Year = Time
  )

# Standardize all country column names to "Country"
gdp_pc_long <- gdp_pc_long %>%
  rename(Country = Country.Name)

contraception <- contraception %>%
  rename(Country = Location)

# Standardize column names for join
pop_long <- pop_long %>%
  rename(Country.or.Area = Country.Name)

# Join by Country and Year
br_with_pop <- br_data %>%
  inner_join(pop_long, by = c("Country.or.Area", "Year"))

# Compute absolute monthly birth rate
br_with_rate <- br_with_pop %>%
  mutate(Birth_Rate = Value / Population)



###### CLIMATE DATA ######
library(readxl)              # load the package each session
library(stringr)

# Function to clean dataset
# rename_years = TRUE → rename "YYYY-MM" to "YYYY"
clean_ckp_data <- function(df, rename_years = TRUE) {
  name_map <- c(
    "American Samoa (U.S.)" = "American Samoa",
    "Curaçao (Neth.)" = "Curaçao",
    "Czech Republic" = "Czechia",
    "Arab Republic of Egypt" = "Egypt",
    "Guam (U.S.)" = "Guam",
    "Netherlands" = "Netherlands (Kingdom of the)",
    "New Caledonia (Fr.)" = "New Caledonia",
    "Puerto Rico (U.S.)" = "Puerto Rico",
    "Slovak Republic" = "Slovakia",
    "Guam (U.K.)" = "United Kingdom of Great Britain and Northern Ireland",
    "United States Virgin Islands (U.S.)" = "United States Virgin Islands",
    "R. B. de Venezuela" = "Venezuela (Bolivarian Republic of)",
    "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland"
  )
  
  df <- df %>%
    mutate(name = ifelse(name %in% names(name_map), name_map[name], name))
  
  if (rename_years) {
    df <- df %>%
      rename_with(~ str_replace(.x, "^(\\d{4})-\\d{2}$", "\\1"))
  }
  
  df
}

# Load and clean both datasets
cpk_tavg <- read_excel("ckp_data/ckp_tavg.xlsx") %>% clean_ckp_data()
cpk_tmin <- read_excel("ckp_data/ckp_tmin.xlsx") %>% clean_ckp_data()
cpk_tmax <- read_excel("ckp_data/ckp_tmax.xlsx") %>% clean_ckp_data()
cpk_prcp <- read_excel("ckp_data/ckp_prcp.xlsx") %>% clean_ckp_data()
cpk_hum <- read_excel("ckp_data/ckp_hum.xlsx") %>% clean_ckp_data()

cpk_tavg_monthly <- read_excel("ckp_data/ckp_tavg_monthly.xlsx") %>% clean_ckp_data(rename_years = FALSE)
cpk_tmin_monthly <- read_excel("ckp_data/ckp_tmin_monthly.xlsx") %>% clean_ckp_data(rename_years = FALSE)
cpk_tmax_monthly <- read_excel("ckp_data/ckp_tmax_monthly.xlsx") %>% clean_ckp_data(rename_years = FALSE)
cpk_prcp_monthly <- read_excel("ckp_data/ckp_prcp_monthly.xlsx") %>% clean_ckp_data(rename_years = FALSE)
cpk_hum_monthly <- read_excel("ckp_data/ckp_hum_monthly.xlsx") %>% clean_ckp_data(rename_years = FALSE)


calcPhotoperiod <- function(lat_deg, day_of_year) {
  lat_rad <- lat_deg * pi / 180
  decl <- asin(sin(-23.44 * pi / 180) * cos(2 * pi * (day_of_year + 10) / 365.24))
  solar_alt <- -0.833 * pi / 180
  
  cosH0 <- (sin(solar_alt) - sin(lat_rad) * sin(decl)) / (cos(lat_rad) * cos(decl))
  cosH0 <- pmin(pmax(cosH0, -1), 1)
  H0 <- acos(cosH0) * 180 / pi
  daylength <- (2 / 15) * H0
  return(daylength)
}

# Get solstice daylength difference for each country
latitude_with_daylength <- periodogram %>%
  dplyr::select(Country, Latitude) %>%
  mutate(
    summer_day = 172,
    winter_day = 355,
    summer_dl = calcPhotoperiod(Latitude, summer_day),
    winter_dl = calcPhotoperiod(Latitude, winter_day),
    mean_daylength_diff = abs(summer_dl - winter_dl)
  )



br_countries_to_keep <- unique(periodogram$Country[periodogram$Birth_pvals_12m < 1e-10])
br_countries_to_keep <- br_countries_to_keep[!is.na(br_countries_to_keep)]
br_data <- br_data[br_data$Country.or.Area %in% br_countries_to_keep, ]

#br_data <- br_data %>%
#  filter(Year >= 1970, Year <= 2019)

br_data <- br_data %>% filter(Country.or.Area != "Republic of Korea")


### Compute mean month and amplitude
compute_seasonality_stats <- function(df, value_col = "Normalized.Value") {
  df <- df %>% mutate(Year = as.integer(Year))
  
  # Keep only complete years (12 months)
  complete_years <- df %>%
    group_by(Country.or.Area, Year) %>%
    filter(n() == 12) %>%
    ungroup()
  
  # Mean month
  mean_months <- complete_years %>%
    group_by(Country.or.Area, Year) %>%
    summarise(
      mean_month = {
        months <- Month_num
        values <- .data[[value_col]]
        angles <- 2 * pi * (months - 1) / 12
        mean_angle <- atan2(sum(sin(angles) * values), sum(cos(angles) * values))
        month <- (mean_angle * 12 / (2 * pi)) %% 12
        if (month == 0) 12 else month
      },
      .groups = "drop"
    )
  
  
  mean_month_by_country <- mean_months %>%
    mutate(angle = 2 * pi * mean_month / 12,
           sin_val = sin(angle),
           cos_val = cos(angle)) %>%
    group_by(Country.or.Area) %>%
    summarise(
      mean_month = {
        mean_angle <- atan2(mean(sin_val), mean(cos_val))
        circ_month <- (mean_angle * 12 / (2 * pi)) %% 12
        if (circ_month == 0) 12 else circ_month
      },
      .groups = "drop"
    )
  
  
  # Amplitude
  monthly_medians <- complete_years %>%
    group_by(Country.or.Area, Month) %>%
    summarise(month_median = median(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  
  amplitude <- monthly_medians %>%
    group_by(Country.or.Area) %>%
    summarise(amplitude = (max(month_median) - min(month_median)) / 2, .groups = "drop")
  
  inner_join(mean_month_by_country, amplitude, by = "Country.or.Area")
}


##########################
# Find year range for birth rate data for each country
year_range_br <- br_data %>%
  group_by(Country = Country.or.Area) %>%
  summarise(min_year = min(Year), max_year = max(Year), .groups = "drop")

apply_year_range <- function(year_range, df, country_col = "Country") {
  df %>%
    inner_join(year_range, by = setNames("Country", country_col)) %>%
    filter(Year >= min_year, Year <= max_year) %>%
    dplyr::select(-min_year, -max_year)
}

# Filter by country-specific year ranges
pop_long_filtered_br <- apply_year_range(year_range_br, pop_long, "Country.or.Area") %>%
  mutate(log10_Population = log10(Population))

gdp_pc_long_filtered_br <- apply_year_range(year_range_br, gdp_pc_long) %>%
  mutate(log10_GDP_PC = log10(GDP_PC))

contraception_filtered_br <- apply_year_range(year_range_br, contraception)


#### CPK 
# Annual helper: wide -> long, filter to countries + year range
make_annual <- function(df, value_name,
                        countries = br_data$Country.or.Area,
                        year_range = year_range_br) {
  df %>%
    filter(name %in% unique(countries)) %>%
    pivot_longer(
      cols = -tidyselect::any_of(c("code","name")),
      names_to = "Year", values_to = value_name
    ) %>%
    mutate(Year = suppressWarnings(as.integer(Year))) %>%
    rename(Country = name) %>%
    inner_join(year_range, by = c("Country")) %>%
    filter(Year >= min_year, Year <= max_year) %>%
    dplyr::select(Country, Year, !!sym(value_name))
}

# Monthly helper: wide -> long (YYYY-MM), split, filter to countries + year range
make_monthly <- function(df, value_name,
                         countries = br_data$Country.or.Area,
                         year_range = year_range_br) {
  df %>%
    filter(name %in% unique(countries)) %>%
    pivot_longer(
      cols = -tidyselect::any_of(c("code","name")),
      names_to = "year_month", values_to = value_name
    ) %>%
    separate(year_month, into = c("Year", "Month"), sep = "-", convert = TRUE) %>%
    rename(Country = name) %>%
    inner_join(year_range, by = c("Country")) %>%
    filter(Year >= min_year, Year <= max_year) %>%
    dplyr::select(Country, Year, Month, !!sym(value_name))
}

# === Your four outputs, now one-liners ===
cpk_tavg_br_filtered <- make_annual(cpk_tavg, "TAVG")
cpk_tmin_br_filtered <- make_annual(cpk_tmin, "TMIN")
cpk_tmax_br_filtered <- make_annual(cpk_tmax, "TMAX")
cpk_prcp_br_filtered <- make_annual(cpk_prcp, "PRCP")
cpk_hum_br_filtered <- make_annual(cpk_hum, "HUM")

cpk_monthly_tavg_br_filtered <- make_monthly(cpk_tavg_monthly, "TAVG")
cpk_monthly_tmin_br_filtered <- make_monthly(cpk_tmin_monthly, "TMIN")
cpk_monthly_tmax_br_filtered <- make_monthly(cpk_tmax_monthly, "TMAX")
cpk_monthly_prcp_br_filtered <- make_monthly(cpk_prcp_monthly, "PRCP")
cpk_monthly_hum_br_filtered <- make_monthly(cpk_hum_monthly, "HUM")



# More than 15 years of data and log scale
population_avg_br <- pop_long_filtered_br %>%
  group_by(Country = Country.or.Area) %>%
  summarise(
    mean_population = mean(Population, na.rm = TRUE),
    log10_population = mean(log10_Population, na.rm = TRUE),
    .groups = "drop"
  )

# More than 15 years of data and log scale
gdp_pc_avg_br <- gdp_pc_long_filtered_br %>%
  group_by(Country) %>%
  summarise(
    mean_gdp_pc = mean(GDP_PC, na.rm = TRUE),
    log10_gdp_pc = mean(log10_GDP_PC, na.rm = TRUE),
    .groups = "drop"
  )

# More than 15 years of data
contraception_avg_br <- contraception_filtered_br %>%
  group_by(Country) %>%
  summarise(mean_contraception = mean(Value, na.rm = TRUE), .groups = "drop")

# TAVG
cpk_tavg_br <- cpk_tavg_br_filtered %>%
  group_by(Country) %>%
  summarise(
    mean_TAVG = mean(TAVG, na.rm = TRUE),
    .groups = "drop"
  )

cpk_tavg_amp_br <- cpk_monthly_tavg_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(
    tavg_amp = max(TAVG, na.rm = TRUE) - min(TAVG, na.rm = TRUE),
    .groups = "drop_last"  # drop only Year grouping
  ) %>%
  summarise(
    mean_TAVG_AMP = mean(tavg_amp, na.rm = TRUE),
    .groups = "drop"
  )

# TMIN
cpk_tmin_br <- cpk_tmin_br_filtered %>%
  group_by(Country) %>%
  summarise(
    mean_TMIN = mean(TMIN, na.rm = TRUE),
    .groups = "drop"
  )

cpk_tmin_amp_br <- cpk_monthly_tmin_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(
    tmin_amp = max(TMIN, na.rm = TRUE) - min(TMIN, na.rm = TRUE),
    .groups = "drop_last"  # drop only Year grouping
  ) %>%
  summarise(
    mean_TMIN_AMP = mean(tmin_amp, na.rm = TRUE),
    .groups = "drop"
  )

# TMAX
cpk_tmax_br <- cpk_tmax_br_filtered %>%
  group_by(Country) %>%
  summarise(
    mean_TMAX = mean(TMAX, na.rm = TRUE),
    .groups = "drop"
  )

cpk_tmax_amp_br <- cpk_monthly_tmax_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(
    tmax_amp = max(TMAX, na.rm = TRUE) - min(TMAX, na.rm = TRUE),
    .groups = "drop_last"  # drop only Year grouping
  ) %>%
  summarise(
    mean_TMAX_AMP = mean(tmax_amp, na.rm = TRUE),
    .groups = "drop"
  )


# PRCP
cpk_prcp_br <- cpk_prcp_br_filtered %>%
  group_by(Country) %>%
  summarise(
    mean_PRCP = mean(PRCP, na.rm = TRUE),
    .groups = "drop"
  )

cpk_prcp_amp_br <- cpk_monthly_prcp_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(
    prcp_amp = max(PRCP, na.rm = TRUE) - min(PRCP, na.rm = TRUE),
    .groups = "drop_last"  # drop only Year grouping
  ) %>%
  summarise(
    mean_PRCP_AMP = mean(prcp_amp, na.rm = TRUE),
    .groups = "drop"
  )


# HUM
cpk_hum_br <- cpk_hum_br_filtered %>%
  group_by(Country) %>%
  summarise(
    mean_HUM = mean(HUM, na.rm = TRUE),
    .groups = "drop"
  )

cpk_hum_amp_br <- cpk_monthly_hum_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(
    hum_amp = max(HUM, na.rm = TRUE) - min(HUM, na.rm = TRUE),
    .groups = "drop_last"  # drop only Year grouping
  ) %>%
  summarise(
    mean_HUM_AMP = mean(hum_amp, na.rm = TRUE),
    .groups = "drop"
  )


latitude <- periodogram %>%
  dplyr::select(Country, Latitude) %>%
  mutate(abs_latitude = abs(Latitude))



##### Compute predictor mean months ######
# ===============================
# Function: compute circular mean month
# ===============================
compute_mean_month <- function(df, value_col) {
  df <- df %>% mutate(Year = as.integer(Year))
  
  # Keep only complete years (12 months)
  complete_years <- df %>%
    group_by(Country, Year) %>%
    filter(n() == 12) %>%
    ungroup()
  
  # Mean month (circular mean) per year
  mean_months <- complete_years %>%
    group_by(Country, Year) %>%
    summarise(
      mean_month = {
        months <- Month
        values <- .data[[value_col]]
        angles <- 2 * pi * (months - 1) / 12
        mean_angle <- atan2(sum(sin(angles) * values, na.rm = TRUE),
                            sum(cos(angles) * values, na.rm = TRUE))
        month <- (mean_angle * 12 / (2 * pi)) %% 12
        if (month == 0) 12 else month
      },
      .groups = "drop"
    )
  
  # Average across years per country (circular mean)
  mean_month_by_country <- mean_months %>%
    mutate(angle = 2 * pi * mean_month / 12,
           sin_val = sin(angle),
           cos_val = cos(angle)) %>%
    group_by(Country) %>%
    summarise(
      mean_month = {
        mean_angle <- atan2(mean(sin_val, na.rm = TRUE),
                            mean(cos_val, na.rm = TRUE))
        circ_month <- (mean_angle * 12 / (2 * pi)) %% 12
        if (circ_month == 0) 12 else circ_month
      },
      .groups = "drop"
    )
  
  mean_month_by_country
}

# ===============================
# Apply to all variables
# ===============================

# PRCP
prcp_mean_month_br <- compute_mean_month(cpk_monthly_prcp_br_filtered, "PRCP")

# TMAX
tmax_mean_month_br <- compute_mean_month(cpk_monthly_tmax_br_filtered, "TMAX")

# TAVG
tavg_mean_month_br <- compute_mean_month(cpk_monthly_tavg_br_filtered, "TAVG")

# TMIN
tmin_mean_month_br <- compute_mean_month(cpk_monthly_tmin_br_filtered, "TMIN")

# HUM
hum_mean_month_br <- compute_mean_month(cpk_monthly_hum_br_filtered, "HUM")

# ===============================
# Adjust for hemisphere + wrap
# ===============================
adjust_months <- function(df, colname) {
  df %>%
    left_join(latitude %>% dplyr::select(Country, Latitude), by = "Country") %>%
    mutate(
      !!colname := ifelse(Latitude < 0, (!!sym(colname) + 6) %% 12, !!sym(colname)),
      !!colname := if_else(!!sym(colname) <= 3, !!sym(colname) + 12, !!sym(colname))
    ) %>%
    dplyr::select(-Latitude)
}

prcp_mean_month_br <- adjust_months(prcp_mean_month_br, "mean_month") %>%
  rename(prcp_mean_month = mean_month)

tmax_mean_month_br <- adjust_months(tmax_mean_month_br, "mean_month") %>%
  rename(tmax_mean_month = mean_month)

tavg_mean_month_br <- adjust_months(tavg_mean_month_br, "mean_month") %>%
  rename(tavg_mean_month = mean_month)

tmin_mean_month_br <- adjust_months(tmin_mean_month_br, "mean_month") %>%
  rename(tmin_mean_month = mean_month)

hum_mean_month_br <- adjust_months(hum_mean_month_br, "mean_month") %>%
  rename(hum_mean_month = mean_month)



# Join by Country and Year
br_with_pop <- br_data %>%
  inner_join(pop_long, by = c("Country.or.Area", "Year"))

# Compute absolute monthly birth rate
br_with_rate <- br_with_pop %>%
  mutate(Birth_Rate = Value / Population)

# Compute annual birth rate per 1,000
annual_birth_rate <- br_with_rate %>%
  group_by(Country.or.Area, Year) %>%
  summarise(
    total_births = sum(Value, na.rm = TRUE),
    population = first(Population),
    .groups = "drop"
  ) %>%
  filter(!is.na(population)) %>%
  mutate(birth_rate_per_1000 = (total_births / population) * 1000)


# Step 2: Keep countries with ≥ 15 years of data
country_counts <- annual_birth_rate %>%
  count(Country.or.Area) # %>%
# filter(n >= 15)

annual_birth_rate <- annual_birth_rate %>%
  filter(Country.or.Area %in% country_counts$Country.or.Area)

# Filter annual_birth_rate to 1970–2020
annual_birth_rate <- annual_birth_rate %>%
  filter(Year >= 1970, Year <= 2019)

birth_rate_avg <- annual_birth_rate %>%
  group_by(Country = Country.or.Area) %>%
  summarise(mean_birth_rate = mean(birth_rate_per_1000, na.rm = TRUE), .groups = "drop")



####### Birth month traits: mean month + amplitude ######

latitude <- periodogram %>%
  dplyr::select(Country, Latitude) %>%
  mutate(abs_latitude = abs(Latitude))

br_traits <- compute_seasonality_stats(br_data) %>%
  rename(Country = Country.or.Area, birth_mean_month = mean_month, birth_amplitude = amplitude)

# Shift birth_mean_month for Southern Hemisphere
br_traits <- br_traits %>%
  left_join(latitude %>% dplyr::select(Country, Latitude), by = "Country") %>%
  mutate(birth_mean_month = ifelse(Latitude < 0, (birth_mean_month + 6) %% 12, birth_mean_month)) %>%
  dplyr::select(-Latitude)

# Shift for correlation and plotting
br_traits <- br_traits %>%
  mutate(birth_mean_month = if_else(birth_mean_month <= 3, birth_mean_month + 12, birth_mean_month))



######## Photoperiod amplitude vs mean birth month #########
library(ggplot2)

# ---- Join the data ----
df_plot <- br_traits %>%
  inner_join(latitude_with_daylength %>% dplyr::select(Country, mean_daylength_diff, Latitude),
             by = "Country") %>%
  filter(!is.na(birth_mean_month), !is.na(mean_daylength_diff), !is.na(Latitude))

df_plot <- df_plot %>%
  mutate(
    abs_lat = abs(Latitude),
    lat_group = case_when(
      abs_lat > 43 ~ "High",
      abs_lat > 20 ~ "Mid",
      TRUE         ~ "Low"
    ),
    hemisphere = ifelse(Latitude >= 0, "Northern", "Southern")
  )

# ---- Spearman correlation ----
sp <- cor.test(df_plot$mean_daylength_diff, df_plot$birth_mean_month, method = "spearman")

p_text <- ifelse(sp$p.value < .Machine$double.xmin,
                 "p < 2.2e-308",
                 paste0("p = ", signif(sp$p.value, 2)))

corr_label <- paste0(
  "Spearman R = ", round(sp$estimate, 2), "\n", p_text
)

p <- ggplot(df_plot, aes(x = mean_daylength_diff, y = birth_mean_month)) +
  # ==== POINTS ====
geom_point(
  aes(fill = lat_group, shape = hemisphere),
  size = 1.5, alpha = 0.9,
  color = "white", stroke = 0.3,
  show.legend = TRUE
) +
  # ==== REGRESSION LINE ====
geom_smooth(
  method = "lm", se = TRUE,
  color = "black", fill = "gray70",
  linewidth = 0.4, alpha = 0.3
) +
  # ==== MANUAL SCALES ====
scale_fill_manual(
  values = c("High" = "deepskyblue2",
             "Mid"  = "yellowgreen",
             "Low"  = "darkorange2"),
  name = "Latitude"
) +
  scale_shape_manual(
    values = c("Northern" = 21, "Southern" = 24), # circle + triangle
    name = "Hemisphere"
  ) +
  guides(
    fill  = guide_legend(
      override.aes = list(shape = 21, size = 2, color = "white", stroke = 0.3)
    ),
    shape = guide_legend(
      override.aes = list(fill = "black", size = 2, color = "white", stroke = 0.3)
    )
  ) +
  # ==== AXES ====
scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 3)) +
  labs(
    x = "Photoperiod amplitude (hours)",
    y = "Mean birth month",
    title = NULL
  ) +
  # ==== THEME FORMATTING ====
theme_minimal(base_size = 5) +
  theme(
    text = element_text(size = 5),
    axis.title = element_text(size = 5),
    axis.text = element_text(size = 5),
    plot.title = element_text(size = 6, hjust = 0.5),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5),
    legend.key.size   = unit(4, "pt"),
    legend.key.width  = unit(6, "pt"),
    legend.spacing.x  = unit(1, "pt"),
    legend.spacing.y  = unit(1, "pt"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line        = element_line(linewidth = 0.5, color = "black"),
    axis.ticks       = element_line(linewidth = 0.5, color = "black"),
    axis.ticks.length = unit(2, "pt")
  ) +
  # ==== ANNOTATION ====
annotate(
  "text",
  x = max(df_plot$mean_daylength_diff, na.rm = TRUE),
  y = max(df_plot$birth_mean_month, na.rm = TRUE),
  hjust = 1, vjust = 1,
  label = corr_label,
  size = 5 / .pt,
  fontface = "italic"
)

# ---- Save ----
ggsave("Birth_figures/a.pdf",
       plot = p, width = 2.5, height = 2, device = "pdf")

################################################

####### Photoperiod amplitude calculation #########
# ---- Packages ----
library(dplyr)
library(ggplot2)

# ---- Function ----
calcPhotoperiod <- function(lat_deg, day_of_year) {
  lat_rad <- lat_deg * pi / 180
  decl <- asin(sin(-23.44 * pi / 180) * cos(2 * pi * (day_of_year + 10) / 365.24))
  solar_alt <- -0.833 * pi / 180
  
  cosH0 <- (sin(solar_alt) - sin(lat_rad) * sin(decl)) / (cos(lat_rad) * cos(decl))
  cosH0 <- pmin(pmax(cosH0, -1), 1)
  H0 <- acos(cosH0) * 180 / pi
  daylength <- (2 / 15) * H0
  return(daylength)
}

# ---- Use all countries and latitudes ----
all_countries <- periodogram %>%
  dplyr::select(Country, Latitude) %>%
  distinct() %>%
  mutate(
    abs_lat = abs(Latitude),
    lat_group = case_when(
      abs_lat > 43 ~ "High",
      abs_lat > 20 ~ "Mid",
      TRUE         ~ "Low"
    ),
    hemisphere = ifelse(Latitude >= 0, "Northern", "Southern")
  )

# ---- Compute daylength for every day ----
daylength_data <- all_countries %>%
  group_by(Country, Latitude, abs_lat, lat_group, hemisphere) %>%
  do({
    tibble(
      day = 1:365,
      # shift Southern Hemisphere by 182 days (wrap around 365)
      shifted_day = if (.$hemisphere == "Southern") ((1:365 + 182 - 1) %% 365) + 1 else 1:365,
      daylength = calcPhotoperiod(.$Latitude, 1:365)
    )
  }) %>%
  ungroup()

# ---- Colors for groups ----
lat_colors <- c(
  "High" = "deepskyblue2",
  "Mid"  = "yellowgreen",
  "Low"  = "darkorange2"
)

# ---- Plot overlay ----
p <- ggplot(daylength_data, aes(x = shifted_day, y = daylength, group = Country)) +
  geom_line(aes(color = lat_group), linewidth = 0.3, alpha = 0.7) +  # thinner lines
  scale_color_manual(values = lat_colors, guide = "none") +          # remove legend
  scale_x_continuous(limits = c(0, 365), breaks = seq(0, 360, 60)) +
  labs(
    x = "Day of year (shifted for Southern Hemisphere)",
    y = "Daylength (hours)"
  ) +
  theme_minimal(base_size = 5) +
  theme(
    text = element_text(size = 5),
    axis.title = element_text(size = 5),
    axis.text = element_text(size = 5),
    plot.title = element_text(size = 6, hjust = 0.5),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line        = element_line(linewidth = 0.4, color = "black"),
    axis.ticks       = element_line(linewidth = 0.4, color = "black"),
    axis.ticks.length = unit(2, "pt")
  )

# ---- Save (1.5 × 1 inches) ----
ggsave("Birth_figures/a_sidepanel.pdf", p, width = 1.5, height = 0.75, device = "pdf")
################################################


################## Correlations for all predictors #######################

# ============================
# Packages
# ============================
library(dplyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(purrr)
# ============================
# Categorization helper
# ============================
categorize_lat <- function(df) {
  df %>%
    mutate(
      abs_lat = abs(Latitude),
      lat_group = case_when(
        abs_lat > 40 ~ "High",
        abs_lat > 20 ~ "Mid",
        TRUE         ~ "Low"
      ),
      hemisphere = ifelse(Latitude >= 0, "Northern", "Southern")
    )
}

# ============================
# Residualization helper
# ============================
residualize <- function(df, predictor, response, control) {
  m1 <- lm(df[[predictor]] ~ df[[control]])
  pred_res <- resid(m1)
  
  m2 <- lm(df[[response]] ~ df[[control]])
  resp_res <- resid(m2)
  
  df %>%
    mutate(
      predictor_resid = pred_res,
      response_resid  = resp_res
    )
}

# ============================
# Scatterplot helper
# ============================
make_plot <- function(df, predictor_col, y_col, x_label, y_label,
                      residual = FALSE) {
  
  # ---- Spearman correlation ----
  sp <- cor.test(df[[predictor_col]], df[[y_col]], method = "spearman")
  
  # Format R with exactly 2 digits after decimal
  r_val <- formatC(sp$estimate, format = "f", digits = 2)
  
  # Handle p-value formatting
  if (sp$p.value == 0) {
    # Underflow → show as "< .Machine$double.xmin"
    p_text <- paste0("p < ", formatC(.Machine$double.xmin, format = "e", digits = 2))
  } else if (sp$p.value >= 1e-4) {
    # 2 significant digits, normal decimal
    p_text <- paste0("p = ", signif(sp$p.value, 2))
  } else {
    # Scientific notation, 2 digits
    p_text <- paste0("p = ", formatC(sp$p.value, format = "e", digits = 2))
  }
  
  corr_label <- paste0("Spearman R = ", r_val, "\n", p_text)
  
  # ---- Axis labels ----
  if (!residual) {
    # Raw plot
    x_lab <- x_label
    y_lab <- "Mean birth month"
  } else {
    # Residualized plot
    unit_part <- sub(".*(\\(.*\\))$", "\\1", x_label)   # keep parentheses part if present
    main_part <- sub("(\\s*\\(.*\\))$", "", x_label)    # remove parentheses part
    main_lower <- tolower(main_part)                   # lowercase main text
    
    if (unit_part == x_label) {
      x_lab <- paste("Residual", main_lower)
    } else {
      x_lab <- paste("Residual", paste0(main_lower, " ", unit_part))
    }
    
    y_lab <- "Residual mean birth month"
  }
  
  # ---- Plot ----
  p <- ggplot(df, aes_string(x = predictor_col, y = y_col)) +
    geom_point(aes(fill = lat_group, shape = hemisphere),
               size = 1.5, alpha = 0.5,
               color = "white", stroke = 0.3) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "black", fill = "gray70",
                linewidth = 0.7, alpha = 0.3) +
    scale_fill_manual(
      values = c("High" = "deepskyblue2",
                 "Mid"  = "yellowgreen",
                 "Low"  = "darkorange2")
    ) +
    scale_shape_manual(
      values = c("Northern" = 21, "Southern" = 24)
    ) +
    labs(x = x_lab, y = y_lab) +
    theme_minimal(base_size = 5) +
    theme(
      text = element_text(size = 5),
      axis.title = element_text(size = 5),
      axis.text = element_text(size = 5),
      plot.title = element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 5),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line        = element_line(linewidth = 0.5, color = "black"),
      axis.ticks       = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.length = unit(2, "pt")
    ) +
    annotate("text",
             x = max(df[[predictor_col]], na.rm = TRUE),
             y = max(df[[y_col]], na.rm = TRUE),
             hjust = 1, vjust = 1,
             label = corr_label,
             size = 5 / .pt,
             fontface = "italic")
  
  # Restrict y only for raw plots
  if (!residual) {
    p <- p + scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 3))
  }
  
  return(p)
}



# ============================
# Generate plots + correlation tables
# ============================
generate_plots_and_corrs <- function(predictor_dfs, predictor_labels,
                                     response_df, y_col, y_label,
                                     raw_pdf, residual_pdf) {
  predictors <- names(predictor_dfs)
  raw_plots <- list()
  resid_plots <- list()
  raw_corrs <- list()
  resid_corrs <- list()
  
  for (pred_name in predictors) {
    df <- predictor_dfs[[pred_name]]
    predictor_col <- names(df)[2]
    label <- pred_name                       # scatterplot label (with units)
    clean_label <- predictor_labels[[pred_name]]
    
    df_joined <- response_df %>%
      inner_join(df, by = "Country") %>%
      inner_join(latitude_with_daylength %>%
                   dplyr::select(Country, Latitude, mean_daylength_diff),
                 by = "Country") %>%
      filter(!is.na(.data[[predictor_col]]), !is.na(.data[[y_col]]))
    
    if (nrow(df_joined) < 3) next
    df_joined <- categorize_lat(df_joined)
    
    # --- Raw correlation ---
    sp_raw <- cor.test(df_joined[[predictor_col]], df_joined[[y_col]], method = "spearman")
    p_raw <- ifelse(sp_raw$p.value == 0, .Machine$double.xmin, sp_raw$p.value)
    raw_corrs[[pred_name]] <- tibble(
      Predictor = clean_label,
      SpearmanR = unname(sp_raw$estimate),
      p_value   = p_raw,
      neglog10_p = -log10(p_raw),
      n         = length(df_joined[[predictor_col]])
    )
    
    raw_plots[[pred_name]] <- make_plot(
      df_joined, predictor_col, y_col,
      x_label = label, y_label = y_label,
      residual = FALSE
    )
    
    # --- Residualized correlation ---
    df_resid <- residualize(df_joined, predictor_col, y_col, "mean_daylength_diff")
    sp_resid <- cor.test(df_resid$predictor_resid, df_resid$response_resid, method = "spearman")
    p_resid <- ifelse(sp_resid$p.value == 0, .Machine$double.xmin, sp_resid$p.value)
    resid_corrs[[pred_name]] <- tibble(
      Predictor = clean_label,
      SpearmanR = unname(sp_resid$estimate),
      p_value   = p_resid,
      neglog10_p = -log10(p_resid),
      n         = length(df_resid$predictor_resid)
    )
    
    resid_plots[[pred_name]] <- make_plot(
      df_resid, "predictor_resid", "response_resid",
      x_label = label, y_label = y_label,
      residual = TRUE
    )
  }
  
  # Save scatterplots
  pdf(raw_pdf, width = 1.5, height = 1.5)
  walk(raw_plots, print)
  dev.off()
  
  pdf(residual_pdf, width = 1.5, height = 1.5)
  walk(resid_plots, print)
  dev.off()
  
  # Build correlation tables
  raw_corrs_df <- bind_rows(raw_corrs) %>%
    mutate(p_adj = p.adjust(p_value, method = "BH"),
           neglog10_padj = -log10(ifelse(p_adj == 0, .Machine$double.xmin, p_adj)))
  resid_corrs_df <- bind_rows(resid_corrs) %>%
    mutate(p_adj = p.adjust(p_value, method = "BH"),
           neglog10_padj = -log10(ifelse(p_adj == 0, .Machine$double.xmin, p_adj)))
  
  return(list(raw = raw_corrs_df, residual = resid_corrs_df))
}


# ============================
# Example usage
# ============================

predictor_dfs <- list(
  "Temperature (°C)" = cpk_tmax_br,
  "Temperature Amp (°C)" = cpk_tmax_amp_br,
  "Temperature Phase (month)" = tmax_mean_month_br,
  "Precipitation (mm)" = cpk_prcp_br,
  "Precipitation Amp (mm)" = cpk_prcp_amp_br,
  "Precipitation Phase (month)" = prcp_mean_month_br,
  "Humidity (%)" = cpk_hum_br,
  "Humidity Amp (%)" = cpk_hum_amp_br,
  "Humidity Phase (month)" = hum_mean_month_br,
  "GDP per capita (log10 $)" = gdp_pc_avg_br %>% dplyr::select(Country, log10_gdp_pc),
  "Population (log10 people)" = population_avg_br %>% dplyr::select(Country, log10_population),
  "Birth Rate (%)" = birth_rate_avg,
  "Contraception (%)" = contraception_avg_br
)

# Parallel list of clean names
predictor_labels <- c(
  "Temperature (°C)" = "Temperature",
  "Temperature Amp (°C)" = "Temperature Amp",
  "Temperature Phase (month)" = "Temperature Phase",
  "Precipitation (mm)" = "Precipitation",
  "Precipitation Amp (mm)" = "Precipitation Amp",
  "Precipitation Phase (month)" = "Precipitation Phase",
  "Humidity (%)" = "Humidity",
  "Humidity Amp (%)" = "Humidity Amp",
  "Humidity Phase (month)" = "Humidity Phase",
  "GDP per capita (log10 $)" = "GDP per capita",
  "Population (log10 people)" = "Population",
  "Birth Rate (%)" = "Birth Rate",
  "Contraception (%)" = "Contraception"
)

# Generate scatterplots + correlation tables
corr_results <- generate_plots_and_corrs(
  predictor_dfs, predictor_labels,
  response_df = br_traits,
  y_col = "birth_mean_month",
  y_label = "Mean birth phase (month)",
  raw_pdf = "Birth_figures/birth_month_vs_predictors.pdf",
  residual_pdf = "Birth_figures/birth_month_vs_predictors_residuals.pdf"
)

##################################################

################## Volcano plots for all predictors #######################
library(ggplot2)
library(ggrepel)
library(grid)

# ----------------------------
# Volcano plot (generic)
# ----------------------------
make_volcano <- function(df,
                         x_limits = c(-1, 1),
                         y_limits = NULL) {
  ggplot(df, aes(x = SpearmanR, y = neglog10_padj,
                 color = p_adj < 0.001, label = Predictor)) +
    geom_point(size = 0.7, alpha = 0.9) +  # smaller points for 1.5x1.5
    # horizontal thresholds
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "steelblue", linewidth = 0.2, alpha = 0.5) +
    geom_hline(yintercept = -log10(0.01), linetype = "dashed", color = "darkorange", linewidth = 0.2, alpha = 0.5) +
    geom_hline(yintercept = -log10(0.001), linetype = "dashed", color = "firebrick", linewidth = 0.2, alpha = 0.5) +
    # vertical line at Spearman = 0
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.2, alpha = 0.5) +
    # variable labels (match axis text size)
    geom_text_repel(
      size = 1.5,
      max.overlaps = Inf,
      segment.size = 0.2,
      point.padding = unit(0.05, "lines"),   # closer to the point
      box.padding   = unit(0.05, "lines"),   # less padding around label box
      min.segment.length = 0,
      alpha = 0.75
    ) +
    # ~1.4 here ≈ 5 pt text, after rescaling
    scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "firebrick")) +
    scale_x_continuous(limits = x_limits,
                       labels = function(x) sprintf("%.1f", x)) +
    scale_y_continuous(limits = y_limits,
                       labels = function(x) sprintf("%.0f", x)) +
    labs(x = "Spearman R",
         y = expression(-log[10]("p-value"))) +
    theme_minimal(base_size = 5) +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(linewidth = 0.3, color = "black"),
      axis.ticks = element_line(linewidth = 0.3, color = "black"),
      axis.ticks.length = unit(1, "pt"),
      plot.title = element_blank()
    )
}

# ----------------------------
# Wrapper to generate plots
# ----------------------------
generate_volcano_plots <- function(corr_results,
                                   raw_low_pdf  = "Birth_figures/volcano_raw_low.pdf",
                                   raw_high_pdf = "Birth_figures/volcano_raw_high.pdf",
                                   resid_pdf    = "Birth_figures/volcano_residual.pdf") {
  shared_xlim <- c(-1, 1)
  
  # raw plots: low and high ranges
  volcano_raw_low  <- make_volcano(corr_results$raw,
                                   x_limits = shared_xlim,
                                   y_limits = c(0, 7))
  volcano_raw_high <- make_volcano(corr_results$raw,
                                   x_limits = shared_xlim,
                                   y_limits = c(305, 310))
  
  # residual plot
  volcano_resid <- make_volcano(corr_results$residual,
                                x_limits = shared_xlim)
  
  # save: all at 1.5 x 1.5 inches
  pdf(raw_low_pdf, width = 1.5, height = 1)
  print(volcano_raw_low)
  dev.off()
  
  pdf(raw_high_pdf, width = 1.5, height = 0.5)
  print(volcano_raw_high)
  dev.off()
  
  pdf(resid_pdf, width = 1.5, height = 1.5)
  print(volcano_resid)
  dev.off()
  
  return(list(raw_low = volcano_raw_low,
              raw_high = volcano_raw_high,
              residual = volcano_resid))
}


# Generate volcano plots
volcano_plots <- generate_volcano_plots(
  corr_results
)

##################################################


############### Birth & Predictor Correlation Analyses per year ###############
compute_birth_by_year <- function(df, value_col = "Normalized.Value") {
  df <- df %>% mutate(Year = as.integer(Year))
  
  # Prefer Month_num if it exists; otherwise derive 1..12 from Month
  if ("Month_num" %in% names(df)) {
    df <- df %>% mutate(.m = as.integer(Month_num))
  } else {
    month_levels <- c("January","February","March","April","May","June",
                      "July","August","September","October","November","December")
    if (is.numeric(df$Month)) {
      df <- df %>% mutate(.m = as.integer(Month))
    } else {
      df <- df %>% mutate(.m = match(as.character(Month), month_levels))
    }
  }
  
  # Keep only complete years (12 months)
  complete_years <- df %>%
    group_by(Country.or.Area, Year) %>%
    filter(n() == 12) %>%
    ungroup()
  
  # ---- 1) Mean month (circular) ----
  mean_by_year <- complete_years %>%
    group_by(Country.or.Area, Year) %>%
    summarise(
      birth_mean_month = {
        ang <- 2 * pi * (.m - 1) / 12
        w   <- .data[[value_col]]
        mth <- (atan2(sum(sin(ang) * w), sum(cos(ang) * w)) * 12 / (2 * pi)) %% 12
        if (mth == 0) 12 else mth
      },
      .groups = "drop"
    )
  
  # ---- 2) Amplitude per year ----
  amp_by_year <- complete_years %>%
    group_by(Country.or.Area, Year) %>%
    summarise(
      amplitude = (max(.data[[value_col]], na.rm = TRUE) -
                     min(.data[[value_col]], na.rm = TRUE)) / 2,
      .groups = "drop"
    )
  
  # ---- 3) Combine mean month + amplitude ----
  birth_by_year <- mean_by_year %>%
    inner_join(amp_by_year, by = c("Country.or.Area", "Year")) %>%
    rename(Country = Country.or.Area) %>%
    left_join(periodogram %>% dplyr::select(Country, Latitude), by = "Country") %>%
    mutate(
      birth_mean_month = ifelse(Latitude < 0, (birth_mean_month + 6) %% 12, birth_mean_month),
      birth_mean_month = ifelse(birth_mean_month == 0, 12, birth_mean_month)
    ) %>%
    dplyr::select(-Latitude)
  
  return(birth_by_year)
}

# Call it like this:
birth_by_year <- compute_birth_by_year(br_data, value_col = "Normalized.Value")
death_by_year <- compute_birth_by_year(mr_data, value_col = "Normalized.Value")

# =========================
# 2) Predictors per country–year
# =========================

# ---- TAVG mean & amp ----
tavg_by_year_br <- cpk_tavg_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(TAVG = mean(TAVG, na.rm = TRUE), .groups = "drop")

tavg_amp_by_year_br <- cpk_monthly_tavg_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(TAVG_AMP = max(TAVG, na.rm = TRUE) - min(TAVG, na.rm = TRUE), .groups = "drop")

# ---- TMIN mean & amp ----
tmin_by_year_br <- cpk_tmin_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(TMIN = mean(TMIN, na.rm = TRUE), .groups = "drop")

tmin_amp_by_year_br <- cpk_monthly_tmin_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(TMIN_AMP = max(TMIN, na.rm = TRUE) - min(TMIN, na.rm = TRUE), .groups = "drop")

# ---- TMAX mean & amp ----
tmax_by_year_br <- cpk_tmax_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(TMAX = mean(TMAX, na.rm = TRUE), .groups = "drop")

tmax_amp_by_year_br <- cpk_monthly_tmax_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(TMAX_AMP = max(TMAX, na.rm = TRUE) - min(TMAX, na.rm = TRUE), .groups = "drop")

# ---- PRCP mean & amp ----
prcp_by_year_br <- cpk_prcp_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(PRCP = mean(PRCP, na.rm = TRUE), .groups = "drop")

prcp_amp_by_year_br <- cpk_monthly_prcp_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(PRCP_AMP = max(PRCP, na.rm = TRUE) - min(PRCP, na.rm = TRUE), .groups = "drop")

# ---- HUM mean & amp ----
hum_by_year_br <- cpk_hum_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(HUM = mean(HUM, na.rm = TRUE), .groups = "drop")

hum_amp_by_year_br <- cpk_monthly_hum_br_filtered %>%
  group_by(Country, Year) %>%
  summarise(HUM_AMP = max(HUM, na.rm = TRUE) - min(HUM, na.rm = TRUE), .groups = "drop")

# ============================
# Generic circular mean month per year
# ============================
compute_mean_month_by_year <- function(df, value_col) {
  df <- df %>% mutate(Year = as.integer(Year))
  
  # Keep only complete years (12 months)
  complete_years <- df %>%
    group_by(Country, Year) %>%
    filter(n() == 12) %>%
    ungroup()
  
  out <- complete_years %>%
    group_by(Country, Year) %>%
    summarise(
      mean_month = {
        ang <- 2 * pi * (Month - 1) / 12
        w   <- .data[[value_col]]
        mth <- (atan2(sum(sin(ang) * w), sum(cos(ang) * w)) * 12 / (2 * pi)) %% 12
        if (mth == 0) 12 else mth
      },
      .groups = "drop"
    )
  
  out
}

# ============================
# Adjust for hemisphere + wrap
# ============================
adjust_months <- function(df, colname) {
  df %>%
    left_join(periodogram %>% dplyr::select(Country, Latitude), by = "Country") %>%
    mutate(
      !!colname := ifelse(Latitude < 0, (!!sym(colname) + 6) %% 12, !!sym(colname)),
      !!colname := ifelse(!!sym(colname) == 0, 12, !!sym(colname)),
      !!colname := if_else(!!sym(colname) <= 3, !!sym(colname) + 12, !!sym(colname))
    ) %>%
    dplyr::select(-Latitude)
}

# ============================
# Apply to PRCP, TMIN, TAVG, TMAX, HUM
# ============================
# PRCP
prcp_mean_month_by_year_br <- compute_mean_month_by_year(cpk_monthly_prcp_br_filtered, "PRCP") %>%
  adjust_months("mean_month") %>%
  rename(prcp_mean_month = mean_month)

# TMIN
tmin_mean_month_by_year_br <- compute_mean_month_by_year(cpk_monthly_tmin_br_filtered, "TMIN") %>%
  adjust_months("mean_month") %>%
  rename(tmin_mean_month = mean_month)

# TAVG
tavg_mean_month_by_year_br <- compute_mean_month_by_year(cpk_monthly_tavg_br_filtered, "TAVG") %>%
  adjust_months("mean_month") %>%
  rename(tavg_mean_month = mean_month)

# TMAX
tmax_mean_month_by_year_br <- compute_mean_month_by_year(cpk_monthly_tmax_br_filtered, "TMAX") %>%
  adjust_months("mean_month") %>%
  rename(tmax_mean_month = mean_month)

# HUM
hum_mean_month_by_year_br <- compute_mean_month_by_year(cpk_monthly_hum_br_filtered, "HUM") %>%
  adjust_months("mean_month") %>%
  rename(hum_mean_month = mean_month)


# ---- GDP per capita (log10), Population (log10), Contraception, Birth rate ----
gdp_pc_by_year_br <- gdp_pc_long_filtered_br %>%
  rename(Country = Country, Year = Year) %>%
  dplyr::select(Country, Year, log10_GDP_PC)

pop_by_year_br <- pop_long_filtered_br %>%
  rename(Country = Country.or.Area) %>%
  dplyr::select(Country, Year, log10_Population)

contraception_by_year_br <- contraception_filtered_br %>%
  dplyr::select(Country, Year, Value) %>%
  rename(Contraception = Value)

birth_rate_by_year_br <- annual_birth_rate %>%
  rename(Country = Country.or.Area) %>%
  dplyr::select(Country, Year, birth_rate_per_1000)





all_br_traits <- compute_seasonality_stats(all_br_data) %>%
  rename(Country = Country.or.Area, birth_mean_month = mean_month, birth_amplitude = amplitude)

# Shift birth_mean_month for Southern Hemisphere
all_br_traits <- all_br_traits %>%
  left_join(latitude %>% dplyr::select(Country, Latitude), by = "Country") %>%
  mutate(birth_mean_month = ifelse(Latitude < 0, (birth_mean_month + 6) %% 12, birth_mean_month)) %>%
  dplyr::select(-Latitude)

all_mr_traits <- compute_seasonality_stats(all_mr_data) %>%
  rename(Country = Country.or.Area, death_mean_month = mean_month, birth_amplitude = amplitude)

# Shift birth_mean_month for Southern Hemisphere
all_mr_traits <- all_mr_traits %>%
  left_join(latitude %>% dplyr::select(Country, Latitude), by = "Country") %>%
  mutate(death_mean_month = ifelse(Latitude < 0, (death_mean_month + 6) %% 12, death_mean_month)) %>%
  dplyr::select(-Latitude)


latitude_with_daylength_by_year <- birth_by_year %>%
  distinct(Country, Year) %>% 
  inner_join(latitude_with_daylength %>%
               select(Country, mean_daylength_diff),
             by = "Country") %>%
  rename(mean_daylength_diff_by_year = mean_daylength_diff)

################## Correlations for all predictors by year #######################
# ============================
# Packages
# ============================
library(dplyr)
library(ggplot2)
library(ggrepel)
library(purrr)

# ============================
# Categorization helper
# ============================
categorize_lat <- function(df) {
  df %>%
    mutate(
      abs_lat = abs(Latitude),
      lat_group = case_when(
        abs_lat > 40 ~ "High",
        abs_lat > 20 ~ "Mid",
        TRUE         ~ "Low"
      ),
      hemisphere = ifelse(Latitude >= 0, "Northern", "Southern")
    )
}

# ============================
# Residualization helper
# ============================
residualize <- function(df, predictor, response, control) {
  m1 <- lm(df[[predictor]] ~ df[[control]])
  pred_res <- resid(m1)
  
  m2 <- lm(df[[response]] ~ df[[control]])
  resp_res <- resid(m2)
  
  df %>%
    mutate(
      predictor_resid = pred_res,
      response_resid  = resp_res
    )
}

# ============================
# Helper: format p-values
# ============================
format_pval <- function(p) {
  if (p < .Machine$double.xmin) {
    return("p < 2.2e-308")
  } else if (p < 0.0001) {
    return(paste0("p = ", formatC(p, format = "e", digits = 2)))
  } else {
    return(paste0("p = ", signif(p, 3)))
  }
}

# ============================
# Scatterplot helpers
# ============================
# ============================
# Raw scatterplot
# ============================
# ============================
# Raw scatterplot
# ============================
make_raw_plot <- function(df, predictor_col, predictor_label) {
  sp <- cor.test(df[[predictor_col]], df$birth_mean_month, method = "spearman")
  p_text <- format_pval(sp$p.value)
  corr_label <- paste0("Spearman R = ", round(sp$estimate, 2), "\n", p_text)
  
  ggplot(df, aes(x = .data[[predictor_col]], y = birth_mean_month, color = Year)) +
    geom_point(size = 0.25, alpha = 0.15) +   # plain points
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "black", fill = "gray70", linewidth = 0.7, alpha = 0.3) +
    scale_color_viridis_c() +
    scale_y_continuous(breaks = seq(0, 12, 3), limits = c(0, 12)) +
    labs(x = predictor_label, y = "Mean Birth Month") +
    theme_minimal(base_size = 5) +
    theme(
      text = element_text(size = 5),
      axis.title = element_text(size = 5),
      axis.text = element_text(size = 5),
      plot.title = element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 5),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(linewidth = 0.5, color = "black"),
      axis.ticks = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.length = unit(2, "pt")
    ) +
    annotate("text",
             x = max(df[[predictor_col]], na.rm = TRUE),
             y = max(df$birth_mean_month, na.rm = TRUE),
             hjust = 1, vjust = 1,
             label = corr_label,
             size = 5 / .pt,   # convert 5 pt to mm
             fontface = "italic")
}

# ============================
# Residual scatterplot
# ============================
make_resid_plot <- function(df, predictor_col, predictor_label) {
  resid_y <- lm(birth_mean_month ~ mean_daylength_diff, data = df)$residuals
  resid_x <- lm(df[[predictor_col]] ~ df$mean_daylength_diff)$residuals
  df_resid <- data.frame(resid_x, resid_y, Year = df$Year)
  
  sp <- cor.test(df_resid$resid_x, df_resid$resid_y, method = "spearman")
  p_text <- format_pval(sp$p.value)
  corr_label <- paste0("Spearman R = ", round(sp$estimate, 2), "\n", p_text)
  
  ggplot(df_resid, aes(x = resid_x, y = resid_y, color = Year)) +
    geom_point(size = 0.25, alpha = 0.15) +   # plain points
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "black", fill = "gray70", linewidth = 0.7, alpha = 0.3) +
    scale_color_viridis_c() +
    labs(x = paste("Residual", predictor_label),
         y = "Residual Mean Birth Month") +
    theme_minimal(base_size = 5) +
    theme(
      text = element_text(size = 5),
      axis.title = element_text(size = 5),
      axis.text = element_text(size = 5),
      plot.title = element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 5),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(linewidth = 0.5, color = "black"),
      axis.ticks = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.length = unit(2, "pt")
    ) +
    annotate("text",
             x = max(df_resid$resid_x, na.rm = TRUE),
             y = max(df_resid$resid_y, na.rm = TRUE),
             hjust = 1, vjust = 1,
             label = corr_label,
             size = 5 / .pt,
             fontface = "italic")
}



# ============================
# Per-year correlation generator
# ============================
generate_yearly_corrs <- function(predictor_dfs, predictor_labels, birth_df, latitude_with_daylength) {
  raw_corrs <- list()
  resid_corrs <- list()
  raw_plots <- list()
  resid_plots <- list()
  
  for (pred_name in names(predictor_dfs)) {
    df_pred <- predictor_dfs[[pred_name]]
    predictor_col <- names(df_pred)[3]  # assumes cols = Country, Year, value
    predictor_label <- predictor_labels[[pred_name]]
    
    df_joined <- birth_df %>%
      inner_join(df_pred, by = c("Country", "Year")) %>%
      inner_join(latitude_with_daylength %>%
                   dplyr::select(Country, mean_daylength_diff),
                 by = "Country") %>%
      filter(!is.na(birth_mean_month),
             !is.na(.data[[predictor_col]]),
             !is.na(mean_daylength_diff))
    
    if (nrow(df_joined) < 5) next
    
    # --- Raw correlation ---
    sp_raw <- cor.test(df_joined[[predictor_col]], df_joined$birth_mean_month, method = "spearman")
    p_raw <- ifelse(sp_raw$p.value == 0, .Machine$double.xmin, sp_raw$p.value)
    raw_corrs[[predictor_label]] <- tibble(
      Predictor = predictor_label,
      SpearmanR = unname(sp_raw$estimate),
      p_value   = p_raw,
      neglog10_p = -log10(p_raw),
      n         = nrow(df_joined)
    )
    raw_plots[[predictor_label]] <- make_raw_plot(df_joined, predictor_col, predictor_label)
    
    # --- Residualized correlation ---
    df_resid <- residualize(df_joined, predictor_col, "birth_mean_month", "mean_daylength_diff")
    sp_resid <- cor.test(df_resid$predictor_resid, df_resid$response_resid, method = "spearman")
    p_resid <- ifelse(sp_resid$p.value == 0, .Machine$double.xmin, sp_resid$p.value)
    resid_corrs[[predictor_label]] <- tibble(
      Predictor = predictor_label,
      SpearmanR = unname(sp_resid$estimate),
      p_value   = p_resid,
      neglog10_p = -log10(p_resid),
      n         = nrow(df_resid)
    )
    resid_plots[[predictor_label]] <- make_resid_plot(df_joined, predictor_col, predictor_label)
  }
  
  raw_df <- bind_rows(raw_corrs) %>%
    mutate(p_adj = p.adjust(p_value, method = "BH"),
           neglog10_padj = -log10(ifelse(p_adj == 0, .Machine$double.xmin, p_adj)))
  resid_df <- bind_rows(resid_corrs) %>%
    mutate(p_adj = p.adjust(p_value, method = "BH"),
           neglog10_padj = -log10(ifelse(p_adj == 0, .Machine$double.xmin, p_adj)))
  
  return(list(raw = raw_df, residual = resid_df,
              raw_plots = raw_plots, resid_plots = resid_plots))
}

# ============================
# Example usage
# ============================
predictor_dfs <- list(
  "Temperature (°C)"          = tmax_by_year_br,
  "Temperature Amp (°C)"      = tmax_amp_by_year_br,
  "Temperature Phase (month)" = tmax_mean_month_by_year_br,
  "Precipitation (mm)"        = prcp_by_year_br,
  "Precipitation Amp (mm)"    = prcp_amp_by_year_br,
  "Precipitation Phase (month)" = prcp_mean_month_by_year_br,
  "Humidity (%)"              = hum_by_year_br,
  "Humidity Amp (%)"          = hum_amp_by_year_br,
  "Humidity Phase (month)"    = hum_mean_month_by_year_br,
  "GDP per capita (log10 $)"  = gdp_pc_by_year_br %>% dplyr::select(Country, Year, log10_GDP_PC),
  "Population"                = pop_by_year_br %>% dplyr::select(Country, Year, log10_Population),
  "Birth Rate (%)"            = birth_rate_by_year_br %>% dplyr::select(Country, Year, birth_rate_per_1000),
  "Contraception (%)"         = contraception_by_year_br,
  "Photoperiod Amp (h)" = latitude_with_daylength_by_year
)

predictor_labels <- c(
  "Temperature (°C)"          = "Temperature",
  "Temperature Amp (°C)"      = "Temperature Amp",
  "Temperature Phase (month)" = "Temperature Phase",
  "Precipitation (mm)"        = "Precipitation",
  "Precipitation Amp (mm)"    = "Precipitation Amp",
  "Precipitation Phase (month)" = "Precipitation Phase",
  "Humidity (%)"              = "Humidity",
  "Humidity Amp (%)"          = "Humidity Amp",
  "Humidity Phase (month)"    = "Humidity Phase",
  "GDP per capita (log10 $)"  = "GDP per capita",
  "Population"                = "Population",
  "Birth Rate (%)"            = "Birth Rate",
  "Contraception (%)"         = "Contraception",
  "Photoperiod Amp (h)" = "Photoperiod Amp"
)

# Generate correlation results and plots
yearly_corrs <- generate_yearly_corrs(
  predictor_dfs, predictor_labels,
  birth_df = birth_by_year,
  latitude_with_daylength = latitude_with_daylength
)

# Save scatterplots
pdf("Birth_figures/birth_month_vs_predictors_by_year.pdf", width = 1.5, height = 1.5)
walk(yearly_corrs$raw_plots, print)
dev.off()

pdf("Birth_figures/birth_month_residuals_by_year.pdf", width = 1.5, height = 1.5)
walk(yearly_corrs$resid_plots, print)
dev.off()

##################################################################

################## Volcano plots for all predictors by year ##################

library(ggplot2)
library(ggrepel)
library(grid)

# ----------------------------
# Volcano plot (generic helper)
# ----------------------------
make_volcano <- function(df,
                         x_limits = c(-1, 1),
                         y_limits = NULL,
                         y_breaks = NULL) {
  ggplot(df, aes(x = SpearmanR, y = neglog10_padj,
                 color = p_adj < 0.001, label = Predictor)) +
    geom_point(size = 0.7, alpha = 0.9) +
    # thresholds
    geom_hline(yintercept = -log10(0.05),
               linetype = "dashed", color = "steelblue",
               linewidth = 0.2, alpha = 0.5) +
    geom_hline(yintercept = -log10(0.01),
               linetype = "dashed", color = "darkorange",
               linewidth = 0.2, alpha = 0.5) +
    geom_hline(yintercept = -log10(0.001),
               linetype = "dashed", color = "firebrick",
               linewidth = 0.2, alpha = 0.5) +
    geom_vline(xintercept = 0,
               linetype = "dashed", color = "black",
               linewidth = 0.2, alpha = 0.5) +
    geom_text_repel(
      size = 1.5,
      max.overlaps = Inf,
      segment.size = 0.2,
      point.padding = unit(0.05, "lines"),
      box.padding   = unit(0.05, "lines"),
      min.segment.length = 0,
      alpha = 0.75
    ) +
    scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "firebrick")) +
    scale_x_continuous(limits = x_limits,
                       labels = function(x) sprintf("%.1f", x)) +
    scale_y_continuous(limits = y_limits,
                       breaks = y_breaks,
                       labels = function(x) sprintf("%.0f", x)) +
    labs(x = "Spearman R",
         y = expression(-log[10]("p-value"))) +
    theme_minimal(base_size = 5) +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(linewidth = 0.3, color = "black"),
      axis.ticks = element_line(linewidth = 0.3, color = "black"),
      axis.ticks.length = unit(1, "pt"),
      axis.text.x = element_text(size = 5),  # keep x numbers
      axis.text.y = element_blank(),         # hide y numbers
      plot.title = element_blank()
    )
}


# ----------------------------
# Wrapper for yearly plots
# ----------------------------
generate_yearly_volcano <- function(yearly_corrs,
                                    raw_low_pdf  = "Birth_figures/yearly_volcano_raw_low.pdf",
                                    raw_high_pdf = "Birth_figures/yearly_volcano_raw_high.pdf",
                                    resid_low_pdf  = "Birth_figures/yearly_volcano_resid_low.pdf",
                                    resid_high_pdf = "Birth_figures/yearly_volcano_resid_high.pdf") {
  shared_xlim <- c(-1, 1)
  
  # ---- RAW ----
  volcano_raw_low <- make_volcano(
    yearly_corrs$raw,
    x_limits = shared_xlim,
    y_limits = c(0, 5),
    y_breaks = seq(0, 5, 2)          # ticks every 2
  )
  volcano_raw_high <- make_volcano(
    yearly_corrs$raw,
    x_limits = shared_xlim,
    y_limits = c(30, 330),
    y_breaks = seq(30, 330, 60)      # ticks every 60
  )
  
  # ---- RESIDUAL ----
  volcano_resid_low <- make_volcano(
    yearly_corrs$residual,
    x_limits = shared_xlim,
    y_limits = c(0, 40),
    y_breaks = seq(0, 40, 10)        # ticks every 10
  )
  volcano_resid_high <- make_volcano(
    yearly_corrs$residual,
    x_limits = shared_xlim,
    y_limits = c(120, 124),
    y_breaks = seq(120, 124, 2)      # ticks every 2
  )
  
  # ---- SAVE ----
  pdf(raw_low_pdf, width = 1.5, height = 0.5)
  print(volcano_raw_low)
  dev.off()
  
  pdf(raw_high_pdf, width = 1.5, height = 1)
  print(volcano_raw_high)
  dev.off()
  
  pdf(resid_low_pdf, width = 1.5, height = 1)
  print(volcano_resid_low)
  dev.off()
  
  pdf(resid_high_pdf, width = 1.5, height = 0.5)
  print(volcano_resid_high)
  dev.off()
  
  return(list(
    raw_low = volcano_raw_low,
    raw_high = volcano_raw_high,
    resid_low = volcano_resid_low,
    resid_high = volcano_resid_high
  ))
}


# ----------------------------
# Run it
# ----------------------------
yearly_volcano_plots <- generate_yearly_volcano(yearly_corrs)

##################################################################


################## Slope of birth rate - temporal shifts ################## 

compute_seasonality_stats_slope <- function(df, latitude_df, type, value_col = "Normalized.Value") {
  df <- df %>% mutate(Year = as.integer(Year))
  
  # Keep only complete years (12 months per year per country)
  complete_years <- df %>%
    group_by(Country.or.Area, Year) %>%
    filter(n() == 12) %>%
    ungroup()
  
  # Compute mean month per year per country (circular mean)
  mean_months <- complete_years %>%
    group_by(Country.or.Area, Year) %>%
    summarise(
      mean_month = {
        months <- Month_num
        values <- .data[[value_col]]
        angles <- 2 * pi * (months - 1) / 12
        mean_angle <- atan2(sum(sin(angles) * values), sum(cos(angles) * values))
        circ_month <- (mean_angle * 12 / (2 * pi)) %% 12
        if (circ_month == 0) 12 else circ_month
      },
      .groups = "drop"
    )
  
  # Apply 6-month shift for Southern Hemisphere
  mean_months <- mean_months %>%
    left_join(latitude_df %>% dplyr::select(Country.or.Area = Country, Latitude), by = "Country.or.Area") %>%
    mutate(mean_month = ifelse(Latitude < 0, (mean_month + 6) %% 12, mean_month)) %>%
    dplyr::select(-Latitude)
  
  # Compute amplitude per year per country
  amplitude <- complete_years %>%
    group_by(Country.or.Area, Year, Month) %>%
    summarise(month_median = median(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    group_by(Country.or.Area, Year) %>%
    summarise(
      amplitude = (max(month_median) - min(month_median)) / 2,
      .groups = "drop"
    )
  
  # Join both time series
  full_df <- inner_join(mean_months, amplitude, by = c("Country.or.Area", "Year"))
  
  
  ##### FACET PLOTS ########
  
  # --- Plot for mean_month ---
  month_plot <- ggplot(full_df, aes(x = Year, y = mean_month)) +
    geom_point(color = "black", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +
    facet_wrap(~ Country.or.Area) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      panel.spacing = unit(1, "lines")
    ) +
    labs(
      x = "Year",
      y = "Mean Birth Month",
      title = "Yearly Trend of Mean Birth Month by Country"
    ) +
    scale_y_continuous(limits = c(0, 12))
  
  ggsave(paste0(type, "_month_temporal_trends_by_country.pdf"), month_plot, width = 30, height = 40, limitsize = FALSE)
  
  
  # --- Plot for amplitude ---
  amp_plot <- ggplot(full_df, aes(x = Year, y = amplitude)) +
    geom_point(color = "black", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +
    facet_wrap(~ Country.or.Area) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      panel.spacing = unit(1, "lines")
    ) +
    labs(
      x = "Year",
      y = "Amplitude",
      title = "Yearly Trend of Birth Amplitude by Country"
    ) +
    scale_y_continuous(
      limits = c(min(full_df$amplitude, na.rm = TRUE), max(full_df$amplitude, na.rm = TRUE))
    )
  
  ggsave(paste0(type, "_amplitude_temporal_trends_by_country.pdf"), amp_plot, width = 30, height = 40, limitsize = FALSE)
  
  
  # Compute slope of mean_month and amplitude over time per country
  month_slopes <- full_df %>%
    group_by(Country.or.Area) %>%
    group_modify(~ tibble(month_slope = compute_circular_slope(.x))) %>%
    ungroup()
  
  amp_slopes <- full_df %>%
    group_by(Country.or.Area) %>%
    summarise(amplitude_slope = coef(lm(amplitude ~ Year))[["Year"]], .groups = "drop")
  
  slopes <- left_join(month_slopes, amp_slopes, by = "Country.or.Area")
  
  return(slopes)
}

############# Compute circular slope function #############
compute_circular_slope <- function(df) {
  # Assumes df has Year and mean_month (1–12)
  
  # Convert month to angle (radians)
  theta <- 2 * pi * df$mean_month / 12
  t <- df$Year - mean(df$Year)  # center time for stability
  
  # Regress sin and cos on time
  fit_s <- lm(sin(theta) ~ t)
  fit_c <- lm(cos(theta) ~ t)
  
  # Slopes
  b_s <- coef(fit_s)[["t"]]
  b_c <- coef(fit_c)[["t"]]
  
  # Mean direction
  s_bar <- mean(sin(theta))
  c_bar <- mean(cos(theta))
  
  # Angular slope (radians per year)
  beta <- (c_bar * b_s - s_bar * b_c) / (c_bar^2 + s_bar^2)
  
  # Convert to months per year
  slope_months <- beta * 12 / (2 * pi)
  
  slope_months
}

######### Calculate mean birth month slopes ##########
br_countries_with_20_years <- br_data %>%
  group_by(Country.or.Area, Year) %>%
  summarise(.groups = "drop") %>%
  count(Country.or.Area) %>%
  filter(n >= 20) %>%
  pull(Country.or.Area)

br_data_20_years <- br_data[br_data$Country.or.Area %in% br_countries_with_20_years, ]

br_slope_traits <- compute_seasonality_stats_slope(br_data_20_years, latitude, "birth") %>%
  rename(Country = Country.or.Area, birth_month_slope = month_slope, birth_amplitude_slope = amplitude_slope)

######################################################


######## Calculate slope for TMAX ##########
cpk_tmax_slope_br <- cpk_tmax_br_filtered %>%
  group_by(Country) %>%
  filter(n() >= 15) %>%
  do({ fit <- lm(TMAX ~ Year, .); tibble(conception_slope = coef(fit)[["Year"]]) }) %>%
  ungroup()

########### Plot mean birth month slope vs TMAX slope figure ############
df_plot_slope <- br_slope_traits %>%
  inner_join(cpk_tmax_slope_br %>% dplyr::select(Country, conception_slope), by = "Country") %>%
  inner_join(latitude %>% dplyr::select(Country, Latitude), by = "Country") %>%
  dplyr::filter(!is.na(birth_month_slope), !is.na(conception_slope), !is.na(Latitude))

df_plot_slope <- df_plot_slope %>%
  mutate(
    abs_lat = abs(Latitude),
    lat_group = case_when(
      abs_lat > 43 ~ "High",
      abs_lat > 20 ~ "Mid",
      TRUE         ~ "Low"
    ),
    hemisphere = ifelse(Latitude >= 0, "Northern", "Southern")
  )

# ---- Function ----
make_slope_plot <- function(df) {
  # Spearman correlation
  sp <- cor.test(df$conception_slope, df$birth_month_slope, method = "spearman")
  p_text <- ifelse(sp$p.value < .Machine$double.xmin,
                   "p < 2.2e-308",
                   paste0("p = ", signif(sp$p.value, 2)))
  corr_label <- paste0(
    "Spearman R = ", round(sp$estimate, 2), "\n", p_text
  )
  
  # Plot
  p <- ggplot(df, aes(x = conception_slope, y = birth_month_slope)) +
    geom_point(aes(fill = lat_group, shape = hemisphere),
               size = 1.5, alpha = 0.7,
               color = "white", stroke = 0.3) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = "black", fill = "gray70",
                linewidth = 0.3, alpha = 0.7) +
    scale_fill_manual(
      values = c("High" = "deepskyblue2",
                 "Mid"  = "yellowgreen",
                 "Low"  = "darkorange2")
    ) +
    scale_shape_manual(
      values = c("Northern" = 21, "Southern" = 24)
    ) +
    scale_x_continuous(labels = function(x) sprintf("%.2f", x)) +
    scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
    labs(
      x = expression(Delta * " Temperature (" * degree * "C/year)"),
      y = expression(Delta * " Birth month (month/year)")
    ) +
    annotate("text",
             x = max(df$conception_slope, na.rm = TRUE),
             y = max(df$birth_month_slope, na.rm = TRUE),
             hjust = 1, vjust = 1,
             label = corr_label,
             size = 2,
             fontface = "italic") +
    theme_minimal(base_size = 5) +
    theme(
      text = element_text(size = 5),
      axis.title = element_text(size = 6),
      axis.text = element_text(size = 5),
      plot.title = element_text(size = 5),
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 5),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line        = element_line(linewidth = 0.3, color = "black"),
      axis.ticks       = element_line(linewidth = 0.3, color = "black"),
      axis.ticks.length = unit(2, "pt")
    )
  
  return(p)
}

# ---- Usage ----
p_slope <- make_slope_plot(df_plot_slope)

# ---- Save ----
ggsave("Birth_figures/conception_vs_birth_month_slope_with_stats.pdf",
       plot = p_slope, width = 1.5, height = 1.5)

################################################################

#################### Temperature change over time ###################

###### Singapore
# ---- Packages ----
library(dplyr)
library(ggplot2)
library(scales)

# =========================
# Input: birth_by_year (already has mean birth month)
# =========================
# Example structure:
# Country | Year | birth_mean_month

# ---- 1) Filter for Singapore ----
mean_month_by_year <- birth_by_year %>%
  filter(Country == "Singapore") %>%
  transmute(
    year = Year,
    mean_birth_month = birth_mean_month,
    conception_month = (birth_mean_month + 3) %% 12
  )

# ---- 2) Monthly climatology (TMAX) per year ----
tmax_monthly_yearly <- cpk_monthly_tmax_br_filtered %>%
  filter(Country == "Singapore") %>%
  group_by(Year, Month) %>%
  summarise(mean_TMAX = mean(TMAX, na.rm = TRUE), .groups = "drop")

# ---- 3) Apply month shift (April→March year) ----
# Shift all months by -3 (so April=1, May=2, ..., Mar=12)
tmax_monthly_yearly <- tmax_monthly_yearly %>%
  mutate(
    Month_shifted = ((Month - 4) %% 12) + 1  # ensures 1–12 range
  )

# ---- 4) Rank months by temperature per year (in shifted frame) ----
tmax_ranked <- tmax_monthly_yearly %>%
  group_by(Year) %>%
  mutate(temp_rank = rank(mean_TMAX, ties.method = "first")) %>%  # 1 = coldest, 12 = hottest
  ungroup() %>%
  mutate(
    xmin = Month_shifted - 1,
    xmax = Month_shifted,
    ymin = Year - 0.5,
    ymax = Year + 0.5
  )

# ---- 5) Overlapping years ----
valid_years <- intersect(unique(mean_month_by_year$year), unique(tmax_ranked$Year))

# ---- 6) Subset both datasets ----
mean_month_by_year_sub <- mean_month_by_year %>%
  filter(year %in% valid_years, year >= 1970, year <= 2025) %>%
  mutate(
    mean_birth_month_shift = ((mean_birth_month - 4) %% 12),
    conception_month_shift = ((conception_month - 4) %% 12)
  )

tmax_ranked_sub <- tmax_ranked %>%
  filter(Year %in% valid_years, Year >= 1970, Year <= 2025)

# ---- 7) Smooth curves (using shifted months) ----
smooth_birth <- {
  fit_b <- loess(mean_birth_month_shift ~ year, data = mean_month_by_year_sub, span = 0.5, degree = 2)
  yr_seq <- seq(min(mean_month_by_year_sub$year), max(mean_month_by_year_sub$year), by = 1)
  data.frame(
    year = yr_seq,
    mean_birth_month_shift = (predict(fit_b, newdata = data.frame(year = yr_seq))) %% 12
  ) %>% drop_na()
}

smooth_concep <- {
  fit_c <- loess(conception_month_shift ~ year, data = mean_month_by_year_sub, span = 0.5, degree = 2)
  yr_seq <- seq(min(mean_month_by_year_sub$year), max(mean_month_by_year_sub$year), by = 1)
  data.frame(
    year = yr_seq,
    conception_month_shift = (predict(fit_c, newdata = data.frame(year = yr_seq))) %% 12
  ) %>% drop_na()
}

# ---- 8) Plot ----
p <- ggplot() +
  # Temperature rank shading per year (shifted months)
  geom_rect(
    data = tmax_ranked_sub,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = temp_rank),
    inherit.aes = FALSE,
    alpha = 0.4
  ) +
  # Birth mean
  geom_point(
    data = mean_month_by_year_sub,
    aes(x = mean_birth_month_shift, y = year),
    color = "white", fill = "gray30",
    shape = 21, size = 0.4, stroke = 0.1, alpha = 0.6
  ) +
  geom_path(
    data = smooth_birth,
    aes(x = mean_birth_month_shift, y = year),
    color = "gray30", linewidth = 0.2
  ) +
  # Conception mean
  geom_point(
    data = mean_month_by_year_sub,
    aes(x = conception_month_shift, y = year),
    color = "white", fill = "forestgreen",
    shape = 21, size = 0.4, stroke = 0.1, alpha = 0.6
  ) +
  geom_path(
    data = smooth_concep,
    aes(x = conception_month_shift, y = year),
    color = "forestgreen", linewidth = 0.2
  ) +
  # X axis: mid-month ticks (now Apr→Mar year)
  scale_x_continuous(
    limits = c(0, 12),
    breaks = c(1.5, 4.5, 7.5, 10.5),
    #labels = c("Jul", "Oct", "Jan", "Apr"),  # corresponding to midpoints in shifted frame
    labels = NULL,
    expand = expansion(mult = c(0, 0))
  ) +
  # Y axis: reversed years
  scale_y_reverse(
    limits = c(2025, 1970),
    breaks = seq(1970, 2020, by = 10),
    labels = seq(1970, 2020, by = 10)
  ) +
  scale_fill_gradientn(
    colours = c("#2166ac", "#fddbc7", "#b2182b"),
    name = "Temp Rank",
    limits = c(1, 12),
    breaks = c(1, 6, 12),
    guide = "none"
  ) +
  coord_cartesian(
    ylim = c(2025, 1965),
    xlim = c(0, 12),
    expand = FALSE
  ) +
  theme_minimal(base_size = 5) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.2, color = "black"),
    axis.ticks.length = unit(2, "pt"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line        = element_line(linewidth = 0.2, color = "black"),
    legend.position  = "none"
  )

# ---- 9) Save ----
ggsave(
  "Birth_figures/singapore_birth_conception_TMAXrank_AprToMar.pdf",
  p, width = 0.75, height = 1, device = "pdf"
)


#### Denmark
# ---- Packages ----
library(dplyr)
library(ggplot2)
library(scales)

# =========================
# Input: birth_by_year (already has mean birth month)
# =========================
# Example structure:
# Country | Year | birth_mean_month

# ---- 1) Filter for Denmark ----
mean_month_by_year <- birth_by_year %>%
  filter(Country == "Denmark") %>%
  transmute(
    year = Year,
    mean_birth_month = birth_mean_month,
    conception_month = (birth_mean_month + 3) %% 12
  )

# ---- 2) Monthly climatology (TMAX) per year ----
tmax_monthly_yearly <- cpk_monthly_tmax_br_filtered %>%
  filter(Country == "Denmark") %>%
  group_by(Year, Month) %>%
  summarise(mean_TMAX = mean(TMAX, na.rm = TRUE), .groups = "drop")

# ---- 3) Rank months by temperature per year ----
tmax_ranked <- tmax_monthly_yearly %>%
  group_by(Year) %>%
  mutate(temp_rank = rank(mean_TMAX, ties.method = "first")) %>%  # 1 = coldest, 12 = hottest
  ungroup() %>%
  mutate(
    xmin = Month - 1,
    xmax = Month,
    ymin = Year - 0.5,
    ymax = Year + 0.5
  )

# ---- 4) Overlapping years ----
valid_years <- intersect(unique(mean_month_by_year$year), unique(tmax_ranked$Year))

# ---- 5) Subset both datasets ----
mean_month_by_year_sub <- mean_month_by_year %>%
  filter(year %in% valid_years, year >= 1970, year <= 2025)

tmax_ranked_sub <- tmax_ranked %>%
  filter(Year %in% valid_years, Year >= 1970, Year <= 2025)

# ---- 6) Smooth curves ----
smooth_birth <- {
  fit_b <- loess(mean_birth_month ~ year, data = mean_month_by_year_sub, span = 0.5, degree = 2)
  yr_seq <- seq(min(mean_month_by_year_sub$year), max(mean_month_by_year_sub$year), by = 1)
  data.frame(
    year = yr_seq,
    mean_birth_month = (predict(fit_b, newdata = data.frame(year = yr_seq))) %% 12
  ) %>% drop_na()
}

smooth_concep <- {
  fit_c <- loess(conception_month ~ year, data = mean_month_by_year_sub, span = 0.5, degree = 2)
  yr_seq <- seq(min(mean_month_by_year_sub$year), max(mean_month_by_year_sub$year), by = 1)
  data.frame(
    year = yr_seq,
    conception_month = (predict(fit_c, newdata = data.frame(year = yr_seq))) %% 12
  ) %>% drop_na()
}

# ---- 7) Plot ----
p <- ggplot() +
  # Temperature rank shading per year
  geom_rect(
    data = tmax_ranked_sub,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = temp_rank),
    inherit.aes = FALSE,
    alpha = 0.4
  ) +
  # Birth mean
  geom_point(
    data = mean_month_by_year_sub,
    aes(x = mean_birth_month, y = year),
    color = "white", fill = "gray30",
    shape = 21, size = 0.4, stroke = 0.1, alpha = 0.6
  ) +
  geom_path(
    data = smooth_birth,
    aes(x = mean_birth_month, y = year),
    color = "gray30", linewidth = 0.2
  ) +
  # Conception mean
  geom_point(
    data = mean_month_by_year_sub,
    aes(x = conception_month, y = year),
    color = "white", fill = "forestgreen",
    shape = 21, size = 0.4, stroke = 0.1, alpha = 0.6
  ) +
  geom_path(
    data = smooth_concep,
    aes(x = conception_month, y = year),
    color = "forestgreen", linewidth = 0.2
  ) +
  # X axis (mid-month ticks)
  scale_x_continuous(
    limits = c(0, 12),
    breaks = c(2.5, 5.5, 8.5, 11.5),
    labels = NULL,
    expand = expansion(mult = c(0, 0))
  ) +
  # Y axis (reversed years)
  scale_y_reverse(
    limits = c(2025, 1970),
    breaks = seq(1970, 2020, by = 10),
    labels = seq(1970, 2020, by = 10)
  ) +
  scale_fill_gradientn(
    colours = c("#2166ac", "#fddbc7", "#b2182b"),
    name = "Temp Rank",
    limits = c(1, 12),
    breaks = c(1, 6, 12),
    guide = "none"
  ) +
  coord_cartesian(
    ylim = c(2025, 1965),
    xlim = c(0, 12),
    expand = FALSE
  ) +
  theme_minimal(base_size = 5) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_line(linewidth = 0.2, color = "black"),
    axis.ticks.length = unit(2, "pt"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line        = element_line(linewidth = 0.2, color = "black"),
    legend.position  = "none"
  )

# ---- 8) Save ----
ggsave(
  "Birth_figures/denmark_birth_conception_TMAXrank_per_year.pdf",
  p, width = 0.75, height = 1, device = "pdf"
)


################################################################

###### Tmax trend ######
countries <- c("Denmark", "Singapore")

# ---- 1) Compute mean annual Tmax ----
tmax_yearly_summary <- cpk_monthly_tmax_br_filtered %>%
  filter(Country %in% countries) %>%
  group_by(Country, Year) %>%
  summarise(Tmax = mean(TMAX, na.rm = TRUE), .groups = "drop")

# ---- 2) Loop over countries ----
for (ctry in countries) {
  dat <- tmax_yearly_summary %>% filter(Country == ctry)
  
  # LOESS smooth
  fit <- loess(Tmax ~ Year, data = dat, span = 0.75)
  pred <- predict(fit, se = TRUE)
  
  fit_df <- tibble(
    Year = dat$Year,
    fit  = as.numeric(pred$fit),
    se   = as.numeric(pred$se.fit)
  ) %>% arrange(Year)
  
  # ---- 3) Country-specific axis limits ----
  if (ctry == "Singapore") {
    x_limits <- c(29, 35)
    x_breaks <- seq(30, 34, 2)
  } else if (ctry == "Denmark") {
    x_limits <- c(9, 15)
    x_breaks <- seq(10, 14, 2)
  } else if (ctry == "Switzerland") {
    x_limits <- c(6, 14)
    x_breaks <- seq(6, 14, 2)
  } else {
    x_limits <- range(dat$Tmax, na.rm = TRUE)
    x_breaks <- pretty(x_limits, n = 5)
  }
  
  # ---- 4) Plot ----
  p <- ggplot(dat, aes(x = Tmax, y = Year)) +
    geom_point(size = 0.5, alpha = 0.5, shape = 19, color = "goldenrod", stroke = 0) +
    geom_ribbon(
      data = fit_df,
      aes(y = Year, xmin = fit - se, xmax = fit + se),
      inherit.aes = FALSE, fill = "goldenrod", alpha = 0.3, color = NA
    ) +
    geom_path(
      data = fit_df,
      aes(y = Year, x = fit),
      color = "darkorange3", linewidth = 0.2, lineend = "round"
    ) +
    scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks,
      expand = expansion(mult = c(0.01, 0.01)),
      labels = NULL
    ) +
    scale_y_reverse(
      limits = c(2025, 1970),
      breaks = seq(1970, 2025, by = 10),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    coord_cartesian(
      ylim = c(2025, 1965),
      xlim = x_limits,
      expand = FALSE
    ) +
    theme_minimal(base_size = 5) +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      # thinner x-axis line and ticks
      axis.line.x = element_line(linewidth = 0.2, color = "black"),
      axis.ticks.x = element_line(linewidth = 0.2, color = "black"),
      axis.ticks.length = unit(2, "pt"),
      
      # completely remove y-axis
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      
      # remove labels/titles on x if desired
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      
      plot.title = element_text(hjust = 0.5, size = 6)
    )
  
  # ---- 5) Save ----
  ggsave(
    paste0("Birth_figures/", tolower(ctry), "_TMAXmean_trend_vertical.pdf"),
    p, width = 0.5, height = 1, device = "pdf"
  )
}
############################



########### Temperature at conception slope vs. Temperature slope ###########

# ============================
# Packages
# ============================
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# ============================
# Birth-by-year computation
# ============================
compute_birth_by_year <- function(df, value_col = "Normalized.Value") {
  df <- df %>% mutate(Year = as.integer(Year))
  
  # Month number (1–12)
  if ("Month_num" %in% names(df)) {
    df <- df %>% mutate(.m = as.integer(Month_num))
  } else {
    month_levels <- c("January","February","March","April","May","June",
                      "July","August","September","October","November","December")
    if (is.numeric(df$Month)) {
      df <- df %>% mutate(.m = as.integer(Month))
    } else {
      df <- df %>% mutate(.m = match(as.character(Month), month_levels))
    }
  }
  
  # Keep only complete years
  complete_years <- df %>%
    group_by(Country.or.Area, Year) %>%
    filter(n() == 12) %>%
    ungroup()
  
  # Circular mean month in [0,12)
  birth_by_year <- complete_years %>%
    group_by(Country.or.Area, Year) %>%
    summarise(
      birth_mean_month = {
        ang <- 2 * pi * (.m - 1) / 12
        w   <- .data[[value_col]]
        (atan2(sum(sin(ang) * w), sum(cos(ang) * w)) * 12 / (2 * pi)) %% 12
      },
      .groups = "drop"
    ) %>%
    rename(Country = Country.or.Area)
  
  birth_by_year
}

# ---- Example ----
birth_by_year <- compute_birth_by_year(br_data, value_col = "Normalized.Value")


interp_month_temp <- function(df_tx, year, frac_month) {
  
  # real month in [0,12), centers are 0.5, 1.5, ..., 11.5
  m <- frac_month %% 12
  
  # Find nearest lower and upper centers
  lower_center <- floor(m) + 0.5
  upper_center <- lower_center + 1
  
  lower_month <- floor(lower_center) + 1
  upper_month <- floor(upper_center) + 1
  
  tx_low <- df_tx %>% filter(Year == year, Month == lower_month) %>% pull(TX)
  tx_high <- df_tx %>% filter(Year == year, Month == upper_month) %>% pull(TX)
  
  if (length(tx_low) == 0 || length(tx_high) == 0) return(NA_real_)
  
  d_low  <- abs(m - lower_center)
  d_high <- abs(m - upper_center)
  
  total <- d_low + d_high
  if (total == 0) return(tx_low)
  
  w_low  <- d_high / total
  w_high <- d_low  / total
  
  w_low * tx_low + w_high * tx_high
}



# ============================
# 1) Shared countries
# ============================
shared_countries <- intersect(unique(birth_by_year$Country),
                              unique(cpk_monthly_tmax_br_filtered$Country))

slopes_df <- birth_by_year %>%
  filter(Country %in% shared_countries) %>%
  group_by(Country) %>%
  group_modify(~ {
    
    # ----------------------------
    # Birth → conception month
    # ----------------------------
    df_birth <- .x %>%
      transmute(
        year = Year,
        conception_month = (birth_mean_month + 3) %% 12
      )
    
    # ----------------------------
    # Monthly TMAX data (year-specific!)
    # ----------------------------
    df_tx <- cpk_monthly_tmax_br_filtered %>%
      filter(Country == .y$Country) %>%
      transmute(
        Year,
        Month,
        TX = TMAX
      )
    
    # Require complete years for interpolation
    valid_years <- df_tx %>%
      group_by(Year) %>%
      filter(n() == 12) %>%
      pull(Year) %>%
      unique()
    
    df_birth <- df_birth %>% filter(year %in% valid_years)
    
    if (nrow(df_birth) < 5) {
      return(tibble(slope = NA_real_))
    }
    
    # ----------------------------
    # Interpolate TMAX at conception month
    # ----------------------------
    df_birth <- df_birth %>%
      rowwise() %>%
      mutate(
        mean_TMAX = interp_month_temp(
          df_tx = df_tx,
          year = year,
          frac_month = conception_month
        )
      ) %>%
      ungroup()
    
    if (all(is.na(df_birth$mean_TMAX))) {
      return(tibble(slope = NA_real_))
    }
    
    # ----------------------------
    # Fit slope
    # ----------------------------
    fit <- lm(mean_TMAX ~ year, data = df_birth)
    tibble(slope = coef(fit)[["year"]])
    
  }) %>%
  ungroup() %>%
  filter(!is.na(slope))

# ============================
# 3) Combine with birth slopes
# ============================
combined_slopes <- slopes_df %>%
  inner_join(br_slope_traits %>% dplyr::select(Country, birth_month_slope),
             by = "Country")

# ============================
# 4) Merge latitude and categorize
# ============================
combined_slopes_lat <- combined_slopes %>%
  inner_join(latitude %>% dplyr::select(Country, Latitude), by = "Country") %>%
  mutate(
    abs_lat = abs(Latitude),
    lat_group = case_when(
      abs_lat > 40 ~ "High",
      abs_lat > 20 ~ "Mid",
      TRUE         ~ "Low"
    ),
    hemisphere = ifelse(Latitude >= 0, "Northern", "Southern")
  )


# ============================
# 5) Merge with conception slopes for plotting
# ============================
plot_df <- combined_slopes_lat %>%
  inner_join(df_plot_slope %>% dplyr::select(Country, conception_slope),
             by = "Country")



# ---- Compute Spearman correlation ----
sp <- cor.test(plot_df$conception_slope, plot_df$slope, method = "spearman")

p_text <- ifelse(sp$p.value < .Machine$double.xmin,
                 "p < 2.2e-308",
                 paste0("p = ", signif(sp$p.value, 2)))

corr_label <- paste0(
  "Spearman R = ", round(sp$estimate, 2), "\n", p_text
)

# ---- Plot (style-matched) ----
p <- ggplot(plot_df, aes(x = conception_slope, y = slope)) +
  geom_point(
    aes(fill = lat_group, shape = hemisphere),
    size = 1.5,
    alpha = 0.7,
    color = "white",
    stroke = 0.3
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "black",
    fill = "gray70",
    linewidth = 0.3,
    alpha = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "High" = "deepskyblue2",
      "Mid"  = "yellowgreen",
      "Low"  = "darkorange2"
    )
  ) +
  scale_shape_manual(
    values = c("Northern" = 21, "Southern" = 24)
  ) +
  scale_x_continuous(labels = function(x) sprintf("%.2f", x)) +
  scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
  labs(
    x = expression(Delta * " Temperature (" * degree * "C/year)"),
    y = expression("Temperature at conception (" * degree * "C/year)")
  ) +
  annotate(
    "text",
    x = max(plot_df$conception_slope, na.rm = TRUE),
    y = max(plot_df$slope, na.rm = TRUE),
    hjust = 1,
    vjust = 1,
    label = corr_label,
    size = 2,
    fontface = "italic"
  ) +
  theme_minimal(base_size = 5) +
  theme(
    text = element_text(size = 5),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 5),
    plot.title = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line        = element_line(linewidth = 0.3, color = "black"),
    axis.ticks       = element_line(linewidth = 0.3, color = "black"),
    axis.ticks.length = unit(2, "pt")
  )

ggsave(
  "Birth_figures/slope_vs_conception.pdf",
  plot = p,
  width = 1.5,
  height = 1.5
)
########################################################





#################### Holiday Analyses ####################
br_countries_to_keep <- unique(periodogram$Country[periodogram$Birth_pvals_12m < 0.05])
br_countries_to_keep <- br_countries_to_keep[!is.na(br_countries_to_keep)]

br_data_holiday <- all_br_data[all_br_data$Country.or.Area %in% br_countries_to_keep, ]

#br_data <- br_data %>%
#  filter(Year >= 1970, Year <= 2019)


br_traits_holiday <- compute_seasonality_stats(br_data_holiday) %>%
  rename(Country = Country.or.Area, birth_mean_month = mean_month, birth_amplitude = amplitude)

# ============================
# Packages
# ============================
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# ============================
# Step 1 – Month dictionary
# ============================
# All map to month index 1–12
month_dict <- c(
  "january" = 1, "jan" = 1,
  "february" = 2, "feb" = 2,
  "march" = 3, "mar" = 3,
  "april" = 4, "apr" = 4,
  "may" = 5,
  "june" = 6, "jun" = 6,
  "july" = 7, "jul" = 7,
  "august" = 8, "aug" = 8,
  "september" = 9, "sep" = 9,
  "october" = 10, "oct" = 10,
  "november" = 11, "nov" = 11,
  "december" = 12, "dec" = 12
)

# ============================
# Step 2 – Extract month helper
# ============================
extract_month_numeric <- function(text) {
  if (is.na(text)) return(NA_integer_)
  
  t <- tolower(text)
  
  # find any month substring
  found <- names(month_dict)[str_detect(t, names(month_dict))]
  
  if (length(found) == 0) return(NA_integer_)
  
  return(month_dict[found[1]])
}

# ============================
# Step 3 – Read holiday CSV
# ============================
holiday_df <- read_csv("climate_holiday_data_manual - holidays.csv")

holiday_df <- holiday_df %>%
  mutate(
    holiday_month_index = sapply(`Date/Month of Biggest Holiday`, extract_month_numeric),
    # convert: 1 → 0.5, 2 → 1.5, … 12 → 11.5
    holiday_month_center = holiday_month_index - 0.5
  )

# ============================
# Step 4 – Merge with br_traits
# ============================
merged <- br_traits_holiday %>%
  left_join(
    holiday_df %>% select(Country, holiday_month_center),
    by = "Country"
  )

# ============================
# Step 5 – Add latitude + groups
# ============================
categorize_lat <- function(df) {
  df %>%
    mutate(
      abs_lat = abs(Latitude),
      lat_group = case_when(
        abs_lat > 40 ~ "High",
        abs_lat > 20 ~ "Mid",
        TRUE         ~ "Low"
      ),
      hemisphere = ifelse(Latitude >= 0, "Northern", "Southern")
    )
}

df_holiday <- merged %>%
  filter(!is.na(holiday_month_center)) %>%
  inner_join(latitude_with_daylength %>% select(Country, Latitude), by = "Country") %>%
  categorize_lat()


pdf("Birth_figures/birth_vs_holiday_month_0.05-sig.pdf", width = 3, height = 3)

# Month centers (same positions)
month_breaks <- seq(0.5, 11.5, by = 1)

# Short month labels
month_labels_short <- c(
  "Jan","Feb","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec"
)

# ============================
# Compute Spearman correlation
# ============================
sp <- cor.test(df_holiday$holiday_month_center, df_holiday$birth_mean_month, method = "spearman")

# --- Format R value (2 decimals)
r_val <- formatC(sp$estimate, format = "f", digits = 2)

# --- Format p-value exactly like your function
if (sp$p.value == 0) {
  p_text <- paste0("p < ", formatC(.Machine$double.xmin, format = "e", digits = 2))
} else if (sp$p.value >= 1e-4) {
  p_text <- paste0("p = ", signif(sp$p.value, 2))
} else {
  p_text <- paste0("p = ", formatC(sp$p.value, format = "e", digits = 2))
}

corr_label <- paste0("Spearman R = ", r_val, "\n", p_text)

# ============================
# THE PLOT
# ============================
ggplot(df_holiday, aes(x = holiday_month_center, y = birth_mean_month)) +
  geom_point(
    aes(fill = lat_group, shape = hemisphere),
    size = 1.5, alpha = 0.5,
    color = "white", stroke = 0.3
  ) +
  geom_smooth(
    method = "lm", formula = y ~ x, se = TRUE,
    color = "black", fill = "gray70",
    linewidth = 0.4, alpha = 0.25
  ) +
  scale_fill_manual(
    values = c(
      "High" = "deepskyblue2",
      "Mid"  = "yellowgreen",
      "Low"  = "darkorange2"
    )
  ) +
  scale_shape_manual(
    values = c("Northern" = 21, "Southern" = 24)
  ) +
  scale_x_continuous(
    limits = c(0, 12),
    breaks = month_breaks,
    labels = month_labels_short
  ) +
  scale_y_continuous(
    limits = c(0, 12),
    breaks = month_breaks,
    labels = month_labels_short
  ) +
  labs(
    x = "Holiday Month",
    y = "Mean Birth Month"
  ) +
  annotate(
    "text",
    x = max(df_holiday$holiday_month_center, na.rm = TRUE),
    y = max(df_holiday$birth_mean_month, na.rm = TRUE),
    label = corr_label,
    hjust = 1, vjust = 1,
    size = 5 / .pt,
    fontface = "italic"
  ) +
  theme_minimal(base_size = 5) +
  theme(
    text = element_text(size = 5),
    axis.title = element_text(size = 5),
    axis.text = element_text(size = 5),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # Thinner axes + ticks
    axis.line        = element_line(linewidth = 0.3, color = "black"),
    axis.ticks       = element_line(linewidth = 0.3, color = "black"),
    axis.ticks.length = unit(1.5, "pt")
  )

dev.off()
########################################################



