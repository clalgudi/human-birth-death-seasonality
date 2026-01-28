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

all_br_data <- read.csv("all_countries_br_data.csv", header = TRUE)
all_mr_data <- read.csv("all_countries_mr_data.csv", header = TRUE)
periodogram <- read.csv("country_metadata_plomb_SL.csv", header = TRUE)


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


###################### Birth per year ########################
br_data <- read.csv("all_countries_br_data.csv", header = TRUE)
mr_data <- read.csv("all_countries_mr_data.csv", header = TRUE)


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
###############################################


############### Polar plot birth month and amplitude #################
# ===========================================
# Clean polar plot: Birth mean month vs amplitude (rotated, same style)
# ===========================================

library(ggplot2)
library(dplyr)
library(scales)

# ---- 1) Merge latitude info ----
df <- all_br_traits %>%
  inner_join(
    periodogram %>%
      dplyr::select(Country, Latitude),
    by = "Country"
  ) %>%
  mutate(abs_lat = abs(Latitude))

# ---- 2) Define dark turbo-like rainbow color scale ----
turbo_colors <- c(
  "#3B00A3", "#1562AC", "#1E9DC5", "#4EC1A6",
  "#A6D96A", "#FEE08B", "#FDAE61", "#F46D43",
  "#D73027", "#A50026"
)

# ---- 3) Plot ----
p <- ggplot(df, aes(x = birth_mean_month, y = birth_amplitude, color = abs_lat)) +
  geom_point(size = 0.6, alpha = 0.6, shape=16) +
  scale_color_gradientn(
    colors = turbo_colors,
    limits = c(5, 65),
    oob = squish,
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(0, 12),
    breaks = seq(0.5, 11.5, by = 1),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    expand = c(0, 0)
  ) +
  coord_polar(start = -pi/2) +  # ensures Jan is at top
  theme_minimal(base_size = 7) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 6.5, color = "black"),
    axis.text.y = element_blank(),  # remove 5,10,15,20,25 labels
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray70", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ---- 4) Save ----
ggsave(
  "Birth_figures/Figure_polar_birth_vs_amplitude.pdf",
  p, width = 1.5, height = 1.5, device = "pdf"
)

###############################################


############### Polar plot death month and amplitude #################

library(ggplot2)
library(dplyr)
library(scales)

# ---- 1) Merge latitude info ----
df <- all_mr_traits %>%
  inner_join(
    periodogram %>%
      dplyr::select(Country, Latitude),
    by = "Country"
  ) %>%
  mutate(abs_lat = abs(Latitude))

# ---- 2) Define dark turbo-like rainbow color scale ----
turbo_colors <- c(
  "#3B00A3", "#1562AC", "#1E9DC5", "#4EC1A6",
  "#A6D96A", "#FEE08B", "#FDAE61", "#F46D43",
  "#D73027", "#A50026"
)

# ---- 3) Plot ----
p <- ggplot(df, aes(x = death_mean_month, y = birth_amplitude, color = abs_lat)) +
  geom_point(size = 0.6, alpha = 0.6, shape=16) +
  scale_color_gradientn(
    colors = turbo_colors,
    limits = c(5, 65),
    oob = squish,
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(0, 12),
    breaks = seq(0.5, 11.5, by = 1),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    expand = c(0, 0)
  ) +
  coord_polar(start = -pi/2) +  # ensures Jan is at top
  theme_minimal(base_size = 7) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 6.5, color = "black"),
    axis.text.y = element_blank(),  # remove 5,10,15,20,25 labels
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray70", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ---- 4) Save ----
ggsave(
  "Birth_figures/Figure_polar_death_vs_amplitude.pdf",
  p, width = 1.5, height = 1.5, device = "pdf"
)
#######################################################


############### Polar plot birth month and amplitude #################
# ===========================================
# Birth month trends for multiple countries
# ===========================================

library(dplyr)
library(ggplot2)

# ---- 1) Country list ----
countries <- c(
  "Norway", "Denmark", "Netherlands (Kingdom of the)",
  "France", "Switzerland", "Canada", "United States of America",
  "Japan", "Israel", "Cuba", "Puerto Rico", "Singapore", "Chile"
)

# ---- 2) Output directory ----
dir.create("Figure3_panels", showWarnings = FALSE)

# ---- 3) Function to make one plot ----
make_country_plot <- function(country_name) {
  message("Processing ", country_name, "...")
  
  df <- birth_by_year %>%
    filter(Country == country_name) %>%
    arrange(Year)
  
  if (nrow(df) < 5) {
    message("  Skipping ", country_name, " (too few data points)")
    return(NULL)
  }
  
  # Smooth LOESS curve (fit across available years only)
  fit <- loess(birth_mean_month ~ Year, data = df, span = 0.5, degree = 1)
  
  yr_seq <- seq(min(df$Year), max(df$Year), by = 1)
  smooth_df <- data.frame(
    Year = yr_seq,
    mean_birth_month = predict(fit, newdata = data.frame(Year = yr_seq))
  )
  
  # ---- Plot ----
  p <- ggplot() +
    geom_point(
      data = df,
      aes(x = birth_mean_month, y = Year),
      color = "#ff9999", fill = "#ff9999",
      shape = 21, size = 0.5, stroke = 0, alpha = 0.8
    ) +
    geom_path(
      data = smooth_df,
      aes(x = mean_birth_month, y = Year),
      color = "#cc0000", linewidth = 0.3
    ) +
    scale_x_continuous(
      limits = c(0, 12),
      breaks = c(2.5, 5.5, 8.5, 11.5),
      labels = c("Mar", "Jun", "Sep", "Dec"),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_reverse(
      limits = c(2023, 1967),
      breaks = seq(1970, 2020, by = 10),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    theme_minimal(base_size = 6) +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(size = 5, color = "black", margin = margin(t = 2)),
      axis.text.y = element_text(size = 5, color = "black"),
      axis.ticks = element_line(linewidth = 0.2, color = "black"),
      axis.ticks.length = unit(1.5, "pt"),
      # dashed grid lines
      panel.grid.major = element_line(color = "gray70", linewidth = 0.2, linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line        = element_line(linewidth = 0.2, color = "black"),
      legend.position  = "none"
    )
  
  # ---- Save ----
  filename <- paste0("Birth_figures/", gsub("[^A-Za-z]", "_", country_name), "_birth_trend.pdf")
  ggsave(filename, p, width = 0.8, height = 0.75, device = "pdf")
}

# ---- 4) Run for all countries ----
lapply(countries, make_country_plot)
#######################################################


############### Smoothed temporal birth month plot #################

library(dplyr)
library(ggplot2)
library(purrr)
library(scales)

br_countries_to_keep <- unique(periodogram$Country[periodogram$Birth_pvals_12m < 1e-10])
br_countries_to_keep <- br_countries_to_keep[!is.na(br_countries_to_keep)]

br_data_korea <- all_br_data[all_br_data$Country.or.Area %in% br_countries_to_keep, ]

birth_by_year <- compute_birth_by_year(br_data_korea, value_col = "Normalized.Value")

# ---- 0) Join metadata ----
dat <- birth_by_year %>%
  inner_join(
    periodogram %>%
      dplyr::select(Country, Abbrev, Latitude, N_years_birth, Birth_pvals_12m),
    by = "Country"
  )

# ---- 1) Filter (keep all hemispheres, use absolute latitude) ----
dat_filt <- dat %>%
  filter(
    !is.na(birth_mean_month),
    !is.na(Year),
    !is.na(Latitude),
    !is.na(N_years_birth), N_years_birth >= 20,
    !is.na(Birth_pvals_12m), Birth_pvals_12m < 0.05
  ) %>%
  mutate(abs_lat = abs(Latitude))

# ---- 2) Countries that need phase-unwrapping ----
unwrap_countries <- c(
  "Egypt", "Guadeloupe", "New Caledonia", "Australia", "Bahrain",
  "Martinique", "Panama", "Seychelles", "United States Virgin Islands",
  "El Salvador", "American Samoa", "Guatemala", "Republic of Korea"
)

# ---- 3) Apply transformation (shift by +12 if <4 for selected countries) ----
birth_lat <- dat_filt %>%
  mutate(
    abs_lat = abs(Latitude),
    birth_month_adj = ifelse(
      Country %in% unwrap_countries & birth_mean_month < 4,
      birth_mean_month + 12,
      birth_mean_month
    )
  )

# ---- 4) Function to smooth one country's trend ----
get_smooth_line <- function(df) {
  df <- df %>% arrange(Year)
  if (nrow(df) < 5) return(tibble())
  
  fit <- loess(birth_month_adj ~ Year, data = df, span = 0.5, degree = 1)
  yr_seq <- seq(min(df$Year), max(df$Year), by = 1)
  
  tibble(
    Country = unique(df$Country),
    abs_lat = unique(df$abs_lat),
    Year = yr_seq,
    mean_birth_month = predict(fit, newdata = tibble(Year = yr_seq))
  )
}

# ---- 5) Apply smoothing for all countries ----
smooth_all <- birth_lat %>%
  split(.$Country) %>%
  map_dfr(get_smooth_line) %>%
  drop_na(mean_birth_month)

smooth_all <- smooth_all %>%
  dplyr::mutate(
    mean_birth_month_shifted = dplyr::if_else(
      Country == "Republic of Korea",
      mean_birth_month - 12,
      mean_birth_month
    )
  )

# ============================================================
# 1) NEW COLOR PALETTE
# ============================================================

turbo_colors <- c(
  "#3B00A3", "#1562AC", "#1E9DC5", "#4EC1A6",
  "#A6D96A", "#FEE08B", "#FDAE61", "#F46D43",
  "#D73027", "#A50026"
)


# ============================================================
# 2) PLOT WITH X-AXIS STARTING AT -1 AND DECEMBER AT -0.5
# ============================================================

# You want:
#   Dec  → -0.5
#   Mar  → 2.5
#   Jun  → 5.5
#   Sep  → 8.5

p_all <- ggplot(
  smooth_all,
  aes(
    x = mean_birth_month_shifted,
    y = Year,
    group = Country
  )
) +
  geom_path(aes(color = abs_lat), linewidth = 0.3, alpha = 0.9) +
  scale_color_gradientn(
    colors = turbo_colors,
    name = NULL,
    limits = c(5, 65),
    oob = squish,
    guide = guide_colorbar(
      title = NULL,
      barheight = 1,
      barwidth = 4,
      ticks = FALSE,
      label.position = "bottom",
      label.theme = element_text(size = 6)
    )
  ) +
  scale_x_continuous(
    limits = c(-1, 13),
    breaks = c(-0.5, 2.5, 5.5, 8.5, 11.5),
    labels = c("Dec", "Mar", "Jun", "Sep", "Dec"),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_reverse(
    limits = c(2023, 1967),
    breaks = seq(1970, 2020, by = 10),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  theme_minimal(base_size = 7) +
  theme(
    axis.title       = element_blank(),
    axis.text.x      = element_text(size = 6, color = "black", margin = margin(t = 2)),
    axis.text.y      = element_text(size = 6, color = "black"),
    axis.ticks       = element_line(linewidth = 0.2, color = "black"),
    axis.ticks.length = unit(1.5, "pt"),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.2, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position  = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.line        = element_line(linewidth = 0.2, color = "black")
  )


# ============================================================
# 3) SAVE
# ============================================================

ggsave(
  "Birth_figures/global_smoothed_lines_shifted_korea.pdf",
  p_all,
  width = 2.5, height = 2.5, device = "pdf"
)
#######################################################



############### Birth & Predictor Correlation Analyses per year ###############
################## Birth season by year supplemental figure ##################

# =========================
# Packages
# =========================
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# =========================
# Inputs expected:
#   - birth_by_year: Country, Year, birth_mean_month
#   - periodogram:   Country, Abbrev, Latitude, N_years_birth, Birth_pvals_12m
# =========================

# ---- 0) Join metadata ----
dat <- birth_by_year %>%
  inner_join(
    periodogram %>%
      dplyr::select(Country, Abbrev, Latitude, N_years_birth, Birth_pvals_12m),
    by = "Country"
  )

# ---- 1) Filter (keep all hemispheres, use absolute latitude) ----
dat_filt <- dat %>%
  filter(
    !is.na(birth_mean_month),
    !is.na(Year),
    !is.na(Latitude),
    !is.na(N_years_birth), N_years_birth >= 20,
    !is.na(Birth_pvals_12m), Birth_pvals_12m < 1e-10
  ) %>%
  mutate(abs_lat = abs(Latitude))

# ---- 2) Latitude bands and month wrapping ----
hi <- dat_filt %>%
  filter(abs_lat > 43) %>%
  transmute(group = "High (|lat| > 43°)", Year,
            MonthWrapped = ifelse(birth_mean_month < 2.5,
                                  birth_mean_month + 12,
                                  birth_mean_month))

midlow <- dat_filt %>%
  filter(abs_lat <= 43) %>%
  transmute(group = "Mid-Low (|lat| ≤ 43°)", Year,
            MonthWrapped = ifelse(birth_mean_month < 2.5,
                                  birth_mean_month + 12,
                                  birth_mean_month))

pool <- bind_rows(hi, midlow)
year_min <- min(pool$Year, na.rm = TRUE)
year_max <- max(pool$Year, na.rm = TRUE)

# ---- 3) Moving-window mean ±95% CI ----
xs <- seq(max(1970, year_min), year_max) + 0.5
step <- 3
smooth_stats <- function(df) {
  purrr::map_dfr(xs, function(xc) {
    idx <- df$Year >= (xc - step) & df$Year <= (xc + step)
    yy  <- df$MonthWrapped[idx]
    n   <- sum(!is.na(yy))
    if (n >= 2) {
      m  <- mean(yy, na.rm = TRUE)
      se <- sd(yy, na.rm = TRUE) / sqrt(n)
      tcrit <- qt(0.975, df = n - 1)
      tibble(x = xc, mean = m, lo = m - tcrit * se, hi = m + tcrit * se, n = n)
    } else {
      tibble(x = xc, mean = NA_real_, lo = NA_real_, hi = NA_real_, n = n)
    }
  })
}
smooth_roll <- pool %>%
  nest(.by = group) %>%
  mutate(smooth = purrr::map(data, smooth_stats)) %>%
  dplyr::select(group, smooth) %>%
  unnest(smooth)

# ---- 4) Linear fits ----
lin_fit <- pool %>%
  nest(.by = group) %>%
  mutate(
    fit = purrr::map(data, ~ lm(MonthWrapped ~ Year, data = .x)),
    cor = purrr::map_dbl(data, ~ cor(.x$Year, .x$MonthWrapped, use = "complete.obs")),
    pred = purrr::map(fit, ~ {
      xq <- data.frame(Year = seq(year_min, year_max, length.out = 200))
      cbind(xq, as.data.frame(predict(.x, newdata = xq, interval = "confidence")))
    })
  )
lin_preds <- lin_fit %>% dplyr::select(group, pred) %>% unnest(pred)
cors     <- lin_fit %>% dplyr::select(group, cor)

# ---- 5) Points for all three panels (range 2–14 each) ----
pts_left  <- pool %>% transmute(group, Year, x = MonthWrapped)       %>% filter(x >= 2, x <= 14)
pts_mid   <- pool %>% transmute(group, Year, x = MonthWrapped + 12)  %>% filter(x >= 14, x <= 26)
pts_right <- pool %>% transmute(group, Year, x = MonthWrapped + 24)  %>% filter(x >= 26, x <= 38)

# ---- 6) Month tick marks ----
breaks_x <- rep(c(2.5, 5.5, 8.5, 11.5), 3) + rep(c(0, 12, 24), each = 4)
labels_x <- rep(c("Mar", "Jun", "Sep", "Dec"), times = 3)

# ---- 7) Plot ----
p <- ggplot() +
  # ===== Left panel =====
geom_point(data = pts_left %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year), colour = "#f15b2bff", size = 0.2, alpha = 0.1) +
  geom_point(data = pts_left %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year), colour = "#29aae1ff", size = 0.2, alpha = 0.1) +
  
  # ===== Middle panel =====
geom_point(data = pts_mid %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year), colour = "#f15b2bff", size = 0.2, alpha = 0.1) +
  geom_point(data = pts_mid %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year), colour = "#29aae1ff", size = 0.2, alpha = 0.1) +
  geom_ribbon(data = smooth_roll %>% filter(group == "High (|lat| > 43°)", !is.na(lo), !is.na(hi)),
              aes(y = x, xmin = lo + 12, xmax = hi + 12),
              fill = "#b32025ff", alpha = 0.35, colour = NA, orientation = "y") +
  geom_ribbon(data = smooth_roll %>% filter(group == "Mid-Low (|lat| ≤ 43°)", !is.na(lo), !is.na(hi)),
              aes(y = x, xmin = lo + 12, xmax = hi + 12),
              fill = "#2075bcff", alpha = 0.35, colour = NA, orientation = "y") +
  geom_path(data = smooth_roll %>% filter(group == "High (|lat| > 43°)", !is.na(mean)),
            aes(x = mean + 12, y = x),
            colour = "#b32025ff", linewidth = 0.3) +
  geom_path(data = smooth_roll %>% filter(group == "Mid-Low (|lat| ≤ 43°)", !is.na(mean)),
            aes(x = mean + 12, y = x),
            colour = "#2075bcff", linewidth = 0.3) +
  
  # ===== Right panel =====
geom_point(data = pts_right %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year), colour = "#f15b2bff", size = 0.2, alpha = 0.1) +
  geom_point(data = pts_right %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year), colour = "#29aae1ff", size = 0.2, alpha = 0.1) +
  geom_ribbon(data = lin_preds %>% filter(group == "High (|lat| > 43°)"),
              aes(y = Year, xmin = lwr + 24, xmax = upr + 24),
              fill = "#b32025ff", alpha = 0.3, colour = NA, orientation = "y") +
  geom_ribbon(data = lin_preds %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
              aes(y = Year, xmin = lwr + 24, xmax = upr + 24),
              fill = "#2075bcff", alpha = 0.3, colour = NA, orientation = "y") +
  geom_path(data = lin_preds %>% filter(group == "High (|lat| > 43°)"),
            aes(x = fit + 24, y = Year),
            colour = "#b32025ff", linewidth = 0.3) +
  geom_path(data = lin_preds %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
            aes(x = fit + 24, y = Year),
            colour = "#2075bcff", linewidth = 0.3) +
  
  # ===== Separators =====
geom_vline(xintercept = c(14, 26),
           colour = "gray40", linetype = "dotted", linewidth = 0.2) +
  
  # ===== r labels =====
annotate("text", x = 27.0, y = year_max - 1,
         label = sprintf("High |lat|, r = %.2f",
                         cors$cor[cors$group == "High (|lat| > 43°)"]),
         hjust = 0, vjust = 1, size = 5/2.8, colour = "#b32025ff") +
  annotate("text", x = 27.0, y = year_max - 6,
           label = sprintf("Mid-Low |lat|, r = %.2f",
                           cors$cor[cors$group == "Mid-Low (|lat| ≤ 43°)"]),
           hjust = 0, vjust = 1, size = 5/2.8, colour = "#2075bcff") +
  
  # ===== Axes =====
scale_x_continuous(limits = c(2, 38),
                   breaks = breaks_x, labels = labels_x,
                   expand = expansion(mult = c(0, 0))) +
  scale_y_reverse(limits = c(year_max + 1, year_min - 1),
                  breaks = seq(1970, 2025, by = 10),
                  expand = expansion(mult = c(0.02, 0.02))) +
  
  # ===== Theme =====
theme_minimal(base_size = 5) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.line.x = element_line(linewidth = 0.2, colour = "black"),
    axis.line.y = element_line(linewidth = 0.2, colour = "black"),
    axis.ticks = element_line(linewidth = 0.2, colour = "black"),
    axis.ticks.length = unit(1.5, "pt"),
    axis.title = element_blank(),
    axis.text = element_text(size = 5, colour = "black"),
    legend.position = "none"
  )

# ---- 8) Save ----
ggsave("Birth_figures/birth_season_vs_year_threepanel_ticks.pdf", plot = p, width = 4, height = 2)

#######################################################


################## Birth amplitude by year supplemental figure ##################
# =========================
# Packages
# =========================
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# =========================
# Inputs expected:
#   - birth_by_year: Country, Year, amplitude
#   - periodogram:   Country, Abbrev, Latitude, N_years_birth, Birth_pvals_12m
# =========================

# ---- 0) Join metadata ----
dat <- birth_by_year %>%
  inner_join(
    periodogram %>%
      dplyr::select(Country, Abbrev, Latitude, N_years_birth, Birth_pvals_12m),
    by = "Country"
  )

# ---- 1) Filter for valid entries ----
dat_filt <- dat %>%
  filter(
    !is.na(amplitude),
    !is.na(Year),
    !is.na(Latitude),
    !is.na(N_years_birth), N_years_birth >= 20,
    !is.na(Birth_pvals_12m), Birth_pvals_12m < 1e-10
  ) %>%
  mutate(abs_lat = abs(Latitude))

# ---- 2) Latitude bands ----
hi <- dat_filt %>%
  filter(abs_lat > 43) %>%
  transmute(group = "High (|lat| > 43°)", Year, Amp = amplitude)

midlow <- dat_filt %>%
  filter(abs_lat <= 43) %>%
  transmute(group = "Mid-Low (|lat| ≤ 43°)", Year, Amp = amplitude)

pool_all <- bind_rows(hi, midlow)   # keep all data for smoothing and fits
pool_plot <- pool_all %>% filter(Amp <= 50)   # only used for plotting

year_min <- min(pool_all$Year, na.rm = TRUE)
year_max <- max(pool_all$Year, na.rm = TRUE)

# ---- 3) Moving-window mean ±95% CI (using full data) ----
xs <- seq(max(1970, year_min), year_max) + 0.5
step <- 3
smooth_stats <- function(df) {
  purrr::map_dfr(xs, function(xc) {
    idx <- df$Year >= (xc - step) & df$Year <= (xc + step)
    yy  <- df$Amp[idx]
    n   <- sum(!is.na(yy))
    if (n >= 2) {
      m  <- mean(yy, na.rm = TRUE)
      se <- sd(yy, na.rm = TRUE) / sqrt(n)
      tcrit <- qt(0.975, df = n - 1)
      tibble(x = xc, mean = m, lo = m - tcrit * se, hi = m + tcrit * se, n = n)
    } else {
      tibble(x = xc, mean = NA_real_, lo = NA_real_, hi = NA_real_, n = n)
    }
  })
}
smooth_roll <- pool_all %>%
  nest(.by = group) %>%
  mutate(smooth = purrr::map(data, smooth_stats)) %>%
  dplyr::select(group, smooth) %>%
  unnest(smooth)

# ---- 4) Linear fits and correlations (using full data) ----
lin_fit <- pool_all %>%
  nest(.by = group) %>%
  mutate(
    fit = purrr::map(data, ~ lm(Amp ~ Year, data = .x)),
    cor = purrr::map_dbl(data, ~ cor(.x$Year, .x$Amp, use = "complete.obs")),
    pred = purrr::map(fit, ~ {
      xq <- data.frame(Year = seq(year_min, year_max, length.out = 200))
      cbind(xq, as.data.frame(predict(.x, newdata = xq, interval = "confidence")))
    })
  )
lin_preds <- lin_fit %>% dplyr::select(group, pred) %>% unnest(pred)
cors     <- lin_fit %>% dplyr::select(group, cor)

# ---- 5) Points for all three panels (filtered data only) ----
# Offsets: +0 (left), +50 (middle), +100 (right)
pts_left  <- pool_plot %>% transmute(group, Year, x = Amp)
pts_mid   <- pool_plot %>% transmute(group, Year, x = Amp + 50)
pts_right <- pool_plot %>% transmute(group, Year, x = Amp + 100)

# ---- 6) Tick marks ----
base_ticks <- seq(0, 50, by = 20)
breaks_x <- c(base_ticks, base_ticks + 50, base_ticks + 100)
labels_x <- rep(base_ticks, 3)

# ---- 7) Plot ----
p <- ggplot() +
  # ===== Left panel =====
geom_point(data = pts_left %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year),
           colour = "#f15b2bff", size = 0.25, alpha = 0.15) +
  geom_point(data = pts_left %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year),
             colour = "#29aae1ff", size = 0.25, alpha = 0.15) +
  
  # ===== Middle panel =====
geom_point(data = pts_mid %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year),
           colour = "#f15b2bff", size = 0.25, alpha = 0.15) +
  geom_point(data = pts_mid %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year),
             colour = "#29aae1ff", size = 0.25, alpha = 0.15) +
  geom_ribbon(data = smooth_roll %>% filter(group == "High (|lat| > 43°)"),
              aes(y = x, xmin = lo + 50, xmax = hi + 50),
              fill = "#b32025ff", alpha = 0.35, colour = NA, orientation = "y") +
  geom_ribbon(data = smooth_roll %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
              aes(y = x, xmin = lo + 50, xmax = hi + 50),
              fill = "#2075bcff", alpha = 0.35, colour = NA, orientation = "y") +
  geom_path(data = smooth_roll %>% filter(group == "High (|lat| > 43°)"),
            aes(x = mean + 50, y = x),
            colour = "#b32025ff", linewidth = 0.3) +
  geom_path(data = smooth_roll %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
            aes(x = mean + 50, y = x),
            colour = "#2075bcff", linewidth = 0.3) +
  
  # ===== Right panel =====
geom_point(data = pts_right %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year),
           colour = "#f15b2bff", size = 0.25, alpha = 0.15) +
  geom_point(data = pts_right %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year),
             colour = "#29aae1ff", size = 0.25, alpha = 0.15) +
  geom_ribbon(data = lin_preds %>% filter(group == "High (|lat| > 43°)"),
              aes(y = Year, xmin = lwr + 100, xmax = upr + 100),
              fill = "#b32025ff", alpha = 0.3, colour = NA, orientation = "y") +
  geom_ribbon(data = lin_preds %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
              aes(y = Year, xmin = lwr + 100, xmax = upr + 100),
              fill = "#2075bcff", alpha = 0.3, colour = NA, orientation = "y") +
  geom_path(data = lin_preds %>% filter(group == "High (|lat| > 43°)"),
            aes(x = fit + 100, y = Year),
            colour = "#b32025ff", linewidth = 0.3) +
  geom_path(data = lin_preds %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
            aes(x = fit + 100, y = Year),
            colour = "#2075bcff", linewidth = 0.3) +
  
  # ===== Separators =====
geom_vline(xintercept = c(50, 100),
           colour = "gray40", linetype = "dotted", linewidth = 0.25) +
  
  # ===== r labels =====
annotate("text", x = 105, y = year_max - 1,
         label = sprintf("High |lat|, r = %.2f",
                         cors$cor[cors$group == "High (|lat| > 43°)"]),
         hjust = 0, vjust = 1, size = 5/2.8, colour = "#b32025ff") +
  annotate("text", x = 105, y = year_max - 6,
           label = sprintf("Mid-Low |lat|, r = %.2f",
                           cors$cor[cors$group == "Mid-Low (|lat| ≤ 43°)"]),
           hjust = 0, vjust = 1, size = 5/2.8, colour = "#2075bcff") +
  
  # ===== Axes =====
scale_x_continuous(limits = c(0, 150),
                   breaks = breaks_x, labels = labels_x,
                   expand = expansion(mult = c(0, 0))) +
  scale_y_reverse(limits = c(year_max + 1, year_min - 1),
                  breaks = seq(1970, 2025, by = 10),
                  expand = expansion(mult = c(0.02, 0.02))) +
  
  # ===== Theme =====
theme_minimal(base_size = 5) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.line.x = element_line(linewidth = 0.2, colour = "black"),
    axis.line.y = element_line(linewidth = 0.2, colour = "black"),
    axis.ticks = element_line(linewidth = 0.2, colour = "black"),
    axis.ticks.length = unit(1.5, "pt"),
    axis.title = element_blank(),
    axis.text = element_text(size = 5, colour = "black"),
    legend.position = "none"
  )

# ---- 8) Save ----
ggsave("Birth_figures/birth_amplitude_vs_year_threepanel_trimmed.pdf", plot = p, width = 4, height = 2)

#######################################################


################## Death season by year supplemental figure ##################
# =========================
# Packages
# =========================
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# =========================
# Inputs expected:
#   - death_by_year: Country, Year, birth_mean_month (renamed here to mean_month)
#   - periodogram:   Country, Abbrev, Latitude, N_years_death, Death_pvals_12m
# =========================

# ---- 0) Join metadata ----
dat <- death_by_year %>%
  inner_join(
    periodogram %>%
      dplyr::select(Country, Abbrev, Latitude, N_years_death, Death_pvals_12m),
    by = "Country"
  )

# ---- 1) Filter (keep all hemispheres, use absolute latitude) ----
dat_filt <- dat %>%
  filter(
    !is.na(birth_mean_month),
    !is.na(Year),
    !is.na(Latitude),
    !is.na(N_years_death), N_years_death >= 20,
    !is.na(Death_pvals_12m), Death_pvals_12m < 1e-10
  ) %>%
  mutate(abs_lat = abs(Latitude))

# ---- 2) Latitude bands and month wrapping ----
hi <- dat_filt %>%
  filter(abs_lat > 43) %>%
  transmute(group = "High (|lat| > 43°)", Year,
            MonthWrapped = ifelse(birth_mean_month < 5.5,
                                  birth_mean_month + 12,
                                  birth_mean_month))

midlow <- dat_filt %>%
  filter(abs_lat <= 43) %>%
  transmute(group = "Mid-Low (|lat| ≤ 43°)", Year,
            MonthWrapped = ifelse(birth_mean_month < 5.5,
                                  birth_mean_month + 12,
                                  birth_mean_month))

pool <- bind_rows(hi, midlow)
year_min <- min(pool$Year, na.rm = TRUE)
year_max <- max(pool$Year, na.rm = TRUE)

# =========================
# NEW: Restrict model fits and smoothing to ≤ 2018
# =========================
pool_fit <- pool %>% filter(Year <= 2018)

# ---- 3) Moving-window mean ±95% CI ----
xs <- seq(max(1970, year_min), 2018) + 0.5   # stop smoothing at 2018
step <- 3
smooth_stats <- function(df) {
  purrr::map_dfr(xs, function(xc) {
    idx <- df$Year >= (xc - step) & df$Year <= (xc + step)
    yy  <- df$MonthWrapped[idx]
    n   <- sum(!is.na(yy))
    if (n >= 2) {
      m  <- mean(yy, na.rm = TRUE)
      se <- sd(yy, na.rm = TRUE) / sqrt(n)
      tcrit <- qt(0.975, df = n - 1)
      tibble(x = xc, mean = m, lo = m - tcrit * se, hi = m + tcrit * se, n = n)
    } else {
      tibble(x = xc, mean = NA_real_, lo = NA_real_, hi = NA_real_, n = n)
    }
  })
}
smooth_roll <- pool_fit %>%
  nest(.by = group) %>%
  mutate(smooth = purrr::map(data, smooth_stats)) %>%
  dplyr::select(group, smooth) %>%
  unnest(smooth)

# ---- 4) Linear fits (only ≤ 2018) ----
lin_fit <- pool_fit %>%
  nest(.by = group) %>%
  mutate(
    fit = purrr::map(data, ~ lm(MonthWrapped ~ Year, data = .x)),
    cor = purrr::map_dbl(data, ~ cor(.x$Year, .x$MonthWrapped, use = "complete.obs")),
    pred = purrr::map(fit, ~ {
      xq <- data.frame(Year = seq(year_min, 2018, length.out = 200))
      cbind(xq, as.data.frame(predict(.x, newdata = xq, interval = "confidence")))
    })
  )
lin_preds <- lin_fit %>% dplyr::select(group, pred) %>% unnest(pred)
cors     <- lin_fit %>% dplyr::select(group, cor)

# ---- 5) Points for all three panels (range 5–17 each) ----
pts_left  <- pool %>% transmute(group, Year, x = MonthWrapped)       %>% filter(x >= 5, x <= 17)
pts_mid   <- pool %>% transmute(group, Year, x = MonthWrapped + 12)  %>% filter(x >= 17, x <= 29)
pts_right <- pool %>% transmute(group, Year, x = MonthWrapped + 24)  %>% filter(x >= 29, x <= 41)

# ---- 6) Month tick marks ----
breaks_x <- rep(c(5.5, 8.5, 11.5, 14.5), 3) + rep(c(0, 12, 24), each = 4)
labels_x <- rep(c("Jun", "Sep", "Dec", "Mar"), times = 3)

# ---- 7) Plot ----
p <- ggplot() +
  # ===== Left panel =====
geom_point(data = pts_left %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year), colour = "#f15b2bff", size = 0.2, alpha = 0.1) +
  geom_point(data = pts_left %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year), colour = "#29aae1ff", size = 0.2, alpha = 0.1) +
  
  # ===== Middle panel =====
geom_point(data = pts_mid %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year), colour = "#f15b2bff", size = 0.2, alpha = 0.1) +
  geom_point(data = pts_mid %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year), colour = "#29aae1ff", size = 0.2, alpha = 0.1) +
  geom_ribbon(data = smooth_roll %>% filter(group == "High (|lat| > 43°)", !is.na(lo), !is.na(hi)),
              aes(y = x, xmin = lo + 12, xmax = hi + 12),
              fill = "#b32025ff", alpha = 0.35, colour = NA, orientation = "y") +
  geom_ribbon(data = smooth_roll %>% filter(group == "Mid-Low (|lat| ≤ 43°)", !is.na(lo), !is.na(hi)),
              aes(y = x, xmin = lo + 12, xmax = hi + 12),
              fill = "#2075bcff", alpha = 0.35, colour = NA, orientation = "y") +
  geom_path(data = smooth_roll %>% filter(group == "High (|lat| > 43°)", !is.na(mean)),
            aes(x = mean + 12, y = x),
            colour = "#b32025ff", linewidth = 0.3) +
  geom_path(data = smooth_roll %>% filter(group == "Mid-Low (|lat| ≤ 43°)", !is.na(mean)),
            aes(x = mean + 12, y = x),
            colour = "#2075bcff", linewidth = 0.3) +
  
  # ===== Right panel =====
geom_point(data = pts_right %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year), colour = "#f15b2bff", size = 0.2, alpha = 0.1) +
  geom_point(data = pts_right %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year), colour = "#29aae1ff", size = 0.2, alpha = 0.1) +
  geom_ribbon(data = lin_preds %>% filter(group == "High (|lat| > 43°)"),
              aes(y = Year, xmin = lwr + 24, xmax = upr + 24),
              fill = "#b32025ff", alpha = 0.3, colour = NA, orientation = "y") +
  geom_ribbon(data = lin_preds %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
              aes(y = Year, xmin = lwr + 24, xmax = upr + 24),
              fill = "#2075bcff", alpha = 0.3, colour = NA, orientation = "y") +
  geom_path(data = lin_preds %>% filter(group == "High (|lat| > 43°)"),
            aes(x = fit + 24, y = Year),
            colour = "#b32025ff", linewidth = 0.3) +
  geom_path(data = lin_preds %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
            aes(x = fit + 24, y = Year),
            colour = "#2075bcff", linewidth = 0.3) +
  
  # ===== Separators =====
geom_vline(xintercept = c(17, 29),
           colour = "gray40", linetype = "dotted", linewidth = 0.2) +
  
  # ===== r labels (computed using ≤ 2018 data) =====
annotate("text", x = 30, y = year_max - 1,
         label = sprintf("High |lat|, r = %.2f",
                         cors$cor[cors$group == "High (|lat| > 43°)"]),
         hjust = 0, vjust = 1, size = 5/2.8, colour = "#b32025ff") +
  annotate("text", x = 30, y = year_max - 6,
           label = sprintf("Mid-Low |lat|, r = %.2f",
                           cors$cor[cors$group == "Mid-Low (|lat| ≤ 43°)"]),
           hjust = 0, vjust = 1, size = 5/2.8, colour = "#2075bcff") +
  
  # ===== Axes =====
scale_x_continuous(limits = c(5, 41),
                   breaks = breaks_x, labels = labels_x,
                   expand = expansion(mult = c(0, 0))) +
  scale_y_reverse(limits = c(year_max + 1, year_min - 1),
                  breaks = seq(1970, 2025, by = 10),
                  expand = expansion(mult = c(0.02, 0.02))) +
  
  # ===== Theme =====
theme_minimal(base_size = 5) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.line.x = element_line(linewidth = 0.2, colour = "black"),
    axis.line.y = element_line(linewidth = 0.2, colour = "black"),
    axis.ticks = element_line(linewidth = 0.2, colour = "black"),
    axis.ticks.length = unit(1.5, "pt"),
    axis.title = element_blank(),
    axis.text = element_text(size = 5, colour = "black"),
    legend.position = "none"
  )

# ---- 8) Save ----
ggsave("Birth_figures/death_season_vs_year_threepanel_until2018.pdf", plot = p, width = 4, height = 2)

#######################################################


################## Death amplitude by year supplemental figure ##################
# =========================
# Packages
# =========================
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# =========================
# Inputs expected:
#   - death_by_year: Country, Year, amplitude
#   - periodogram:   Country, Abbrev, Latitude, N_years_death, Death_pvals_12m
# =========================

# ---- 0) Join metadata ----
dat <- death_by_year %>%
  inner_join(
    periodogram %>%
      dplyr::select(Country, Abbrev, Latitude, N_years_death, Death_pvals_12m),
    by = "Country"
  )

# ---- 1) Filter valid entries ----
dat_filt <- dat %>%
  filter(
    !is.na(amplitude),
    !is.na(Year),
    !is.na(Latitude),
    !is.na(N_years_death), N_years_death >= 20,
    !is.na(Death_pvals_12m), Death_pvals_12m < 1e-10
  ) %>%
  mutate(abs_lat = abs(Latitude))

# ---- 2) Latitude bands ----
hi <- dat_filt %>%
  filter(abs_lat > 43) %>%
  transmute(group = "High (|lat| > 43°)", Year, Amp = amplitude)

midlow <- dat_filt %>%
  filter(abs_lat <= 43) %>%
  transmute(group = "Mid-Low (|lat| ≤ 43°)", Year, Amp = amplitude)

pool_all <- bind_rows(hi, midlow)         # all data for smoothing + fits
pool_plot <- pool_all %>% filter(Amp <= 50)  # filtered only for plotting

year_min <- min(pool_all$Year, na.rm = TRUE)
year_max <- max(pool_all$Year, na.rm = TRUE)

# ---- 3) Restrict smoothing & fits to years ≤ 2018 ----
pool_fit <- pool_all %>% filter(Year <= 2018)

# ---- 4) Moving-window mean ±95% CI (years ≤ 2018, full data) ----
xs <- seq(max(1970, year_min), 2018) + 0.5
step <- 3
smooth_stats <- function(df) {
  purrr::map_dfr(xs, function(xc) {
    idx <- df$Year >= (xc - step) & df$Year <= (xc + step)
    yy  <- df$Amp[idx]
    n   <- sum(!is.na(yy))
    if (n >= 2) {
      m  <- mean(yy, na.rm = TRUE)
      se <- sd(yy, na.rm = TRUE) / sqrt(n)
      tcrit <- qt(0.975, df = n - 1)
      tibble(x = xc, mean = m, lo = m - tcrit * se, hi = m + tcrit * se, n = n)
    } else {
      tibble(x = xc, mean = NA_real_, lo = NA_real_, hi = NA_real_, n = n)
    }
  })
}

smooth_roll <- pool_fit %>%
  nest(.by = group) %>%
  mutate(smooth = purrr::map(data, smooth_stats)) %>%
  dplyr::select(group, smooth) %>%
  unnest(smooth)

# ---- 5) Linear fits (years ≤ 2018, full data) ----
lin_fit <- pool_fit %>%
  nest(.by = group) %>%
  mutate(
    fit = purrr::map(data, ~ lm(Amp ~ Year, data = .x)),
    cor = purrr::map_dbl(data, ~ cor(.x$Year, .x$Amp, use = "complete.obs")),
    pred = purrr::map(fit, ~ {
      xq <- data.frame(Year = seq(year_min, 2018, length.out = 200))
      cbind(xq, as.data.frame(predict(.x, newdata = xq, interval = "confidence")))
    })
  )
lin_preds <- lin_fit %>% dplyr::select(group, pred) %>% unnest(pred)
cors     <- lin_fit %>% dplyr::select(group, cor)

# ---- 6) Points for all three panels (filtered data only) ----
# Offsets: +0 (left), +50 (middle), +100 (right)
pts_left  <- pool_plot %>% transmute(group, Year, x = Amp)
pts_mid   <- pool_plot %>% transmute(group, Year, x = Amp + 50)
pts_right <- pool_plot %>% transmute(group, Year, x = Amp + 100)

# ---- 7) Tick marks ----
base_ticks <- seq(0, 50, by = 20)
breaks_x <- c(base_ticks, base_ticks + 50, base_ticks + 100)
labels_x <- rep(base_ticks, 3)

# ---- 8) Plot ----
p <- ggplot() +
  # ===== Left panel =====

geom_point(data = pts_left %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year),
           colour = "#f15b2bff", size = 0.25, alpha = 0.15) +
  
  geom_point(data = pts_left %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year),
             colour = "#29aae1ff", size = 0.25, alpha = 0.15) +
  
  # ===== Middle panel =====
geom_point(data = pts_mid %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year),
           colour = "#f15b2bff", size = 0.25, alpha = 0.15) +
  geom_point(data = pts_mid %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year),
             colour = "#29aae1ff", size = 0.25, alpha = 0.15) +
  geom_ribbon(data = smooth_roll %>% filter(group == "High (|lat| > 43°)"),
              aes(y = x, xmin = lo + 50, xmax = hi + 50),
              fill = "#b32025ff", alpha = 0.35, colour = NA, orientation = "y") +
  geom_ribbon(data = smooth_roll %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
              aes(y = x, xmin = lo + 50, xmax = hi + 50),
              fill = "#2075bcff", alpha = 0.35, colour = NA, orientation = "y") +
  geom_path(data = smooth_roll %>% filter(group == "High (|lat| > 43°)"),
            aes(x = mean + 50, y = x),
            colour = "#b32025ff", linewidth = 0.3) +
  geom_path(data = smooth_roll %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
            aes(x = mean + 50, y = x),
            colour = "#2075bcff", linewidth = 0.3) +
  
  # ===== Right panel =====
geom_point(data = pts_right %>% filter(group == "High (|lat| > 43°)"),
           aes(x = x, y = Year),
           colour = "#f15b2bff", size = 0.25, alpha = 0.15) +
  geom_point(data = pts_right %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
             aes(x = x, y = Year),
             colour = "#29aae1ff", size = 0.25, alpha = 0.15) +
  geom_ribbon(data = lin_preds %>% filter(group == "High (|lat| > 43°)"),
              aes(y = Year, xmin = lwr + 100, xmax = upr + 100),
              fill = "#b32025ff", alpha = 0.3, colour = NA, orientation = "y") +
  geom_ribbon(data = lin_preds %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
              aes(y = Year, xmin = lwr + 100, xmax = upr + 100),
              fill = "#2075bcff", alpha = 0.3, colour = NA, orientation = "y") +
  geom_path(data = lin_preds %>% filter(group == "High (|lat| > 43°)"),
            aes(x = fit + 100, y = Year),
            colour = "#b32025ff", linewidth = 0.3) +
  geom_path(data = lin_preds %>% filter(group == "Mid-Low (|lat| ≤ 43°)"),
            aes(x = fit + 100, y = Year),
            colour = "#2075bcff", linewidth = 0.3) +
  
  # ===== Separators =====
geom_vline(xintercept = c(50, 100),
           colour = "gray40", linetype = "dotted", linewidth = 0.25) +
  
  # ===== r labels =====
annotate("text", x = 105, y = year_max - 1,
         label = sprintf("High |lat|, r = %.2f",
                         cors$cor[cors$group == "High (|lat| > 43°)"]),
         hjust = 0, vjust = 1, size = 5/2.8, colour = "#b32025ff") +
  annotate("text", x = 105, y = year_max - 6,
           label = sprintf("Mid-Low |lat|, r = %.2f",
                           cors$cor[cors$group == "Mid-Low (|lat| ≤ 43°)"]),
           hjust = 0, vjust = 1, size = 5/2.8, colour = "#2075bcff") +
  
  # ===== Axes =====
scale_x_continuous(limits = c(0, 150),
                   breaks = breaks_x, labels = labels_x,
                   expand = expansion(mult = c(0, 0))) +
  scale_y_reverse(limits = c(year_max + 1, year_min - 1),
                  breaks = seq(1970, 2025, by = 10),
                  expand = expansion(mult = c(0.02, 0.02))) +
  
  # ===== Theme =====
theme_minimal(base_size = 5) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.line.x = element_line(linewidth = 0.2, colour = "black"),
    axis.line.y = element_line(linewidth = 0.2, colour = "black"),
    axis.ticks = element_line(linewidth = 0.2, colour = "black"),
    axis.ticks.length = unit(1.5, "pt"),
    axis.title = element_blank(),
    axis.text = element_text(size = 5, colour = "black"),
    legend.position = "none"
  )

# ---- 9) Save ----
ggsave("Birth_figures/death_amplitude_vs_year_threepanel_trimmed.pdf", plot = p, width = 4, height = 2)

#######################################################







