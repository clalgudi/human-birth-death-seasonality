# =========================
# 0) Packages
# =========================
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# =========================
# 1) Input files
# =========================
files <- c(
  "DNKmonthly.txt",
  "NLDmonthly.txt",
  "FRAmonthly.txt",
  "CHEmonthly.txt",
  "USAmonthly.txt"
)

# =========================
# 2) Exact MATLAB colormap recreation
# =========================
kk  <- 50L
n1  <- as.integer(round(0.7 * kk))

r <- c(rep(0, n1),
       seq(0, 1, length.out = kk),
       rep(1, kk),
       seq(1, 0.7, length.out = n1))

g <- c(rep(0, n1),
       seq(0, 1, length.out = kk),
       seq(1, 0, length.out = kk),
       rep(0, n1))

b <- c(seq(0.7, 1, length.out = n1),
       rep(1, kk),
       seq(1, 0, length.out = kk),
       rep(0, n1))

col_hex <- rgb(
  pmin(pmax(r, 0), 1),
  pmin(pmax(g, 0), 1),
  pmin(pmax(b, 0), 1)
)

# =========================
# 3) Loop over countries
# =========================
for (f in files) {
  
  message("Processing ", f)
  
  # -------------------------
  # Read + tidy
  # -------------------------
  dat_raw <- read_csv(f, show_col_types = FALSE)
  
  dat_tidy <- dat_raw %>%
    transmute(
      year   = suppressWarnings(as.integer(Year)),
      monthn = suppressWarnings(as.integer(Month)),
      births = suppressWarnings(as.numeric(Births))
    ) %>%
    filter(!is.na(year), !is.na(monthn), !is.na(births), monthn %in% 1:12) %>%
    group_by(year, monthn) %>%
    summarise(births = sum(births, na.rm = TRUE), .groups = "drop")
  
  # -------------------------
  # Normalize to deviation
  # -------------------------
  dat_norm <- dat_tidy %>%
    mutate(
      n_days = case_when(
        monthn %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31,
        monthn %in% c(4, 6, 9, 11) ~ 30,
        monthn == 2 ~ 28,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(year) %>%
    mutate(
      total_births = sum(births, na.rm = TRUE),
      total_days   = sum(n_days,  na.rm = TRUE),
      rate = (births / total_births) / n_days * (total_days / 12),
      deviation = (rate - 1/12) * 12 * 100,
      deviation_capped = pmax(pmin(deviation, 20), -20)
    ) %>%
    ungroup() %>%
    mutate(country = str_remove(basename(f), "monthly.txt$"))
  
  # -------------------------
  # Missing years grid
  # -------------------------
  all_years <- 1861:2025
  missing_years <- setdiff(all_years, unique(dat_norm$year))
  
  dot_grid <- expand.grid(
    year = missing_years,
    monthn = seq(0.5, 11.5, by = 1)
  )
  
  # -------------------------
  # Plot
  # -------------------------
  p <- ggplot() +
    geom_tile(
      data = dat_norm,
      aes(x = monthn - 0.5, y = year, fill = deviation),
      width = 1, height = 1
    ) +
    geom_point(
      data = dot_grid,
      aes(x = monthn, y = year),
      color = "gray80",
      size = 0.02,
      alpha = 0.8
    ) +
    scale_fill_gradientn(
      colors = col_hex,
      limits = c(-25, 25),
      oob = squish,
      guide = "none"
    ) +
    scale_x_continuous(
      limits = c(0, 12),
      breaks = c(2.5, 5.5, 8.5, 11.5),
      labels = NULL,
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_reverse(
      limits = c(2025, 1861),
      breaks = seq(1880, 2020, by = 20),
      labels = NULL,
      expand = expansion(mult = c(0, 0))
    ) +
    theme_minimal(base_size = 5) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_text(size = 5),
      axis.ticks = element_line(linewidth = 0.2),
      axis.ticks.length = unit(0.5, "mm"),
      axis.line = element_line(linewidth = 0.2, colour = "black"),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
  
  # -------------------------
  # Save
  # -------------------------
  out_file <- paste0(
    "Birth_figures/",
    str_remove(basename(f), "\\.txt$"),
    "_birth_heatmap.pdf"
  )
  
  ggsave(
    out_file,
    p,
    width = 0.75,
    height = 5,
    units = "in",
    device = "pdf"
  )
}
