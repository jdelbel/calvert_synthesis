# Modularized functions for fluorescence correction

# Function to prepare and merge datasets
prepare_fluorescence_data <- function(fluor_file, chl_file) {
  # Read data
  f <- read_csv(fluor_file)
  c <- read_csv(chl_file)
  
  # Process fluorescence data
  f_dm <- f %>% 
    group_by(date, station, pres) %>% 
    summarise(f_dm = mean(flu_cor, na.rm = TRUE),
              n_prof = n(),
              .groups = "drop") %>% 
    mutate(f_dm = round(f_dm, 2)) %>% 
    unite(id, c(date, station), sep = "-", remove = FALSE)
  
  # Process chlorophyll data
  c_processed <- prepare_chlorophyll_data(c)
  
  # Join datasets
  f_dm %>% 
    left_join(c_processed, by = c("date", "station", "id"))
}

# Function to prepare chlorophyll data (bulk + size-fractionated)
prepare_chlorophyll_data <- function(chl_data) {
  # Bulk chlorophyll processing
  c_bulk <- chl_data %>% 
    filter(filter_type == "Bulk GF/F",
           chla_flag %in% c("AV", "SVC", "ADL") | is.na(chla_flag)) %>% 
    group_by(date, station = site_id, line_out_depth) %>% 
    summarise(chl_dm = mean(chla, na.rm = TRUE), .groups = "drop") %>% 
    mutate(pres = ifelse(round(line_out_depth) == 0, 1, round(line_out_depth)))
  
  # Size-fractionated processing
  c_sf <- chl_data %>% 
    filter(filter_type != "Bulk GF/F",
           chla_flag %in% c("AV", "SVC", "ADL") | is.na(chla_flag),
           !is.na(chla), chla > 0) %>% 
    group_by(date, station = site_id, line_out_depth, filter_type) %>% 
    summarise(avg_chla = mean(chla, na.rm = TRUE), .groups = "drop") %>% 
    group_by(date, station, line_out_depth) %>% 
    filter(n() == 3) %>%  # Require all 3 size fractions
    summarise(sum_sf = sum(avg_chla, na.rm = TRUE), .groups = "drop") %>% 
    mutate(pres = ifelse(round(line_out_depth) == 0, 1, round(line_out_depth)))
  
  # Combine bulk and size-fractionated
  c_bulk %>% 
    full_join(c_sf, by = c("date", "station", "pres")) %>% 
    mutate(chl_comb = coalesce(chl_dm, sum_sf)) %>% 
    select(date, station, pres, chl_dm, chl_comb) %>% 
    unite(id, c(date, station), sep = "-", remove = FALSE)
}

# Function to fit slope corrections
fit_slope_corrections <- function(data, outlier_ids = NULL) {
  # Remove specified outliers
  if (!is.null(outlier_ids)) {
    data <- data %>% 
      anti_join(outlier_ids, by = c("id", "pres"))
  }
  
  data %>% 
    filter(!is.na(chl_comb), !is.na(f_dm)) %>%
    group_by(date, station) %>% 
    summarise(
      fit_results = list(fit_slope_model(cur_data())),
      .groups = "drop"
    ) %>% 
    unnest_wider(fit_results) %>% 
    unite(id, c(date, station), sep = "-", remove = FALSE)
}

# Helper function for individual slope fitting
fit_slope_model <- function(data) {
  # Separate data with and without surface
  data_no_surf <- filter(data, pres != 1)
  data_with_surf <- data
  
  # Fit models if sufficient data
  models <- list(
    no_surf = if(nrow(data_no_surf) >= 2) lm(chl_comb ~ f_dm, data = data_no_surf) else NULL,
    with_surf = if(nrow(data_with_surf) >= 2) lm(chl_comb ~ f_dm, data = data_with_surf) else NULL
  )
  
  # Extract model statistics
  stats <- map_dfr(models, extract_model_stats, .id = "model_type")
  
  # Select best model
  best_model <- select_best_model(stats)
  
  return(best_model)
}

# Function to extract model statistics
extract_model_stats <- function(model) {
  if (is.null(model)) return(tibble(slope = NA, r2 = NA, p_value = NA, nobs = NA))
  
  coefs <- summary(model)$coefficients
  tibble(
    intercept = round(coef(model)[1], 2),
    slope = round(coef(model)[2], 2),
    r2 = round(summary(model)$adj.r.squared, 2),
    p_value = round(coefs[2, 4], 5),
    nobs = nobs(model)
  )
}

# Function to select best model
select_best_model <- function(stats) {
  # Implement your model selection logic here
  # (simplified version of your existing logic)
  if (all(is.na(stats$slope))) return(stats[1, ])
  
  valid_models <- stats %>% filter(!is.na(slope), p_value <= 0.05, slope > 0)
  
  if (nrow(valid_models) == 0) return(stats[1, ] %>% mutate(slope = NA))
  
  # Select model with lowest p-value
  best <- valid_models %>% slice_min(p_value, n = 1)
  
  return(best)
}

# Function for NPQ correction
apply_npq_correction <- function(data, depth_threshold = 6) {
  # Identify quenched profiles
  quenched_profiles <- data %>% 
    filter(!is.na(chl_comb), !is.na(f_slope)) %>% 
    mutate(diff = f_slope - chl_comb) %>%
    group_by(id) %>%
    summarise(is_quenched = any(diff <= -1 & pres <= 5, na.rm = TRUE),
              .groups = "drop") %>%
    filter(is_quenched) %>%
    pull(id)
  
  # Apply LOESS correction
  data %>% 
    group_by(id) %>%
    mutate(
      chl_loess = if_else(
        id %in% quenched_profiles,
        fit_loess_safe(cur_data()),
        NA_real_
      ),
      diff_ratio = f_slope / chl_loess,
      f_npq = case_when(
        pres <= depth_threshold & !is.na(chl_loess) & diff_ratio < 0.8 ~ chl_loess,
        TRUE ~ f_slope
      ),
      npq_cor = pres <= depth_threshold & !is.na(chl_loess) & diff_ratio < 0.8
    ) %>%
    ungroup()
}

# Main workflow function
process_fluorescence_corrections <- function(fluor_file, chl_file, outlier_ids = NULL) {
  # Prepare data
  message("Preparing fluorescence and chlorophyll data...")
  merged_data <- prepare_fluorescence_data(fluor_file, chl_file)
  
  # Fit slope corrections
  message("Fitting slope corrections...")
  slope_fits <- fit_slope_corrections(merged_data, outlier_ids)
  
  # Apply slope corrections
  message("Applying slope corrections...")
  corrected_data <- merged_data %>% 
    left_join(slope_fits, by = c("date", "station", "id")) %>% 
    mutate(f_slope = f_dm * slope)
  
  # Apply NPQ corrections
  message("Applying NPQ corrections...")
  final_data <- apply_npq_correction(corrected_data)
  
  return(final_data)
}