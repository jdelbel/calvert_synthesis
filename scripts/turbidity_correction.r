
# Required libraries
library(tidyverse)
library(lubridate)
library(here)

# Initialize flagged casts tibble - MOVED TO GLOBAL ENVIRONMENT
flagged_casts <- tibble(
  castpk = character(),
  flag_type = character(),
  flagged_datetime = as_datetime(character())
)

# Initialize automated QC results
auto_qc_results <- tibble(
  castpk = character(),
  qc_flag = character(),
  qc_reason = character(),
  qc_value = numeric()
)

# Automated QC function
run_automated_qc <- function(data, 
                             spike_threshold = 5,        # FTU increase from surface
                             spike_depth_limit = 10,     # meters from surface
                             max_turbidity = 50,         # Maximum reasonable turbidity
                             min_observations = 5,       # Minimum observations per cast
                             max_range = 100) {          # Maximum turbidity range
  
  cat("Running automated QC checks...\n")
  
  # Get unique casts
  unique_casts <- data %>% distinct(castpk) %>% pull(castpk)
  
  # Initialize results
  qc_results <- tibble(
    castpk = character(),
    qc_flag = character(), 
    qc_reason = character(),
    qc_value = numeric()
  )
  
  for (cast_id in unique_casts) {
    profile_data <- data %>% 
      filter(castpk == cast_id) %>%
      arrange(pres)  # Ensure sorted by pressure
    
    # Skip if insufficient data
    if (nrow(profile_data) < min_observations) {
      qc_results <- bind_rows(qc_results, tibble(
        castpk = as.character(cast_id),
        qc_flag = "insufficient_data",
        qc_reason = paste("Only", nrow(profile_data), "observations"),
        qc_value = nrow(profile_data)
      ))
      next
    }
    
    # Check 1: Unrealistic maximum values
    max_turb <- max(profile_data$t, na.rm = TRUE)
    if (max_turb > max_turbidity) {
      qc_results <- bind_rows(qc_results, tibble(
        castpk = as.character(cast_id),
        qc_flag = "high_values",
        qc_reason = paste("Max turbidity", round(max_turb, 2), "FTU"),
        qc_value = max_turb
      ))
    }
    
    # Check 2: Unrealistic range
    turb_range <- max_turb - min(profile_data$t, na.rm = TRUE)
    if (turb_range > max_range) {
      qc_results <- bind_rows(qc_results, tibble(
        castpk = as.character(cast_id),
        qc_flag = "high_range",
        qc_reason = paste("Range", round(turb_range, 2), "FTU"),
        qc_value = turb_range
      ))
    }
    
    # Check 3: Surface spike detection
    # Find observations within spike_depth_limit of surface
    surface_data <- profile_data %>% 
      filter(pres <= spike_depth_limit) %>%
      arrange(pres)
    
    if (nrow(surface_data) >= 3) {
      # Get surface value (shallowest measurement)
      surface_turb <- surface_data$t[1]
      
      # Find minimum turbidity in the upper water column (next few measurements)
      if (nrow(surface_data) >= 4) {
        subsurface_min <- min(surface_data$t[2:min(4, nrow(surface_data))], na.rm = TRUE)
        spike_magnitude <- surface_turb - subsurface_min
        
        if (spike_magnitude > spike_threshold) {
          qc_results <- bind_rows(qc_results, tibble(
            castpk = as.character(cast_id),
            qc_flag = "surface_spike",
            qc_reason = paste("Surface spike", round(spike_magnitude, 2), "FTU above subsurface"),
            qc_value = spike_magnitude
          ))
        }
      }
    }
    
    # Check 4: Profile shape issues (monotonic decrease indicating sensor problems)
    if (nrow(profile_data) >= 10) {
      # Check if turbidity consistently decreases with depth (unusual)
      depth_turb_cor <- cor(profile_data$pres, profile_data$t, use = "complete.obs")
      if (!is.na(depth_turb_cor) && depth_turb_cor < -0.8) {
        qc_results <- bind_rows(qc_results, tibble(
          castpk = as.character(cast_id),
          qc_flag = "monotonic_decrease",
          qc_reason = paste("Strong negative correlation with depth (r =", round(depth_turb_cor, 2), ")"),
          qc_value = depth_turb_cor
        ))
      }
    }
  }
  
  # Update global variable
  assign("auto_qc_results", qc_results, envir = .GlobalEnv)
  
  # Summary
  flag_summary <- qc_results %>% 
    count(qc_flag, name = "count") %>%
    arrange(desc(count))
  
  cat("Automated QC complete!\n")
  cat("Total casts checked:", length(unique_casts), "\n")
  cat("Casts with QC flags:", length(unique(qc_results$castpk)), "\n\n")
  
  if (nrow(flag_summary) > 0) {
    cat("QC Flag Summary:\n")
    for (i in 1:nrow(flag_summary)) {
      cat("  -", flag_summary$qc_flag[i], ":", flag_summary$count[i], "casts\n")
    }
  }
  
  return(qc_results)
}

# Function to plot and inspect individual profiles
inspect_profile <- function(data, cast_id) {
  # Filter data for specific cast
  profile_data <- data %>% 
    filter(castpk == cast_id)
  
  # Check for automated QC flags for this cast
  auto_flags <- auto_qc_results %>% 
    filter(castpk == cast_id)
  
  # Create the plot
  plot_color <- if (nrow(auto_flags) > 0) "red" else "darkblue"
  
  p <- profile_data %>% 
    ggplot(aes(x = t, y = -pres)) +
    geom_line(color = plot_color, linewidth = 1, orientation = "y") +
    geom_point(color = plot_color, size = 0.8, alpha = 0.7) +
    labs(
      title = str_glue("Cast ID: {cast_id}"),
      x = "Turbidity",
      y = "Depth (negative pressure)",
      subtitle = str_glue("n = {nrow(profile_data)} observations")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12)
    )
  
  # Add automated QC flag annotation if present
  if (nrow(auto_flags) > 0) {
    p <- p + 
      labs(subtitle = str_glue("n = {nrow(profile_data)} observations | AUTO QC: {nrow(auto_flags)} flag(s)")) +
      theme(plot.title = element_text(color = "red"))
  }
  
  print(p)
  
  # Interactive flagging
  cat("\n")
  cat("Cast ID:", cast_id, "\n")
  
  # Show automated QC results if any
  if (nrow(auto_flags) > 0) {
    cat("⚠️  AUTOMATED QC FLAGS:\n")
    for (i in 1:nrow(auto_flags)) {
      cat("   ", auto_flags$qc_flag[i], ":", auto_flags$qc_reason[i], "\n")
    }
    cat("\n")
  }
  
  # Calculate summary stats using tidyverse
  stats <- profile_data %>% 
    summarise(
      turb_min = round(min(t, na.rm = TRUE), 3),
      turb_max = round(max(t, na.rm = TRUE), 3),
      depth_min = round(min(pres, na.rm = TRUE), 1),
      depth_max = round(max(pres, na.rm = TRUE), 1)
    )
  
  cat("Profile range: Turbidity", stats$turb_min, "to", stats$turb_max, "\n")
  cat("Depth range:", stats$depth_min, "to", stats$depth_max, "\n")
  
  response <- readline(prompt = "Flag this profile? (b=bad cast, s=surface spike, n=accept, a=accept+ignore auto QC, q=quit): ")
  
  # Clean the response - remove any whitespace and convert to lowercase
  response_clean <- str_trim(str_to_lower(response))
  
  if (response_clean == "b") {
    # Create new flag with consistent data types
    new_flag <- tibble(
      castpk = as.character(cast_id),
      flag_type = "bad_cast",
      flagged_datetime = now()
    )
    
    # Use assign to update global variable
    assign("flagged_casts", bind_rows(flagged_casts, new_flag), envir = .GlobalEnv)
    cat("✓ Cast", cast_id, "flagged as BAD CAST!\n\n")
    
  } else if (response_clean == "s") {
    # Create new flag with consistent data types
    new_flag <- tibble(
      castpk = as.character(cast_id),
      flag_type = "surface_spike", 
      flagged_datetime = now()
    )
    
    # Use assign to update global variable
    assign("flagged_casts", bind_rows(flagged_casts, new_flag), envir = .GlobalEnv)
    cat("✓ Cast", cast_id, "flagged as SURFACE SPIKE!\n\n")
    
  } else if (response_clean == "q") {
    return("quit")
  } else if (response_clean == "a") {
    # Accept and ignore auto QC flags for this cast
    cat("✓ Cast", cast_id, "accepted (auto QC flags ignored).\n\n")
  } else {
    cat("✓ Cast", cast_id, "accepted.\n\n")
  }
  
  return("continue")
}

# Main inspection function
inspect_all_profiles <- function(data, start_from = 1) {
  unique_casts <- data %>% 
    distinct(castpk) %>% 
    pull(castpk)
  
  total_casts <- length(unique_casts)
  
  cat("Starting turbidity profile inspection...\n")
  cat("Total profiles to inspect:", total_casts, "\n")
  cat("Currently flagged:", nrow(flagged_casts), "profiles\n")
  
  if (nrow(flagged_casts) > 0) {
    flag_summary <- flagged_casts %>% 
      count(flag_type, name = "count")
    
    bad_count <- flag_summary %>% 
      filter(flag_type == "bad_cast") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    spike_count <- flag_summary %>% 
      filter(flag_type == "surface_spike") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    cat("  - Bad casts:", bad_count, "\n")
    cat("  - Surface spikes:", spike_count, "\n")
  }
  cat("\n")
  
  i <- start_from
  for (i in start_from:total_casts) {
    cast_id <- unique_casts[i]
    
    cat("Progress:", i, "of", total_casts, "\n")
    
    result <- inspect_profile(data, cast_id)
    
    if (result == "quit") {
      cat("Inspection stopped at cast", i, "of", total_casts, "\n")
      break
    }
  }
  
  # Summary
  cat("\n=== INSPECTION COMPLETE ===\n")
  cat("Total profiles inspected:", i, "\n")
  cat("Flagged profiles:", nrow(flagged_casts), "\n")
  
  if (nrow(flagged_casts) > 0) {
    flag_summary <- flagged_casts %>% 
      count(flag_type, name = "count")
    
    bad_count <- flag_summary %>% 
      filter(flag_type == "bad_cast") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    spike_count <- flag_summary %>% 
      filter(flag_type == "surface_spike") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    cat("  - Bad casts:", bad_count, "\n")
    cat("  - Surface spikes:", spike_count, "\n")
    
    # Automatically save flagged casts to CSV
    save_response <- readline(prompt = "Save flagged casts to CSV? (y/n): ")
    if (str_trim(str_to_lower(save_response)) == "y") {
      filename <- readline(prompt = "Enter filename (or press Enter for default): ")
      if (str_trim(filename) == "") {
        save_flagged_csv()
      } else {
        if (!str_detect(filename, "\\.csv$")) filename <- str_c(filename, ".csv")
        save_flagged_csv(filename)
      }
    }
  }
}

# Utility functions
show_flagged <- function() {
  if (nrow(flagged_casts) == 0) {
    cat("No profiles currently flagged.\n")
  } else {
    cat("Flagged cast summary (", nrow(flagged_casts), " total):\n")
    
    # Count by flag type using tidyverse
    flag_summary <- flagged_casts %>% 
      count(flag_type, name = "count")
    
    bad_count <- flag_summary %>% 
      filter(flag_type == "bad_cast") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    spike_count <- flag_summary %>% 
      filter(flag_type == "surface_spike") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    cat("  - Bad casts:", bad_count, "\n")
    cat("  - Surface spikes:", spike_count, "\n\n")
    
    # Show detailed list
    if (bad_count > 0) {
      bad_casts <- flagged_casts %>% 
        filter(flag_type == "bad_cast") %>% 
        pull(castpk)
      cat("Bad cast IDs:", str_c(bad_casts, collapse = ", "), "\n")
    }
    if (spike_count > 0) {
      spike_casts <- flagged_casts %>% 
        filter(flag_type == "surface_spike") %>% 
        pull(castpk)
      cat("Surface spike IDs:", str_c(spike_casts, collapse = ", "), "\n")
    }
  }
}

save_flagged_csv <- function(filename = "flagged_turbidity_casts.csv") {
  if (nrow(flagged_casts) == 0) {
    cat("No flagged casts to save.\n")
    return(invisible())
  }
  
  # Prepare data for export with separate date and time columns
  export_data <- flagged_casts %>% 
    mutate(
      flagged_date = date(flagged_datetime),
      flagged_time = format(flagged_datetime, "%H:%M:%S")
    ) %>% 
    select(castpk, flag_type, flagged_date, flagged_time)
  
  # Save to CSV
  write_csv(export_data, filename)
  cat("✓ Flagged casts saved to:", filename, "\n")
  cat("Total flagged casts:", nrow(flagged_casts), "\n")
  
  # Summary by flag type
  flag_summary <- flagged_casts %>% 
    count(flag_type, name = "count")
  
  bad_count <- flag_summary %>% 
    filter(flag_type == "bad_cast") %>% 
    pull(count) %>% 
    {if(length(.) == 0) 0 else .}
  
  spike_count <- flag_summary %>% 
    filter(flag_type == "surface_spike") %>% 
    pull(count) %>% 
    {if(length(.) == 0) 0 else .}
  
  cat("  - Bad casts:", bad_count, "\n")
  cat("  - Surface spikes:", spike_count, "\n")
  
  return(export_data)
}

unflag_cast <- function(cast_id) {
  # Get current flagged_casts from global environment
  current_flags <- get("flagged_casts", envir = .GlobalEnv)
  
  if (cast_id %in% current_flags$castpk) {
    flag_info <- current_flags %>% 
      filter(castpk == cast_id) %>% 
      pull(flag_type)
    
    updated_flags <- current_flags %>% 
      filter(castpk != cast_id)
    
    assign("flagged_casts", updated_flags, envir = .GlobalEnv)
    cat("Cast", cast_id, "(", flag_info, ") unflagged.\n")
  } else {
    cat("Cast", cast_id, "was not flagged.\n")
  }
}

clear_flags <- function() {
  empty_flags <- tibble(
    castpk = character(),
    flag_type = character(),
    flagged_datetime = as_datetime(character())
  )
  assign("flagged_casts", empty_flags, envir = .GlobalEnv)
  cat("All flags cleared.\n")
}

# Function to get flagged casts by type
get_flagged_casts <- function(flag_type = c("all", "bad_cast", "surface_spike")) {
  flag_type <- match.arg(flag_type)
  
  if (flag_type == "all") {
    return(flagged_casts)
  } else {
    return(flagged_casts %>% filter(flag_type == !!flag_type))
  }
}

# Function to exclude flagged casts from your data
exclude_flagged <- function(data, exclude_types = c("bad_cast", "surface_spike")) {
  if (nrow(flagged_casts) == 0) {
    cat("No flagged casts to exclude.\n")
    return(data)
  }
  
  casts_to_exclude <- flagged_casts %>% 
    filter(flag_type %in% exclude_types) %>% 
    pull(castpk)
  
  filtered_data <- data %>% 
    filter(!castpk %in% casts_to_exclude)
  
  excluded_count <- length(unique(data$castpk)) - length(unique(filtered_data$castpk))
  cat("Excluded", excluded_count, "casts based on flags:", str_c(exclude_types, collapse = ", "), "\n")
  cat("Remaining casts:", length(unique(filtered_data$castpk)), "\n")
  
  return(filtered_data)
}

# Function to show automated QC results
show_auto_qc <- function() {
  if (nrow(auto_qc_results) == 0) {
    cat("No automated QC results available. Run run_automated_qc(t1) first.\n")
    return()
  }
  
  cat("=== AUTOMATED QC RESULTS ===\n")
  cat("Total casts with QC flags:", length(unique(auto_qc_results$castpk)), "\n\n")
  
  # Summary by flag type
  flag_summary <- auto_qc_results %>% 
    count(qc_flag, name = "count") %>%
    arrange(desc(count))
  
  cat("Flag type summary:\n")
  for (i in 1:nrow(flag_summary)) {
    cat("  ", flag_summary$qc_flag[i], ":", flag_summary$count[i], "casts\n")
  }
  
  cat("\nDetailed results:\n")
  for (flag_type in unique(auto_qc_results$qc_flag)) {
    cat("\n", toupper(flag_type), ":\n")
    flag_data <- auto_qc_results %>% 
      filter(qc_flag == flag_type) %>%
      arrange(desc(qc_value))
    
    for (i in 1:nrow(flag_data)) {
      cat("  Cast", flag_data$castpk[i], ":", flag_data$qc_reason[i], "\n")
    }
  }
}

# Function to get casts with specific auto QC flags
get_auto_qc_casts <- function(flag_type = "all") {
  if (nrow(auto_qc_results) == 0) {
    cat("No automated QC results available.\n")
    return(character(0))
  }
  
  if (flag_type == "all") {
    return(unique(auto_qc_results$castpk))
  } else {
    return(auto_qc_results %>% 
             filter(qc_flag == flag_type) %>% 
             pull(castpk))
  }
}

# Modified inspection function to prioritize flagged casts
inspect_flagged_first <- function(data, start_from = 1) {
  # Get all unique casts
  all_casts <- data %>% distinct(castpk) %>% pull(castpk)
  
  # Get casts with auto QC flags
  flagged_casts_auto <- if (nrow(auto_qc_results) > 0) {
    unique(auto_qc_results$castpk)
  } else {
    character(0)
  }
  
  # Reorder: flagged casts first, then others
  if (length(flagged_casts_auto) > 0) {
    unflagged_casts <- setdiff(all_casts, flagged_casts_auto)
    ordered_casts <- c(flagged_casts_auto, unflagged_casts)
    cat("Inspection order: AUTO QC FLAGGED casts first (", length(flagged_casts_auto), "), then clean casts (", length(unflagged_casts), ")\n\n")
  } else {
    ordered_casts <- all_casts
    cat("No automated QC flags found. Inspecting in original order.\n\n")
  }
  
  # Use the same inspection logic as before but with reordered casts
  total_casts <- length(ordered_casts)
  
  cat("Starting turbidity profile inspection...\n")
  cat("Total profiles to inspect:", total_casts, "\n")
  cat("Currently flagged:", nrow(flagged_casts), "profiles\n")
  
  if (nrow(flagged_casts) > 0) {
    flag_summary <- flagged_casts %>% 
      count(flag_type, name = "count")
    
    bad_count <- flag_summary %>% 
      filter(flag_type == "bad_cast") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    spike_count <- flag_summary %>% 
      filter(flag_type == "surface_spike") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    cat("  - Bad casts:", bad_count, "\n")
    cat("  - Surface spikes:", spike_count, "\n")
  }
  cat("\n")
  
  i <- start_from
  for (i in start_from:total_casts) {
    cast_id <- ordered_casts[i]
    
    # Show if this cast has auto QC flags
    auto_flag_status <- if (cast_id %in% flagged_casts_auto) " [AUTO QC FLAGGED]" else ""
    cat("Progress:", i, "of", total_casts, auto_flag_status, "\n")
    
    result <- inspect_profile(data, cast_id)
    
    if (result == "quit") {
      cat("Inspection stopped at cast", i, "of", total_casts, "\n")
      break
    }
  }
  
  # Summary
  cat("\n=== INSPECTION COMPLETE ===\n")
  cat("Total profiles inspected:", i, "\n")
  cat("Flagged profiles:", nrow(flagged_casts), "\n")
  
  if (nrow(flagged_casts) > 0) {
    flag_summary <- flagged_casts %>% 
      count(flag_type, name = "count")
    
    bad_count <- flag_summary %>% 
      filter(flag_type == "bad_cast") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    spike_count <- flag_summary %>% 
      filter(flag_type == "surface_spike") %>% 
      pull(count) %>% 
      {if(length(.) == 0) 0 else .}
    
    cat("  - Bad casts:", bad_count, "\n")
    cat("  - Surface spikes:", spike_count, "\n")
    
    # Automatically save flagged casts to CSV
    save_response <- readline(prompt = "Save flagged casts to CSV? (y/n): ")
    if (str_trim(str_to_lower(save_response)) == "y") {
      filename <- readline(prompt = "Enter filename (or press Enter for default): ")
      if (str_trim(filename) == "") {
        save_flagged_csv()
      } else {
        if (!str_detect(filename, "\\.csv$")) filename <- str_c(filename, ".csv")
        save_flagged_csv(filename)
      }
    }
  }
}
preview_profiles <- function(data, n_profiles = 9) {
  unique_casts <- data %>% 
    distinct(castpk) %>% 
    slice_sample(n = min(n_profiles, n())) %>% 
    pull(castpk)
  
  plot_data <- data %>% 
    filter(castpk %in% unique_casts) %>% 
    mutate(cast_label = str_c("Cast ", castpk))
  
  p <- plot_data %>% 
    ggplot(aes(x = t, y = -pres)) +
    geom_line(color = "darkblue", orientation = "y") +
    geom_point(color = "darkblue", size = 0.5, alpha = 0.7) +
    facet_wrap(~ cast_label, scales = "free", ncol = 3) +
    labs(x = "Turbidity", y = "Depth") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 8),
      strip.text = element_text(size = 10)
    )
  
  print(p)
}

# Usage instructions
cat("=== TURBIDITY PROFILE INSPECTOR WITH AUTO QC ===\n")
cat("WORKFLOW:\n")
cat("1. run_automated_qc(t1)                  - Run automated QC checks first\n")
cat("2. show_auto_qc()                        - Review automated QC results\n")
cat("3. inspect_flagged_first(t1)             - Inspect with auto-flagged casts first\n")
cat("   OR inspect_all_profiles(t1)           - Inspect in original order\n")
cat("\nOTHER FUNCTIONS:\n")
cat("4. preview_profiles(t1)                  - Quick preview of random profiles\n")
cat("5. show_flagged()                        - Show currently flagged casts\n")
cat("6. save_flagged_csv()                    - Save flagged casts to CSV file\n")
cat("7. unflag_cast(cast_id)                  - Remove flag from specific cast\n")
cat("8. clear_flags()                         - Clear all flags\n")
cat("9. get_flagged_casts('bad_cast')         - Get specific flag types\n")
cat("10. exclude_flagged(t1, c('bad_cast'))   - Remove flagged casts from data\n")
cat("11. get_auto_qc_casts('surface_spike')   - Get casts with specific auto QC flags\n")
cat("\nDuring inspection: 'b' = bad cast, 's' = surface spike, 'n' = accept, 'a' = accept+ignore auto QC, 'q' = quit\n")
cat("Auto QC flagged profiles are shown in RED with warning symbols\n")
cat("\nAUTO QC PARAMETERS (adjustable in run_automated_qc()):\n")
cat("- Surface spike detection: >5 FTU increase within 10m of surface\n")
cat("- High values: >50 FTU maximum\n")
cat("- High range: >100 FTU total range\n")
cat("- Insufficient data: <5 observations per cast\n")
cat("- Monotonic decrease: strong negative correlation with depth\n")

# Uncomment to start inspection:
# preview_profiles(t1)
# inspect_all_profiles(t1)



t <- read_csv(here("files", "8_binAvg-1749754037389.csv")) 

#Renaming columns so that they are easier to work with
t1 <- t %>% 
  select(castpk = `Cast PK`,
         ctd_num = `CTD serial number`,
         station = Station,
         time = `Measurement time`,
         pres = `Pressure (dbar)`,
         t = `Turbidity (FTU)`,
         t_flag = `Turbidity flag`) %>% 
  filter(t_flag == "null") %>% 
  mutate(date = date(time))

# preview_profiles(t1)
# inspect_all_profiles(t1)
# 1. Load your data (already done)
# 2. Run automated QC first
run_automated_qc(t1)

# 3. Review what was flagged
show_auto_qc()

# 4. Start inspection with flagged profiles first
inspect_flagged_first(t1)

# OR customize QC parameters:
run_automated_qc(t1, 
                 spike_threshold = 3,     # More sensitive spike detection
                 max_turbidity = 30,      # Lower max value threshold
                 spike_depth_limit = 5)   # Shallower spike detection
