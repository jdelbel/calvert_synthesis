
# Turbidity Profile Inspector - Tidyverse Version
# Interactive tool to visually inspect turbidity profiles and flag bad ones

# Required libraries
library(tidyverse)
library(lubridate)
library(here)

# Initialize flagged casts tibble
flagged_casts <- tibble(
  castpk = character(),
  flag_type = character(),
  flagged_datetime = as_datetime(character())
)

# Function to plot and inspect individual profiles
inspect_profile <- function(data, cast_id) {
  # Filter data for specific cast
  profile_data <- data %>% 
    filter(castpk == cast_id)
  
  # Create the plot
  p <- profile_data %>% 
    ggplot(aes(x = t, y = -pres)) +
    geom_line(color = "darkblue", linewidth = 1, orientation = "y") +
    geom_point(color = "darkblue", size = 0.8, alpha = 0.7) +
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
  
  print(p)
  
  # Interactive flagging
  cat("\n")
  cat("Cast ID:", cast_id, "\n")
  
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
  
  response <- readline(prompt = "Flag this profile? (b=bad cast, s=surface spike, n=accept, q=quit): ")
  
  if (str_to_lower(response) == "b") {
    new_flag <- tibble(
      castpk = cast_id,
      flag_type = "bad_cast",
      flagged_datetime = now()
    )
    flagged_casts <<- bind_rows(flagged_casts, new_flag)
    cat("Cast", cast_id, "flagged as BAD CAST!\n\n")
  } else if (str_to_lower(response) == "s") {
    new_flag <- tibble(
      castpk = cast_id,
      flag_type = "surface_spike",
      flagged_datetime = now()
    )
    flagged_casts <<- bind_rows(flagged_casts, new_flag)
    cat("Cast", cast_id, "flagged as SURFACE SPIKE!\n\n")
  } else if (str_to_lower(response) == "q") {
    return("quit")
  } else {
    cat("Cast", cast_id, "accepted.\n\n")
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
      ifelse(length(.) == 0, 0, .)
    
    spike_count <- flag_summary %>% 
      filter(flag_type == "surface_spike") %>% 
      pull(count) %>% 
      ifelse(length(.) == 0, 0, .)
    
    cat("  - Bad casts:", bad_count, "\n")
    cat("  - Surface spikes:", spike_count, "\n")
  }
  cat("\n")
  
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
      ifelse(length(.) == 0, 0, .)
    
    spike_count <- flag_summary %>% 
      filter(flag_type == "surface_spike") %>% 
      pull(count) %>% 
      ifelse(length(.) == 0, 0, .)
    
    cat("  - Bad casts:", bad_count, "\n")
    cat("  - Surface spikes:", spike_count, "\n")
    
    # Automatically save flagged casts to CSV
    save_response <- readline(prompt = "Save flagged casts to CSV? (y/n): ")
    if (str_to_lower(save_response) == "y") {
      filename <- readline(prompt = "Enter filename (or press Enter for default): ")
      if (filename == "") {
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
      ifelse(length(.) == 0, 0, .)
    
    spike_count <- flag_summary %>% 
      filter(flag_type == "surface_spike") %>% 
      pull(count) %>% 
      ifelse(length(.) == 0, 0, .)
    
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
  cat("Flagged casts saved to:", filename, "\n")
  cat("Total flagged casts:", nrow(flagged_casts), "\n")
  
  # Summary by flag type
  flag_summary <- flagged_casts %>% 
    count(flag_type, name = "count")
  
  bad_count <- flag_summary %>% 
    filter(flag_type == "bad_cast") %>% 
    pull(count) %>% 
    ifelse(length(.) == 0, 0, .)
  
  spike_count <- flag_summary %>% 
    filter(flag_type == "surface_spike") %>% 
    pull(count) %>% 
    ifelse(length(.) == 0, 0, .)
  
  cat("  - Bad casts:", bad_count, "\n")
  cat("  - Surface spikes:", spike_count, "\n")
  
  return(export_data)
}

unflag_cast <- function(cast_id) {
  if (cast_id %in% flagged_casts$castpk) {
    flag_info <- flagged_casts %>% 
      filter(castpk == cast_id) %>% 
      pull(flag_type)
    
    flagged_casts <<- flagged_casts %>% 
      filter(castpk != cast_id)
    
    cat("Cast", cast_id, "(", flag_info, ") unflagged.\n")
  } else {
    cat("Cast", cast_id, "was not flagged.\n")
  }
}

clear_flags <- function() {
  flagged_casts <<- tibble(
    castpk = character(),
    flag_type = character(),
    flagged_datetime = as_datetime(character())
  )
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

# Quick preview function to see multiple profiles at once
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
cat("=== TURBIDITY PROFILE INSPECTOR - TIDYVERSE VERSION ===\n")
cat("Usage:\n")
cat("1. inspect_all_profiles(t1)              - Start inspection from beginning\n")
cat("2. inspect_all_profiles(t1, start_from = 50) - Resume from cast 50\n")
cat("3. preview_profiles(t1)                  - Quick preview of random profiles\n")
cat("4. show_flagged()                        - Show currently flagged casts\n")
cat("5. save_flagged_csv()                    - Save flagged casts to CSV file\n")
cat("6. unflag_cast(cast_id)                  - Remove flag from specific cast\n")
cat("7. clear_flags()                         - Clear all flags\n")
cat("8. get_flagged_casts('bad_cast')         - Get specific flag types\n")
cat("9. exclude_flagged(t1, c('bad_cast'))    - Remove flagged casts from data\n")
cat("\nDuring inspection: 'b' = bad cast, 's' = surface spike, 'n' = accept, 'q' = quit\n")
cat("CSV output includes: castpk, flag_type, flagged_date, flagged_time\n")

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
inspect_all_profiles(t1)
