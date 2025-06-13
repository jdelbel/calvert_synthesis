
library(ggplot2)
library(readr)
library(dplyr)
library(here)
library(lubridate)

# Turbidity Profile Inspector
# Interactive tool to visually inspect turbidity profiles and flag bad ones

# Required libraries
library(ggplot2)

# Initialize flagged casts vector
flagged_casts <- c()

# Function to plot and inspect individual profiles
inspect_profile <- function(data, cast_id) {
  # Filter data for specific cast
  profile_data <- data[data$castpk == cast_id, ]
  
  # Create the plot
  p <- ggplot(profile_data, aes(x = t, y = -pres)) +
    geom_line(color = "darkblue", size = 1, orientation = "y") +
    geom_point(color = "darkblue", size = 0.8, alpha = 0.7) +
    labs(
      title = paste("Cast ID:", cast_id),
      x = "Turbidity",
      y = "Depth (negative pressure)",
      subtitle = paste("n =", nrow(profile_data), "observations")
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
  cat("Profile range: Turbidity", round(min(profile_data$t, na.rm = TRUE), 3), 
      "to", round(max(profile_data$t, na.rm = TRUE), 3), "\n")
  cat("Depth range:", round(min(profile_data$pres, na.rm = TRUE), 1), 
      "to", round(max(profile_data$pres, na.rm = TRUE), 1), "\n")
  
  response <- readline(prompt = "Flag this profile? (y/n/q to quit): ")
  
  if (tolower(response) == "y") {
    flagged_casts <<- c(flagged_casts, cast_id)
    cat("Cast", cast_id, "flagged!\n\n")
  } else if (tolower(response) == "q") {
    return("quit")
  } else {
    cat("Cast", cast_id, "accepted.\n\n")
  }
  
  return("continue")
}

# Main inspection function
inspect_all_profiles <- function(data, start_from = 1) {
  unique_casts <- unique(data$castpk)
  total_casts <- length(unique_casts)
  
  cat("Starting turbidity profile inspection...\n")
  cat("Total profiles to inspect:", total_casts, "\n")
  cat("Currently flagged:", length(flagged_casts), "profiles\n\n")
  
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
  cat("Flagged profiles:", length(flagged_casts), "\n")
  
  if (length(flagged_casts) > 0) {
    cat("Flagged cast IDs:", paste(flagged_casts, collapse = ", "), "\n")
    
    # Automatically save flagged casts to CSV
    save_response <- readline(prompt = "Save flagged casts to CSV? (y/n): ")
    if (tolower(save_response) == "y") {
      filename <- readline(prompt = "Enter filename (or press Enter for default): ")
      if (filename == "") {
        save_flagged_csv()
      } else {
        if (!grepl("\\.csv$", filename)) filename <- paste0(filename, ".csv")
        save_flagged_csv(filename)
      }
    }
  }
}

# Utility functions
show_flagged <- function() {
  if (length(flagged_casts) == 0) {
    cat("No profiles currently flagged.\n")
  } else {
    cat("Flagged cast IDs (", length(flagged_casts), "):\n")
    cat(paste(flagged_casts, collapse = ", "), "\n")
  }
}

save_flagged_csv <- function(filename = "flagged_turbidity_casts.csv") {
  if (length(flagged_casts) == 0) {
    cat("No flagged casts to save.\n")
    return()
  }
  
  # Create dataframe with flagged casts and timestamp
  flagged_df <- data.frame(
    castpk = flagged_casts,
    flagged_date = Sys.Date(),
    flagged_time = format(Sys.time(), "%H:%M:%S"),
    reason = "visual_inspection",
    stringsAsFactors = FALSE
  )
  
  # Save to CSV
  write.csv(flagged_df, filename, row.names = FALSE)
  cat("Flagged casts saved to:", filename, "\n")
  cat("Total flagged casts:", length(flagged_casts), "\n")
  
  return(flagged_df)
}

unflag_cast <- function(cast_id) {
  if (cast_id %in% flagged_casts) {
    flagged_casts <<- flagged_casts[flagged_casts != cast_id]
    cat("Cast", cast_id, "unflagged.\n")
  } else {
    cat("Cast", cast_id, "was not flagged.\n")
  }
}

clear_flags <- function() {
  flagged_casts <<- c()
  cat("All flags cleared.\n")
}

# Quick preview function to see multiple profiles at once
preview_profiles <- function(data, n_profiles = 9) {
  unique_casts <- unique(data$castpk)
  selected_casts <- sample(unique_casts, min(n_profiles, length(unique_casts)))
  
  plot_list <- list()
  for (i in 1:length(selected_casts)) {
    profile_data <- data[data$castpk == selected_casts[i], ]
    plot_list[[i]] <- ggplot(profile_data, aes(x = t, y = -pres)) +
      geom_line(color = "darkblue") +
      geom_point(color = "darkblue", size = 0.5, alpha = 0.7) +
      labs(title = paste("Cast", selected_casts[i]), x = "Turbidity", y = "Depth") +
      theme_bw() +
      theme(axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))
  }
  
  # Arrange plots (requires gridExtra)
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::grid.arrange(grobs = plot_list, ncol = 3)
  } else {
    cat("Install gridExtra package for multi-panel preview: install.packages('gridExtra')\n")
    for (p in plot_list) print(p)
  }
}

# Usage instructions
cat("=== TURBIDITY PROFILE INSPECTOR ===\n")
cat("Usage:\n")
cat("1. inspect_all_profiles(t1)           - Start inspection from beginning\n")
cat("2. inspect_all_profiles(t1, start_from = 50) - Resume from cast 50\n")
cat("3. preview_profiles(t1)               - Quick preview of random profiles\n")
cat("4. show_flagged()                     - Show currently flagged casts\n")
cat("5. save_flagged_csv()                 - Save flagged casts to CSV file\n")
cat("6. unflag_cast(cast_id)               - Remove flag from specific cast\n")
cat("7. clear_flags()                      - Clear all flags\n")
cat("\nDuring inspection: 'y' = flag, 'n' = accept, 'q' = quit\n")
cat("CSV output includes: castpk, flagged_date, flagged_time, reason\n")

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
