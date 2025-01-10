# experiment_presets
# subset_df <- df_final_filtered[df_final_filtered$ce_context_experiment_presets == experiment_preset, ]
# depth_value = unique(subset_df$ce_context_depth)[1] # This is the same for the whole column since its data of ONE preset
# gl_limits_x_max <- subset_df$stop_before_tick[1]
# plot_base_name = paste("plot_", directory_files, "_behaviour_", experiment_preset, sep="")

behaviourPlot8Criteria <- function(plot_specific_f_name) {
  
  print("-- Plot Criteria Measurements --")
  #colnames(subset_df)
  
  df_criteria <<- subset_df[, c(1, 2, 3, 15, 192:203)]
  colnames(df_criteria)
  
  # Create results criteria
  preset <- c('no_preset')
  criteria_n <- c(-1)
  criteria_text <- c('criteria')
  criteria_value <- c(100)
  criteria_passed <- c(FALSE)
  
  # Make the data.frame global (using <<-)
  df_criteria_single_results <<- data.frame(preset, criteria_n, criteria_text, criteria_value, criteria_passed)
  
  crit_n <<- 0
  
  # Normal life
  behaviourPlot8CriteriaNightHome()
  behaviourPlot8CriteriaLeisureNotSickNotNight()
  behaviourPlot8CriteriaEssShop()
  behaviourPlot8CriteriaNonEssShop()
  
  # Obligations
  behaviourPlot8CriteriaNotSkipSchool()
  behaviourPlot8CriteriaNotSkipUniversity()
  
  # Covid
  behaviourPlot8CriteriaRestWhenKnowSick()
  behaviourPlot8CriteriaQuarantineChildren()
  
  # Remove the initial row
  df_criteria_single_results <<- df_criteria_single_results %>% filter(criteria_n != -1)
  
  # Print the criteria results
  df_criteria_single_results
  
  # Add the criteria results to the bigger data frame
  df_criteria_results <<- rbind(df_criteria_results, df_criteria_single_results)
  df_criteria_results <<- df_criteria_results %>% filter(criteria_n != -1)
}

behaviourPlot8CriteriaAddResult <- function(p_criteria_text, p_criteria_value, p_criteria_passed) {
  
  df_criteria_single_results <<- rbind(df_criteria_single_results, c(experiment_preset, crit_n, p_criteria_text, p_criteria_value, p_criteria_passed))
  print(df_criteria_single_results[nrow(df_criteria_single_results),])
  
  crit_n <<- crit_n + 1
}

behaviourPlot8CriteriaResultsInitialise <- function() {
  
  # Create results criteria
  preset <- c('no_preset')
  criteria_n <- c(-1)
  criteria_text <- c('criteria')
  criteria_value <- c(100)
  criteria_passed <- c(FALSE)
  
  # Make the data.frame global (using <<-)
  df_criteria_results <<- data.frame(preset, criteria_n, criteria_text, criteria_value, criteria_passed)
}

# =====================================
# Normal life
# =====================================

behaviourPlot8CriteriaNightHome <- function() {
  
  df_criteria_night_home <<- df_criteria[, c(4,5)]
  df_criteria_night_home <<- df_criteria_night_home %>% filter(criteria_night_home != -1)
  
  criteria_text   <- "Critera Night Home, mean > 99%"
  criteria_value  <- round(mean(df_criteria_night_home$criteria_night_home), 2)
  criteria_passed <- criteria_value > 99
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
  
  # Plotting
  p <- ggplot(df_criteria_night_home, aes(x = tick, y = criteria_night_home)) +
    geom_line(color = "blue", linewidth = 1.2) +  # Add a line with styling
    labs(title = "Line Plot of Criteria Night Home", x = "Tick", y = "Criteria Night Home") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  print(p)
}

behaviourPlot8CriteriaLeisureNotSickNotNight <- function() {
  
  df_criteria_leisure <<- df_criteria[, c(4,6)]
  df_criteria_leisure <<- df_criteria_leisure %>% filter(criteria_leisure_not_sick_not_night != -1)
  mean(df_criteria_leisure$criteria_leisure_not_sick_not_night)
  
  criteria_text   <- "Critera Leisure, Mean > 5 and mean < 20"
  criteria_value  <- round(mean(df_criteria_leisure$criteria_leisure_not_sick_not_night), 2)
  criteria_passed <- criteria_value > 5 && criteria_value < 20
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)

  # Plotting
  p <- ggplot(df_criteria_leisure, aes(x = tick, y = criteria_leisure_not_sick_not_night)) +
    geom_line(color = "orange", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Leisure", x = "Tick", y = "Criteria Leisure Not Sick Not Night") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  print(p)
}

behaviourPlot8CriteriaEssShop <- function() {
  
  df_criteria_ess_shop <<- df_criteria[, c(4,7)]
  df_criteria_ess_shop <<- df_criteria_ess_shop %>% filter(criteria_ess_shopping_not_sick_not_night != -1)
  mean(df_criteria_ess_shop$criteria_ess_shopping_not_sick_not_night)
  
  criteria_text   <- "Critera Ess Shopping, Mean > 5 and mean < 20"
  criteria_value  <- round(mean(df_criteria_ess_shop$criteria_ess_shopping_not_sick_not_night), 2)
  criteria_passed <- criteria_value > 5 && criteria_value < 20
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
  
  # Plotting
  p <- ggplot(df_criteria_ess_shop, aes(x = tick, y = criteria_ess_shopping_not_sick_not_night)) +
    geom_line(color = "purple", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Ess Shopping", x = "Tick", y = "Criteria Ess Shopping") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  print(p)
}

behaviourPlot8CriteriaNonEssShop <- function() {
  
  df_criteria_non_ess_shop <<- df_criteria[, c(4,8)]
  df_criteria_non_ess_shop <<- df_criteria_non_ess_shop %>% filter(criteria_non_ess_shopping_not_sick_not_night != -1)
  mean(df_criteria_non_ess_shop$criteria_non_ess_shopping_not_sick_not_night)
  
  criteria_text   <- "Critera Non-Ess Shopping, Mean > 5 and mean < 20"
  criteria_value  <- round(mean(df_criteria_non_ess_shop$criteria_non_ess_shopping_not_sick_not_night), 2)
  criteria_passed <- criteria_value > 5 && criteria_value < 20
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
  
  # Plotting
  p <- ggplot(df_criteria_non_ess_shop, aes(x = tick, y = criteria_non_ess_shopping_not_sick_not_night)) +
    geom_line(color = "brown", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Non-Ess Shopping", x = "Tick", y = "Criteria Non-Ess Shopping") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  print(p)
}

# =====================================
# Obligation
# =====================================

behaviourPlot8CriteriaNotSkipSchool <- function() {
  
  df_criteria_not_skip_school <<- df_criteria[, c(4,9)]
  df_criteria_not_skip_school <<- df_criteria_not_skip_school %>% filter(criteria_not_skip_school != -1)
  
  criteria_text   <- "Critera Not Skip School, mean > 95%"
  criteria_value  <- round(mean(df_criteria_not_skip_school$criteria_not_skip_school), 2)
  criteria_passed <- criteria_value > 95
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaNotSkipUniversity <- function() {
  
  df_criteria_not_skip_university <<- df_criteria[, c(4,10)]
  df_criteria_not_skip_university <<- df_criteria_not_skip_university %>% filter(criteria_not_skip_university != -1)
  
  criteria_text   <- "Critera Not Skip University, mean > 95%"
  criteria_value  <- round(mean(df_criteria_not_skip_university$criteria_not_skip_university), 2)
  criteria_passed <- criteria_value > 95
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

# =====================================
# COVID
# =====================================

behaviourPlot8CriteriaRestWhenKnowSick <- function() {
  
  df_criteria_rest_when_know_sick <<- df_criteria[, c(4,16)]
  df_criteria_rest_when_know_sick <<- df_criteria_rest_when_know_sick %>% filter(criteria_rest_when_know_sick != -1)
  
  criteria_text   <- "Critera Rest When Know Sick, mean > 90%"
  criteria_value  <- round(mean(df_criteria_rest_when_know_sick$criteria_rest_when_know_sick), 2)
  criteria_passed <- criteria_value > 90
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaQuarantineChildren <- function() {
  
  df_criteria_quarantine_children <<- df_criteria[, c(4,12)]
  df_criteria_quarantine_children <<- df_criteria_quarantine_children %>% filter(criteria_children_staying_in_quarantine != -1)
  
  criteria_text   <- "Critera Children in Quarantine, mean > 80%"
  criteria_value  <- round(mean(df_criteria_quarantine_children$criteria_children_staying_in_quarantine), 2)
  criteria_passed <- criteria_value > 80
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}
