# experiment_presets
# subset_df <- df_final_filtered[df_final_filtered$ce_context_experiment_presets == experiment_preset, ]
# depth_value = unique(subset_df$ce_context_depth)[1] # This is the same for the whole column since its data of ONE preset
# gl_limits_x_max <- subset_df$stop_before_tick[1]
# plot_base_name = paste("plot_", directory_files, "_behaviour_", experiment_preset, sep="")

# df_criteria_results_0 <- df_criteria_results_0 %>% rename(value_0 = crit_value_0, pass_0 = crit_pass_0)
# df_criteria_results_1 <- df_criteria_results_1 %>% rename(crit_value_1 = criteria_value, crit_pass_1 = criteria_passed)
# df_criteria_results_2 <- df_criteria_results_2 %>% rename(crit_value_2 = criteria_value, crit_pass_2 = criteria_passed)
# df_criteria_results_3 <- df_criteria_results_3 %>% rename(crit_value_3 = criteria_value, crit_pass_3 = criteria_passed)
# df_criteria_results_4 <- df_criteria_results_4 %>% rename(crit_value_4 = criteria_value, crit_pass_4 = criteria_passed)
# 
# df_1_to_5 <- df_criteria_results_0
# df_1_to_5$value_1 <- df_criteria_results_1$crit_value_1
# df_1_to_5$pass_1 <- df_criteria_results_1$crit_pass_1
# df_1_to_5$value_2 <- df_criteria_results_2$crit_value_2
# df_1_to_5$pass_2 <- df_criteria_results_2$crit_pass_2
# df_1_to_5$value_3 <- df_criteria_results_3$crit_value_3
# df_1_to_5$pass_3 <- df_criteria_results_3$crit_pass_3
# df_1_to_5$value_4 <- df_criteria_results_4$crit_value_4
# df_1_to_5$pass_4 <- df_criteria_results_4$crit_pass_4

behaviourPlot8CriteriaGetCriteriaDf <- function() {

  if (criteria_get_mean_off_runs) {
    
    # Only selecting the current experiment
    temp_df_final_preset <- df_final[df_final$ce_context_experiment_presets == experiment_preset, ]
  
    temp_df_final_preset_mean <- temp_df_final_preset %>%
      group_by(ce_context_experiment_presets, tick) %>%
      summarise(criteria_night_home = mean(criteria_night_home),
                criteria_leisure_not_sick_not_night = mean(criteria_leisure_not_sick_not_night),
                criteria_ess_shopping_not_sick_not_night = mean(criteria_ess_shopping_not_sick_not_night),
                criteria_non_ess_shopping_not_sick_not_night = mean(criteria_non_ess_shopping_not_sick_not_night),
                criteria_not_skip_work = mean(criteria_not_skip_work),
                criteria_when_possible_work_at_workplace_and_not_from_home = mean(criteria_when_possible_work_at_workplace_and_not_from_home),
                criteria_not_skip_school = mean(criteria_not_skip_school),
                criteria_not_skip_university = mean(criteria_not_skip_university),
                criteria_people_staying_in_quarantine = mean(criteria_people_staying_in_quarantine),
                criteria_children_staying_in_quarantine = mean(criteria_children_staying_in_quarantine),
                criteria_students_staying_in_quarantine = mean(criteria_students_staying_in_quarantine),
                criteria_workers_staying_in_quarantine = mean(criteria_workers_staying_in_quarantine),
                criteria_retireds_staying_in_quarantine = mean(criteria_retireds_staying_in_quarantine),
                criteria_rest_when_know_sick = mean(criteria_rest_when_know_sick),
                infected = mean(infected))
  
    temp_df_criteria <<- temp_df_final_preset_mean
    
  } else {
    temp_df_criteria <<- select(subset_df, run_number, ce_context_experiment_presets, random_seed,
                              tick, criteria_night_home:criteria_rest_when_know_sick, infected)
  }
  colnames(temp_df_criteria)
  
  return(temp_df_criteria)
}

behaviourPlot8Criteria <- function(plot_specific_f_name) {
  
  print("-- Plot Criteria Measurements --")
  #colnames(subset_df)
  
  df_criteria <<- behaviourPlot8CriteriaGetCriteriaDf()
  
  #df_criteria <<- select(subset_df, ce_context_experiment_presets, tick,
  #                       criteria_night_home:criteria_rest_when_know_sick, infected)
  colnames(df_criteria)
  
  # Create results criteria
  preset <- c('no_preset')
  criteria_n <- c(-1)
  criteria_text <- c('criteria')
  criteria_value <- c(100)
  criteria_passed <- c(FALSE)
  
  # Make the data.frame global (using <<-)
  df_criteria_single_results <<- data.frame(preset, criteria_n, criteria_text, criteria_value, criteria_passed)
  
  crit_n <<- 1
  
  # Normal life
  behaviourPlot8CriteriaNightHome()
  behaviourPlot8CriteriaRecentlyLeisure()
  behaviourPlot8CriteriaRecentlyEssShop()
  behaviourPlot8CriteriaRecentlyNonEssShop()
  
  # Obligations
  behaviourPlot8CriteriaNotSkipWork()
  behaviourPlot8CriteriaWorkAtWorkplaceNotAtHome()
  
  behaviourPlot8CriteriaNotSkipSchool()
  behaviourPlot8CriteriaNotSkipUniversity()
  
  # Covid
  behaviourPlot8CriteriaRestWhenKnowSick()
  
  behaviourPlot8CriteriaQuarantinePeople()
  behaviourPlot8CriteriaQuarantineChildren()
  behaviourPlot8CriteriaQuarantineStudents()
  behaviourPlot8CriteriaQuarantineWorkers()
  behaviourPlot8CriteriaQuarantineRetireds()
  
  behaviourPlot8CriteriaInfectionCurvePeak()
  
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
  
  df_criteria_night_home <<- select(df_criteria, tick, criteria_night_home)
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

behaviourPlot8CriteriaRecentlyLeisure <- function() {
  
  df_criteria_leisure <<- select(df_criteria, tick, criteria_recently_leisure)
  df_criteria_leisure <<- df_criteria_leisure %>% filter(criteria_recently_leisure != -1)
  mean(df_criteria_leisure$criteria_recently_leisure)
  
  criteria_text   <- "Critera Recently Leisure, mean > 99%"
  criteria_value  <- round(mean(df_criteria_leisure$criteria_recently_leisure), 2)
  criteria_passed <- criteria_value > 99
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)

  # Plotting
  p <- ggplot(df_criteria_leisure, aes(x = tick, y = criteria_recently_leisure)) +
    geom_line(color = "orange", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Leisure", x = "Tick", y = "Criteria Recently Leisure") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  print(p)
}

behaviourPlot8CriteriaRecentlyEssShop <- function() {
  
  df_criteria_ess_shop <<- select(df_criteria, tick, criteria_recently_ess_shopping) 
  df_criteria_ess_shop <<- df_criteria_ess_shop %>% filter(criteria_recently_ess_shopping != -1)
  mean(df_criteria_ess_shop$criteria_recently_ess_shopping)
  
  criteria_text   <- "Critera Recently Ess Shopping, mean > 99%"
  criteria_value  <- round(mean(df_criteria_ess_shop$criteria_recently_ess_shopping), 2)
  criteria_passed <- criteria_value > 99
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
  
  # Plotting
  p <- ggplot(df_criteria_ess_shop, aes(x = tick, y = criteria_recently_ess_shopping)) +
    geom_line(color = "purple", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Ess Shopping", x = "Tick", y = "Criteria Recently Ess Shopping") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  print(p)
}

behaviourPlot8CriteriaRecentlyNonEssShop <- function() {
  
  df_criteria_non_ess_shop <<- select(df_criteria, tick, criteria_recently_non_ess_shopping)
  df_criteria_non_ess_shop <<- df_criteria_non_ess_shop %>% filter(criteria_recently_non_ess_shopping != -1)
  mean(df_criteria_non_ess_shop$criteria_recently_non_ess_shopping)
  
  criteria_text   <- "Critera Recently Non-Ess Shopping, mean > 99%"
  criteria_value  <- round(mean(df_criteria_non_ess_shop$criteria_recently_non_ess_shopping), 2)
  criteria_passed <- criteria_value > 99
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
  
  # Plotting
  p <- ggplot(df_criteria_non_ess_shop, aes(x = tick, y = criteria_recently_non_ess_shopping)) +
    geom_line(color = "brown", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Non-Ess Shopping", x = "Tick", y = "Criteria Recently Non-Ess Shopping") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  print(p)
}

# =====================================
# Obligation
# =====================================

behaviourPlot8CriteriaNotSkipWork <- function() {
  
  df_criteria_not_skip_work <<- select(df_criteria, tick, criteria_not_skip_work) 
  df_criteria_not_skip_work <<- df_criteria_not_skip_work %>% filter(criteria_not_skip_work != -1)
  
  criteria_text   <- "Critera Not Skip Work, mean > 98%"
  criteria_value  <- round(mean(df_criteria_not_skip_work$criteria_not_skip_work), 2)
  criteria_passed <- criteria_value > 98
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaWorkAtWorkplaceNotAtHome <- function() {
  
  df_criteria_work_at_workplace <<- select(df_criteria, tick, criteria_when_possible_work_at_workplace_and_not_from_home)
  df_criteria_work_at_workplace <<- df_criteria_work_at_workplace %>% filter(criteria_when_possible_work_at_workplace_and_not_from_home != -1)
  
  criteria_text   <- "Critera Work at Workplace when possible, not at home, 85% < mean"
  criteria_value  <- round(mean(df_criteria_work_at_workplace$criteria_when_possible_work_at_workplace_and_not_from_home), 2)
  criteria_passed <- 85 < criteria_value
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaNotSkipSchool <- function() {
  
  df_criteria_not_skip_school <<- select(df_criteria, tick, criteria_not_skip_school) 
  df_criteria_not_skip_school <<- df_criteria_not_skip_school %>% filter(criteria_not_skip_school != -1)
  
  criteria_text   <- "Critera Not Skip School, mean > 95%"
  criteria_value  <- round(mean(df_criteria_not_skip_school$criteria_not_skip_school), 2)
  criteria_passed <- criteria_value > 95
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaNotSkipUniversity <- function() {
  
  df_criteria_not_skip_university <<- select(df_criteria, tick, criteria_not_skip_university)  
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
  
  df_criteria_rest_when_know_sick <<- select(df_criteria, tick, criteria_rest_when_know_sick)  
  df_criteria_rest_when_know_sick <<- df_criteria_rest_when_know_sick %>% filter(criteria_rest_when_know_sick != -1)
  
  criteria_text   <- "Critera Rest When Know Sick, mean > 90%"
  criteria_value  <- round(mean(df_criteria_rest_when_know_sick$criteria_rest_when_know_sick), 2)
  criteria_passed <- criteria_value > 90
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaQuarantinePeople <- function() {
  
  df_criteria_quarantine_people <<- select(df_criteria, tick, criteria_people_staying_in_quarantine)  
  df_criteria_quarantine_people <<- df_criteria_quarantine_people %>% filter(criteria_people_staying_in_quarantine != -1)
  
  if (!grepl("lockdown", experiment_preset, fixed = TRUE)) { # No Lockdown
    criteria_text   <- "Critera People in Quarantine, 90% < mean < 100%"
    criteria_value  <- round(mean(df_criteria_quarantine_people$criteria_people_staying_in_quarantine), 2)
    criteria_passed <- 90 < criteria_value && criteria_value < 100
  } else {
    criteria_text   <- "Critera People in Quarantine, 80% < mean < 90%"
    criteria_value  <- round(mean(df_criteria_quarantine_people$criteria_people_staying_in_quarantine), 2)
    criteria_passed <- 80 < criteria_value && criteria_value < 98
  }
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaQuarantineChildren <- function() {
  
  df_criteria_quarantine_children <<- select(df_criteria, tick, criteria_children_staying_in_quarantine)  
  df_criteria_quarantine_children <<- df_criteria_quarantine_children %>% filter(criteria_children_staying_in_quarantine != -1)
  
  if (!grepl("lockdown", experiment_preset, fixed = TRUE)) { # No Lockdown
    criteria_text   <- "Critera Children in Quarantine, 90% < mean < 100%"
    criteria_value  <- round(mean(df_criteria_quarantine_children$criteria_children_staying_in_quarantine), 2)
    criteria_passed <- 90 < criteria_value && criteria_value < 100
  } else {
    criteria_text   <- "Critera Children in Quarantine, 80% < mean < 98%"
    criteria_value  <- round(mean(df_criteria_quarantine_children$criteria_children_staying_in_quarantine), 2)
    criteria_passed <- 80 < criteria_value && criteria_value < 98
  }
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaQuarantineStudents <- function() {
  
  df_criteria_quarantine_students <<- select(df_criteria, tick, criteria_students_staying_in_quarantine)  
  df_criteria_quarantine_students <<- df_criteria_quarantine_students %>% filter(criteria_students_staying_in_quarantine != -1)
  
  if (!grepl("lockdown", experiment_preset, fixed = TRUE)) { # No Lockdown
    criteria_text   <- "Critera Students in Quarantine, 90% < mean < 100%"
    criteria_value  <- round(mean(df_criteria_quarantine_students$criteria_students_staying_in_quarantine), 2)
    criteria_passed <- 90 < criteria_value && criteria_value < 100
  } else {
    criteria_text   <- "Critera Students in Quarantine, 80% < mean < 98%"
    criteria_value  <- round(mean(df_criteria_quarantine_students$criteria_students_staying_in_quarantine), 2)
    criteria_passed <- 80 < criteria_value && criteria_value < 98
  }
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaQuarantineWorkers <- function() {
  
  df_criteria_quarantine_workers <<- select(df_criteria, tick, criteria_workers_staying_in_quarantine)  
  df_criteria_quarantine_workers <<- df_criteria_quarantine_workers %>% filter(criteria_workers_staying_in_quarantine != -1)
  
  if (!grepl("lockdown", experiment_preset, fixed = TRUE)) { # No Lockdown
    criteria_text   <- "Critera Workers in Quarantine, 90% < mean < 100%"
    criteria_value  <- round(mean(df_criteria_quarantine_workers$criteria_workers_staying_in_quarantine), 2)
    criteria_passed <- 90 < criteria_value && criteria_value < 100
  } else {
    criteria_text   <- "Critera Workers in Quarantine, 80% < mean < 98%"
    criteria_value  <- round(mean(df_criteria_quarantine_workers$criteria_workers_staying_in_quarantine), 2)
    criteria_passed <- 80 < criteria_value && criteria_value < 98
  }
    
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaQuarantineRetireds <- function() {
  
  df_criteria_quarantine_retireds <<- select(df_criteria, tick, criteria_retireds_staying_in_quarantine)  
  df_criteria_quarantine_retireds <<- df_criteria_quarantine_retireds %>% filter(criteria_retireds_staying_in_quarantine != -1)
  
  if (!grepl("lockdown", experiment_preset, fixed = TRUE)) { # No Lockdown
    criteria_text   <- "Critera Retireds in Quarantine, 90% < mean < 100%"
    criteria_value  <- round(mean(df_criteria_quarantine_retireds$criteria_retireds_staying_in_quarantine), 2)
    criteria_passed <- 90 < criteria_value && criteria_value < 100
  } else {
    criteria_text   <- "Critera Retireds in Quarantine, 80% < mean < 98%"
    criteria_value  <- round(mean(df_criteria_quarantine_retireds$criteria_retireds_staying_in_quarantine), 2)
    criteria_passed <- 80 < criteria_value && criteria_value < 98
  }
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}

behaviourPlot8CriteriaInfectionCurvePeak <- function() {
  
  df_criteria_infection_curve_peak <<- select(df_criteria, tick, infected)  
  infection_curve_peak_tick_row <- df_criteria_infection_curve_peak[which.max(df_criteria_infection_curve_peak$infected),]
  
  if (!grepl("lockdown", experiment_preset, fixed = TRUE)) { # No Lockdown
    criteria_text   <- "Criteria Infection Peak Tick, 75 < value < 150"
    criteria_value  <- infection_curve_peak_tick_row$tick
    criteria_passed <- 75 < criteria_value && criteria_value < 150
  }
  else { # Lockdown
    criteria_text   <- "Criteria Infection Peak Tick, 250 < value < 400"
    criteria_value  <- infection_curve_peak_tick_row$tick
    criteria_passed <- 250 < criteria_value && criteria_value < 400
  }
  
  behaviourPlot8CriteriaAddResult(criteria_text, criteria_value, criteria_passed)
}