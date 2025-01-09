behaviourPlot8Criteria <- function(plot_specific_f_name) {
  
  print("-- Plot Criteria Measurements --")
  #colnames(subset_df)
  
  df_criteria <- subset_df[, c(1, 2, 3, 15, 192:203)]
  colnames(df_criteria)
  
  behaviourPlot8CriteriaNightHome()
  behaviourPlot8CriteriaLeisureNotSickNotNight()
  behaviourPlot8CriteriaEssShop()
  behaviourPlot8CriteriaNonEssShop()
  
  behaviourPlot8CriteriaCheckAll()
}

behaviourPlot8CriteriaCheckAll <- function() {
  
  print(paste(":: Critera Night Home, always > 99% Rest at home at night: ", all(df_criteria_night_home$criteria_night_home > 99)))
  print(paste(":: Critera Leisure Mean: ", round(mean(df_criteria_leisure$criteria_leisure_not_sick_not_night), 2) ))
  print(paste(":: Critera Ess Shopping Mean: ", round(mean(df_criteria_ess_shop$criteria_ess_shopping_not_sick_not_night), 2) ))
  print(paste(":: Critera Non-Ess Shopping Mean: ", round(mean(df_criteria_non_ess_shop$criteria_non_ess_shopping_not_sick_not_night), 2) ))
}

behaviourPlot8CriteriaNotSkipSchool <- function() {
  
  df_criteria_not_skip_school <- df_criteria[, c(4,9)]
  df_criteria_not_skip_school <- df_criteria_not_skip_school %>% filter(criteria_not_skip_school != -1)
  df_criteria_not_skip_school <- df_criteria_not_skip_school %>% filter(criteria_not_skip_school != "<RuntimePrimitiveException>")
  
  print(paste(":: Critera Not Skip School, always > 95%: ", all(df_criteria_not_skip_school$criteria_not_skip_school > 95)))
}


behaviourPlot8CriteriaNightHome <- function() {
  
  df_criteria_night_home <- df_criteria[, c(4,5)]
  df_criteria_night_home <- df_criteria_night_home %>% filter(criteria_night_home != -1)
  
  print(paste(":: Critera Night Home, always > 99% Rest at home at night: ", all(df_criteria_night_home$criteria_night_home > 99)))
  
  p <- ggplot(df_criteria_night_home, aes(x = tick, y = criteria_night_home)) +
    geom_line(color = "blue", linewidth = 1.2) +  # Add a line with styling
    labs(title = "Line Plot of Criteria Night Home", x = "Tick", y = "Criteria Night Home") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  # Print the plot
  print(p)
}

behaviourPlot8CriteriaLeisureNotSickNotNight <- function() {
  
  df_criteria_leisure <- df_criteria[, c(4,6)]
  df_criteria_leisure <- df_criteria_leisure %>% filter(criteria_leisure_not_sick_not_night != -1)
  mean(df_criteria_leisure$criteria_leisure_not_sick_not_night)
  
  print(paste(":: Critera Leisure Mean: ", round(mean(df_criteria_leisure$criteria_leisure_not_sick_not_night), 2) ))
  
  p <- ggplot(df_criteria_leisure, aes(x = tick, y = criteria_leisure_not_sick_not_night)) +
    geom_line(color = "orange", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Leisure", x = "Tick", y = "Criteria Leisure Not Sick Not Night") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  # Print the plot
  print(p)
}

behaviourPlot8CriteriaEssShop <- function() {
  
  df_criteria_ess_shop <- df_criteria[, c(4,7)]
  df_criteria_ess_shop <- df_criteria_ess_shop %>% filter(criteria_ess_shopping_not_sick_not_night != -1)
  mean(df_criteria_ess_shop$criteria_ess_shopping_not_sick_not_night)
  
  print(paste(":: Critera Ess Shopping Mean: ", round(mean(df_criteria_ess_shop$criteria_ess_shopping_not_sick_not_night), 2) ))
  
  p <- ggplot(df_criteria_ess_shop, aes(x = tick, y = criteria_ess_shopping_not_sick_not_night)) +
    geom_line(color = "purple", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Ess Shopping", x = "Tick", y = "Criteria Ess Shopping") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  # Print the plot
  print(p)
}

behaviourPlot8CriteriaNonEssShop <- function() {
  
  df_criteria_non_ess_shop <- df_criteria[, c(4,8)]
  df_criteria_non_ess_shop <- df_criteria_non_ess_shop %>% filter(criteria_non_ess_shopping_not_sick_not_night != -1)
  mean(df_criteria_non_ess_shop$criteria_non_ess_shopping_not_sick_not_night)
  
  print(paste(":: Critera Non-Ess Shopping Mean: ", round(mean(df_criteria_non_ess_shop$criteria_non_ess_shopping_not_sick_not_night), 2) ))
  
  p <- ggplot(df_criteria_non_ess_shop, aes(x = tick, y = criteria_non_ess_shopping_not_sick_not_night)) +
    geom_line(color = "brown", linewidth = 1.2) +
    labs(title = "Line Plot of Criteria Non-Ess Shopping", x = "Tick", y = "Criteria Non-Ess Shopping") +
    theme_minimal(base_size = 14) + # Use a clean theme with adjusted text size
    coord_equal(xlim = c(0, gl_limits_x_max), ylim = c(0, 100))
  
  # Print the plot
  print(p)
}