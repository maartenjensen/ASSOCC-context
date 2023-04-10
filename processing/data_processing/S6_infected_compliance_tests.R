#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6InfectedComplianceTests <- function(df_scenario6, p_independent_variable, output_dir, one_plot) {
  
  name = "s6_infected_compliance_tests"
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  df_data <- df_scenario6 %>% 
    group_by(tick, !!p_independent_variable) %>% 
    summarise(infected = mean(infected, na.rm = TRUE),
              believe_infected = mean(believe_infected, na.rm = TRUE),
              tests_performed = mean(tests_performed, na.rm = TRUE),
              ratio_quarantiners_complying = mean(ratio_quarantiners_currently_complying_to_quarantine, na.rm = TRUE))
  
  # Creating data for tests performed per day
  df_tests <- df_data %>% select(tick, !!p_independent_variable, tests_performed)
  df_tests_per_tick <- df_tests %>%
    group_by(!!p_independent_variable) %>%
    arrange(tick) %>% 
    mutate(tests_per_tick = tests_performed - lag(tests_performed, default = first(tests_performed)))
  
  df_tests_per_tick$day <- dmfConvertTicksToDay(df_tests_per_tick$tick)

  df_tests_per_day <- df_tests_per_tick %>% 
    group_by(day, !!p_independent_variable) %>% 
    summarise(tests_per_day = sum(tests_per_tick, na.rm = TRUE))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_data, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_tests_per_day, file=paste(output_dir, "/plot_data_", name, "_tests_per_day.csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  # Infected
  plots_data_infected <- gather(df_data, variable, measurement, infected)
  dmfPdfOpen(output_dir, "s6_infected")
  print(plot_ggplot_tick(plots_data_infected, p_independent_variable, "Agents infected depending on the app usage ratio", "Number of infected"))
  dmfPdfClose()
  
  # Epistemic infected
  plots_data_epistemic_infected <- gather(df_data, variable, measurement, believe_infected)
  dmfPdfOpen(output_dir, "s6_infected_epistemic")
  print(plot_ggplot_tick(plots_data_epistemic_infected, p_independent_variable, "Agents believing being infected depending on the app usage ratio",
                         "Number of agents believing they are infected"))
  dmfPdfClose()
  
  # Ratio quarantine 
  plots_data_ratio_quarantine <- gather(df_data, variable, measurement, ratio_quarantiners_complying)
  dmfPdfOpen(output_dir, "s6_quarantiners_complying")
  print(plot_ggplot_tick(plots_data_ratio_quarantine, p_independent_variable, "Ratio agents in quarantine depending on the app usage ratio",
                         "Ratio of people complying to quarantine"))
  dmfPdfClose()
  
  # Ratio quarantine smoothed
  dmfPdfOpen(output_dir, "s6_quarantiners_complying_smooth")
  print(plot_ggplot_tick_smooth(plots_data_ratio_quarantine, p_independent_variable, "Ratio agents in quarantine depending on the app usage ratio (Smoothed)",
                                "Ratio of people complying to quarantine"))
  dmfPdfClose()
  
  # Total tests performed
  plots_data_tests <- gather(df_data, variable, measurement, tests_performed)
  dmfPdfOpen(output_dir, "s6_cumulative_tests")
  print(plot_ggplot_tick(plots_data_tests, p_independent_variable, "Cummulative number of tests depending on the app usage ratio", "Cummulative number of tests"))
  dmfPdfClose()
  
  # Number of tests per day
  plots_data_tests_per_day <- gather(df_tests_per_day, variable, measurement, tests_per_day)
  dmfPdfOpen(output_dir, "s6_tests_per_day")
  print(plot_ggplot_day(plots_data_tests_per_day, p_independent_variable, "Number of tests depending on the app usage ratio per day",
                        "Number of tests per day"))
  dmfPdfClose()
  
  # Number of tests per day smoothed
  dmfPdfOpen(output_dir, "s6_tests_per_day_smooth")
  print(plot_ggplot_day_smooth(plots_data_tests_per_day, p_independent_variable, "Number of tests depending on the app usage ratio per day (Smoothed)",
                               "Number of tests per day"))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================
plot_ggplot_tick <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement)) +
    geom_line(aes(col=as.factor(!!p_independent_variable))) +
    scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Ticks", y=p_y_lab) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_tick_smooth <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement)) +
    stat_smooth(aes(col=as.factor(!!p_independent_variable)), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Ticks", y=p_y_lab) +
    gl_plot_guides + gl_plot_theme 
}

plot_ggplot_day <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_line(aes(col=as.factor(!!p_independent_variable))) +
    scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_day_smooth <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_smooth(aes(col=as.factor(!!p_independent_variable)), span=0.1, se=FALSE) +
    scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    gl_plot_guides + gl_plot_theme
}