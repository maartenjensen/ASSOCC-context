#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6InfectedComplianceTests <- function(df_scenario6, output_dir, one_plot) {
  
  name = "s6_infected_compliance_tests"
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))

  df_data <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(infected = mean(infected, na.rm = TRUE),
              believe_infected = mean(believe_infected, na.rm = TRUE),
              tests_performed = mean(tests_performed, na.rm = TRUE),
              ratio_quarantiners_complying = mean(ratio_quarantiners_currently_complying_to_quarantine, na.rm = TRUE),
              infected_this_tick = mean(infected_this_tick),
              dead_people = mean(dead_people))
  
  # Creating data for tests performed per day
  df_tests <- df_data %>% select(tick, ratio_of_app_users, tests_performed)
  df_deaths <- df_data %>% select(tick, ratio_of_app_users, dead_people)
  
  v_testing_days <- gl_mean_end_quaran_day - 7
  print(paste("Started the track and tracing app at day:", v_testing_days))
  v_testing_days <- 375 - v_testing_days
  print(paste("Number of days testing:", v_testing_days))
  
  df_tests_max <- df_data %>% group_by(ratio_of_app_users) %>%
    summarise(tests_performed = max(tests_performed, na.rm = TRUE), tests_performed_day = max(tests_performed, na.rm = TRUE)/v_testing_days,
              tests_performed_day_ratio_divide_1126 = (max(tests_performed, na.rm = TRUE)/v_testing_days)/1126  )
  print(df_tests_max)

  df_tests_per_tick <- df_tests %>%
    group_by(ratio_of_app_users) %>%
    arrange(tick) %>% 
    mutate(tests_per_tick = tests_performed - lag(tests_performed, default = first(tests_performed)))
  
  df_deaths_per_tick <- df_deaths %>%
    group_by(ratio_of_app_users) %>%
    arrange(tick) %>% 
    mutate(deaths_per_tick = dead_people - lag(dead_people, default = first(dead_people)))
  
  df_tests_per_tick$day <- dmfConvertTicksToDay(df_tests_per_tick$tick)
  df_deaths_per_tick$day <- dmfConvertTicksToDay(df_deaths_per_tick$tick)
  
  df_tests_per_day <- df_tests_per_tick %>% 
    group_by(day, ratio_of_app_users) %>% 
    summarise(tests_per_day = sum(tests_per_tick, na.rm = TRUE))
  
  df_deaths_per_day <- df_deaths_per_tick %>% 
    group_by(day, ratio_of_app_users) %>% 
    summarise(deaths_per_day = sum(deaths_per_tick, na.rm = TRUE))
  
  # Creating data for infected performed per day
  df_infected_per_tick <- df_data %>% select(tick, ratio_of_app_users, infected_this_tick)
  
  df_infected_per_tick$day <- dmfConvertTicksToDay(df_infected_per_tick$tick)
  df_infected_per_tick$week <- dmfConvertTicksToWeek(df_infected_per_tick$tick)
  
  df_infected_per_day <- df_infected_per_tick %>% 
    group_by(day, ratio_of_app_users) %>% 
    summarise(infected_this_day = sum(infected_this_tick, na.rm = TRUE),
              week = mean(week))
  
  # Divide by week
  df_infected_per_week_average <- df_infected_per_day %>%
    group_by(week, ratio_of_app_users) %>% 
    summarise(infected_this_week = mean(infected_this_day, na.rm = TRUE),
              day = mean(day))
  
  
  print(paste(name, " print data", sep=""))
  print(df_data %>% group_by(ratio_of_app_users) %>% summarise(infected = max(infected)))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_data, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_tests_per_day, file=paste(output_dir, "/plot_data_", name, "_tests_per_day.csv", sep=""))
  write.csv(df_deaths_per_day, file=paste(output_dir, "/plot_data_", name, "_deaths_per_day.csv", sep=""))
  write.csv(df_infected_per_day, file=paste(output_dir, "/plot_data_", name, "_infected_per_day.csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  # Infected
  plots_data_infected <- gather(df_data, variable, measurement, infected)
  dmfPdfOpen(output_dir, "s6_infected")
  print(plot_ggplot_tick(plots_data_infected, "Agents infected depending on the app usage ratio",
                         "Number of infected", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Number of infected per day
  plots_data_infected_per_day <- gather(df_infected_per_day, variable, measurement, infected_this_day)
  dmfPdfOpen(output_dir, "s6_infected_newly_per_day")
  print(plot_ggplot_day(plots_data_infected_per_day, "Number of newly infected depending on the app usage ratio per day",
                        "Newly infected per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Number of infected per day smoothed
  dmfPdfOpen(output_dir, "s6_infected_newly_per_day_smooth")
  print(plot_ggplot_day_smooth(plots_data_infected_per_day, "Number of newly infected depending on the app usage ratio per day (Smoothed)",
                               "Newly infected per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Number of infected per week averaged
  seg_infected_per_week_average <- gather(df_infected_per_week_average, variable, measurement, infected_this_week)
  dmfPdfOpen(output_dir, "s6_infected_newly_per_week_average")
  print(plot_ggplot_day_smooth(seg_infected_per_week_average, "Number of newly infected depending on the app usage ratio per week",
                               "Newly infected per week", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()

  # Epistemic infected
  plots_data_epistemic_infected <- gather(df_data, variable, measurement, believe_infected)
  dmfPdfOpen(output_dir, "s6_infected_epistemic")
  print(plot_ggplot_tick(plots_data_epistemic_infected, "Agents believing being infected depending on the app usage ratio",
                         "Number of agents believing they are infected", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Total deaths
  plots_data_deaths <- gather(df_data, variable, measurement, dead_people)
  dmfPdfOpen(output_dir, "s6_deaths_cumulative")
  print(plot_ggplot_tick(plots_data_deaths, "Cummulative deaths depending on the app usage ratio", "Cummulative number of deaths",
                               gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Number of deaths per day
  plots_data_deaths_per_day <- gather(df_deaths_per_day, variable, measurement, deaths_per_day)
  dmfPdfOpen(output_dir, "s6_deaths_per_day")
  print(plot_ggplot_day(plots_data_deaths_per_day, "Deaths depending on the app usage ratio per day",
                        "Number of deaths per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Number of deaths per day smoothed
  dmfPdfOpen(output_dir, "s6_deaths_per_day_smooth")
  print(plot_ggplot_day_smooth(plots_data_deaths_per_day, "Deaths depending on the app usage ratio per day (Smooth)",
                               "Number of deaths per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Total tests performed
  plots_data_tests <- gather(df_data, variable, measurement, tests_performed)
  dmfPdfOpen(output_dir, "s6_tests_cumulative")
  print(plot_ggplot_tick_tests(plots_data_tests, "Cummulative number of tests depending on the app usage ratio", "Cummulative number of tests",
                         gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Total tests performed 2
  plots_data_tests <- gather(df_data, variable, measurement, tests_performed)
  dmfPdfOpen(output_dir, "s6_tests_cumulative_with_random_testing")
  print(plot_ggplot_tick_tests_2(plots_data_tests, "Cummulative number of tests depending on the app usage ratio", "Cummulative number of tests",
                               gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Number of tests per day
  plots_data_tests_per_day <- gather(df_tests_per_day, variable, measurement, tests_per_day)
  dmfPdfOpen(output_dir, "s6_tests_per_day")
  print(plot_ggplot_day(plots_data_tests_per_day, "Number of tests depending on the app usage ratio per day",
                        "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Number of tests per day smoothed
  dmfPdfOpen(output_dir, "s6_tests_per_day_smooth")
  print(plot_ggplot_day_smooth(plots_data_tests_per_day, "Number of tests depending on the app usage ratio per day (Smoothed)",
                               "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Number of tests per day smoothed random testing
  dmfPdfOpen(output_dir, "s6_tests_per_day_smooth_with_random_testing")
  print(plot_ggplot_day_smooth_2(plots_data_tests_per_day, "Number of tests depending on the app usage ratio per day (Smoothed)",
                               "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Ratio quarantine 
  plots_data_ratio_quarantine <- gather(df_data, variable, measurement, ratio_quarantiners_complying)
  dmfPdfOpen(output_dir, "s6_quarantiners_complying")
  print(plot_ggplot_tick(plots_data_ratio_quarantine, "Ratio agents in quarantine depending on the app usage ratio",
                         "Ratio of people complying to quarantine", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Ratio quarantine smoothed
  dmfPdfOpen(output_dir, "s6_quarantiners_complying_smooth")
  print(plot_ggplot_tick_smooth(plots_data_ratio_quarantine, "Ratio agents in quarantine depending on the app usage ratio (Smoothed)",
                                "Ratio of people complying to quarantine", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================
plot_ggplot_tick <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                             p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users))) +
    #scale_colour_brewer(palette = "viridis", name=gl_plot_variable_name) +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days) 
}

#print(plot_ggplot_tick(plots_data_infected, "Agents infected depending on the app usage ratio",
#                       "Number of infected", gl_mean_start_quaran_day, gl_mean_end_quaran_day))


plot_ggplot_tick_smooth <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                                    p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = tick/4, 
               y = measurement)) +
    stat_smooth(aes(col=as.factor(ratio_of_app_users)), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    #scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days)
}

plot_ggplot_day <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                            p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users))) +
    #scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days)
}

plot_ggplot_day_smooth <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                                   p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_smooth(aes(col=as.factor(ratio_of_app_users)), span=0.1, se=FALSE) +
    #scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days)
}

plot_ggplot_tick_tests <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                             p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users))) +
    #scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, 10000))
    
}

plot_ggplot_tick_tests_2 <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                                   p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  df_line <- df_for_testing_line
  
  x0 <- 0
  y0 <- 0
  x1 <- (gl_mean_end_quaran_day - 7) # 7 days before end of global quarantine
  y1 <- 0
  x2 <- 375 # end of run
  y2_1 <- df_for_testing_1perc * (x2 - x1)
  y2_2 <- df_for_testing_2perc * (x2 - x1)
  y2_5 <- df_for_testing_5perc * (x2 - x1)
  #df_line <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users))) +
    #scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = c(0, 375), ylim = c(0, 10000)) +
    geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1), linetype="dashed") +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2_1), linetype="dashed") +
    geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1), linetype="dotdash") +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2_2), linetype="dotdash") +
    geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1), linetype="dotted") +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2_5), linetype="dotted") +
    annotate(geom="text", x=385, y=y2_1-3, label="1%", color="black", vjust=1) +
    annotate(geom="text", x=385, y=y2_2+3, label="2%", color="black", vjust=0) +
    annotate(geom="text", x=255.5, y=10100, label="5%", color="black", hjust=0, vjust=0)
}

plot_ggplot_day_smooth_2 <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                                   p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_smooth(aes(col=as.factor(ratio_of_app_users)), span=0.1, se=FALSE) +
    #scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme +
    geom_hline(yintercept = df_for_testing_1perc, linetype="dashed", color="black", size=0.6) +
    geom_hline(yintercept = df_for_testing_2perc, linetype="dotdash", color="black", size=0.6) +
    geom_hline(yintercept = df_for_testing_5perc, linetype="dotted", color="black", size=0.6) +
    annotate(geom="text", x=385, y=df_for_testing_1perc-3, label="1%", color="black", vjust=1) +
    annotate(geom="text", x=385, y=df_for_testing_2perc+3, label="2%", color="black", vjust=0) +
    annotate(geom="text", x=385, y=df_for_testing_5perc+3, label="5%", color="black", vjust=0) + coord_cartesian(xlim = c(0, 375))
}