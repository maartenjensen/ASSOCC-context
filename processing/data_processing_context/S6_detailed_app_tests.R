#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6DetailedAppTests <- function(df_scenario6, output_dir, one_plot) {
  
  name = "s6_detailed_app_tests"
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))

  df_data <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(tests_performed = mean(tests_performed, na.rm = TRUE),
              tests_performed_by_tracking_app = mean(tests_performed_by_tracking_app, na.rm = TRUE),
              people_recorded_positive_by_tracking_app = mean(people_recorded_positive_by_tracking_app, na.rm = TRUE),
              tests_performed_by_tracking_app_positive = mean(tests_performed_by_tracking_app_positive, na.rm = TRUE),
              tests_performed_by_tracking_app_positive_believe_healthy = mean(tests_performed_by_tracking_app_positive_believe_healthy, na.rm = TRUE),
              tests_performed_by_tracking_app_positive_no_symptoms = mean(tests_performed_by_tracking_app_positive_no_symptoms, na.rm = TRUE))
  

  v_testing_days <- gl_mean_end_quaran_day - 7
  print(paste("Started the track and tracing app at day:", v_testing_days))
  v_testing_days <- 375 - v_testing_days
  print(paste("Number of days testing:", v_testing_days))
  
  df_tests_max <- df_data %>% group_by(ratio_of_app_users) %>%
    summarise(tests_performed = max(tests_performed, na.rm = TRUE), tests_performed_day = max(tests_performed, na.rm = TRUE)/v_testing_days,
              tests_performed_day_ratio_divide_1126 = (max(tests_performed, na.rm = TRUE)/v_testing_days)/1126  )
  print(df_tests_max)
  
  #REAL DATA MANIPULATION

  df_tests_per_tick <- df_data %>%
    group_by(ratio_of_app_users) %>%
    arrange(tick) %>% 
    mutate(tests_per_tick = tests_performed - lag(tests_performed, default = first(tests_performed)),
           tests_performed_by_tracking_app = tests_performed_by_tracking_app - lag(tests_performed_by_tracking_app, default = first(tests_performed_by_tracking_app)),
           people_recorded_positive_by_tracking_app = people_recorded_positive_by_tracking_app - 
             lag(people_recorded_positive_by_tracking_app, default = first(people_recorded_positive_by_tracking_app)),
           tests_performed_by_tracking_app_positive = tests_performed_by_tracking_app_positive -
             lag(tests_performed_by_tracking_app_positive, default = first(tests_performed_by_tracking_app_positive)))
  
  df_tests_per_tick$day <- dmfConvertTicksToDay(df_tests_per_tick$tick)
  
  df_tests_per_day <- df_tests_per_tick %>% 
    group_by(day, ratio_of_app_users) %>% 
    summarise(tests_p_day = sum(tests_per_tick, na.rm = TRUE),
              tests_through_contacts_p_day = sum(tests_performed_by_tracking_app, na.rm = TRUE),
              people_recorded_positive_in_total = sum(people_recorded_positive_by_tracking_app, na.rm = TRUE),
              people_recorded_positive_through_contacts = sum(tests_performed_by_tracking_app_positive, na.rm = TRUE))
  
  print(paste(name, " print data", sep=""))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_tests_per_day, file=paste(output_dir, "/plot_data_", name, "_tests.csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  # Total tests performed
  plots_data_tests <- gather(df_data, variable, measurement, tests_performed)
  dmfPdfOpen(output_dir, "s6_tests_cumulative")
  print(plot_ggplot_tick_tests(plots_data_tests, ratio_of_app_users, "Cummulative number of tests depending on the app usage ratio", "Cummulative number of tests",
                         gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  plots_data_tests <- gather(df_data, variable, measurement, tests_performed)
  dmfPdfOpen(output_dir, "s6_tests_cumulative_with_random_testing")
  print(plot_ggplot_tick_tests_2(plots_data_tests, ratio_of_app_users, "Cummulative number of tests depending on the app usage ratio", "Cummulative number of tests",
                               gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Total tests performed by tracking app contacts
  plots_data_tests <- gather(df_data, variable, measurement, tests_performed_by_tracking_app)
  dmfPdfOpen(output_dir, "s6_tests_cumulative")
  print(plot_ggplot_tick_tests(plots_data_tests, ratio_of_app_users, "Cummulative number of tests performed on contacts", "Cummulative number of tests",
                               gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Total tests performed
  plots_data_tests <- gather(df_data, variable, measurement, people_recorded_positive_by_tracking_app)
  dmfPdfOpen(output_dir, "s6_tests_cumulative")
  print(plot_ggplot_tick_tests(plots_data_tests, ratio_of_app_users, "Cummulative people_recorded_positive_in_total", "Cummulative number of tests",
                               gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # Total tests performed
  plots_data_tests <- gather(df_data, variable, measurement, tests_performed_by_tracking_app_positive)
  dmfPdfOpen(output_dir, "s6_tests_cumulative")
  print(plot_ggplot_tick_tests(plots_data_tests, ratio_of_app_users, "Cummulative people_recorded_positive_through_contacts", "Cummulative number of tests",
                               gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  
  
  # Number of tests per day
  #plots_data_tests_per_day <- gather(df_tests_per_day, variable, measurement, tests_p_day)
  #dmfPdfOpen(output_dir, "s6_tests_per_day")
  #print(plot_ggplot_day(plots_data_tests_per_day, ratio_of_app_users, "Number of tests depending on the app usage ratio per day",
  #                      "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  #dmfPdfClose()
  
  # Number of tests per day smoothed
  #dmfPdfOpen(output_dir, "s6_tests_per_day_smooth")
  #print(plot_ggplot_day_smooth(plots_data_tests_per_day, ratio_of_app_users, "Number of tests depending on the app usage ratio per day (Smoothed)",
  #                             "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  #dmfPdfClose()
  
  
  
  
  # people_recorded_positive_in_total
  plots_data_tests_per_day <- gather(df_tests_per_day, variable, measurement, people_recorded_positive_in_total)
  dmfPdfOpen(output_dir, "s6_tests_per_day")
  print(plot_ggplot_day(plots_data_tests_per_day, ratio_of_app_users, "Number people_recorded_positive_in_total",
                        "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # people_recorded_positive_in_total day smoothed
  dmfPdfOpen(output_dir, "s6_tests_per_day_smooth")
  print(plot_ggplot_day_smooth(plots_data_tests_per_day, ratio_of_app_users, "Number people_recorded_positive_in_total (Smoothed)",
                               "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  
  
  
  
  # people_recorded_positive_through_contacts per day
  plots_data_tests_per_day <- gather(df_tests_per_day, variable, measurement, people_recorded_positive_through_contacts)
  dmfPdfOpen(output_dir, "s6_tests_per_day")
  print(plot_ggplot_day(plots_data_tests_per_day, ratio_of_app_users, "Number people_recorded_positive_through_contacts per day",
                        "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  # people_recorded_positive_through_contacts per day smoothed
  dmfPdfOpen(output_dir, "s6_tests_per_day_smooth")
  print(plot_ggplot_day_smooth(plots_data_tests_per_day, ratio_of_app_users, "Number of people_recorded_positive_through_contacts per day (Smoothed)",
                               "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
  
  
  
  
  # Number of tests per day smoothed random testing
  dmfPdfOpen(output_dir, "s6_tests_per_day_smooth_with_random_testing")
  print(plot_ggplot_day_smooth_2(plots_data_tests_per_day, ratio_of_app_users, "Number of tests depending on the app usage ratio per day (Smoothed)",
                               "Number of tests per day", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================
plot_ggplot_day <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None",
                            p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users))) +
    scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, 30))
}

plot_ggplot_day_smooth <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None",
                                   p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_smooth(aes(col=as.factor(ratio_of_app_users)), span=0.1, se=FALSE) +
    scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, 12))
}

plot_ggplot_tick_tests <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None",
                             p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users))) +
    scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, 500))
    
}

plot_ggplot_tick_tests_2 <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None",
                                   p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users))) +
    scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran_tick, xmax=p_mean_end_quaran_tick, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran_tick, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran_tick, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, 500))
  
}

plot_ggplot_day_smooth_2 <- function(data_to_plot, p_independent_variable, p_title = "None", p_y_lab = "None",
                                   p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_smooth(aes(col=as.factor(ratio_of_app_users)), span=0.1, se=FALSE) +
    scale_colour_brewer(palette = "Spectral", name=gl_plot_variable_name) +
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