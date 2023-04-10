#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6Quarantiners <- function(df_scenario6, output_dir, one_plot) {

  name = "s6_quarantiners_per_day"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  # quarantiners PER APP USAGE SCENARIO ----------------------------
  df_quarantiners <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(count_officially_quarantiners = mean(count_officially_quarantiners, na.rm = TRUE),
              count_actual_quarantiners = mean(count_actual_quarantiners, na.rm = TRUE),
              dead_people = mean(dead_people, na.rm = TRUE))
  colnames(df_quarantiners)

  # Count total people then add column for dead people
  total_people = dmfGetTotalAmountOfPeople(df_scenario6)
  df_quarantiners$total_people = total_people - df_quarantiners$dead_people

  df_quarantiners_ratio_per_agent <- df_quarantiners %>%
    group_by(tick, ratio_of_app_users) %>% 
    summarise(QuarantinersRatio = count_officially_quarantiners / total_people,
              ActualQuarantinersRatio = count_actual_quarantiners / total_people)
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_quarantiners_ratio_per_agent, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_quarantiners_ratio_per_agent <- gather(df_quarantiners_ratio_per_agent, variable, measurement, QuarantinersRatio)
  seg_actual_quarantiners_ratio_per_agent <- gather(df_quarantiners_ratio_per_agent, variable, measurement, ActualQuarantinersRatio)
  
  print(paste(name, " making plots", sep=""))

  dmfPdfOpen(output_dir, "s6_quarantiners_per_agent")
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, max(seg_quarantiners_ratio_per_agent$measurement)))
  print(plot_ggplot(seg_quarantiners_ratio_per_agent, gl_mean_start_quaran_day, gl_mean_end_quaran_day, limits))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "s6_quarantiners_actual_per_agent")
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, max(seg_actual_quarantiners_ratio_per_agent$measurement)))
  print(plot_ggplot(seg_actual_quarantiners_ratio_per_agent, gl_mean_start_quaran_day, gl_mean_end_quaran_day, limits))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "s6_quarantiners_smooth")
  # manual limit
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, max(seg_quarantiners_ratio_per_agent$measurement)))
  print(plot_ggplot_smooth(seg_quarantiners_ratio_per_agent, gl_mean_start_quaran_day, gl_mean_end_quaran_day, limits))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "s6_quarantiners_actual_smooth")
  # manual limit
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, max(seg_actual_quarantiners_ratio_per_agent$measurement)))
  print(plot_ggplot_smooth(seg_actual_quarantiners_ratio_per_agent, gl_mean_start_quaran_day, gl_mean_end_quaran_day, limits))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================
plot_ggplot <- function(data_to_plot, p_mean_start_quaran = 0, p_mean_end_quaran = 0, p_limits = c(0,100), p_size = 1, p_title_add = "") {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users)), size=p_size) +
    #scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    xlab("Days") +
    ylab("Ratio of agents in quarantine") + 
    labs(title=paste("Ratio of total agents quarantining", p_title_add, sep=""), 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    annotate("rect", xmin=p_mean_start_quaran, xmax=p_mean_end_quaran, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + p_limits
}

plot_ggplot_smooth <- function(data_to_plot, p_mean_start_quaran = 0, p_mean_end_quaran = 0, p_limits = c(0,100)) {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement)) +
    geom_smooth(aes(col=as.factor(ratio_of_app_users)), span=0.1, se=FALSE) +
    #scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    xlab("Days") +
    ylab("Ratio of agents in quarantine") + 
    labs(title=paste("Ratio of total agents quarantining (smoothed)"),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    annotate("rect", xmin=p_mean_start_quaran, xmax=p_mean_end_quaran, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + p_limits
}