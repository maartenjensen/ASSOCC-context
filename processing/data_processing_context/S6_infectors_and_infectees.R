#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6InfectorsAndInfectees <- function(df_scenario6, output_dir, one_plot) {
  
  name = "s6_cummulative_infector_and_infectees"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  df_cummulative_infectors_and_infectee <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(young_infected = mean(cumulative_youngs_infected, na.rm = TRUE),
              young_infector = mean(cumulative_youngs_infector, na.rm = TRUE),
              student_infected = mean(cumulative_students_infected, na.rm = TRUE),
              student_infector = mean(cumulative_students_infector, na.rm = TRUE),
              worker_infected = mean(cumulative_workers_infected, na.rm = TRUE),
              worker_infector = mean(cumulative_workers_infector, na.rm = TRUE),
              retired_infected = mean(cumulative_retireds_infected, na.rm = TRUE),
              retired_infector = mean(cumulative_retireds_infector, na.rm = TRUE))
  colnames(df_cummulative_infectors_and_infectee)

  df_cummulative_infectors_and_infectee$day <- dmfConvertTicksToDay(df_cummulative_infectors_and_infectee$tick)
  
  # Sum for every day (combine the four ticks)
  df_cummulative_infectors_and_infectee_avg_day <- df_cummulative_infectors_and_infectee %>% 
    group_by(day, ratio_of_app_users) %>% 
    summarise(young_infected = mean(young_infected, na.rm = TRUE),
              young_infector = mean(young_infector, na.rm = TRUE),
              student_infected = mean(student_infected, na.rm = TRUE),
              student_infector = mean(student_infector, na.rm = TRUE),
              worker_infected = mean(worker_infected, na.rm = TRUE),
              worker_infector = mean(worker_infector, na.rm = TRUE),
              retired_infected = mean(retired_infected, na.rm = TRUE),
              retired_infector = mean(retired_infector, na.rm = TRUE))
  
  df_cummulative_infectee_avg_day <- df_cummulative_infectors_and_infectee %>% 
    group_by(day, ratio_of_app_users) %>% 
    summarise(young_infected = mean(young_infected, na.rm = TRUE),
              student_infected = mean(student_infected, na.rm = TRUE),
              worker_infected = mean(worker_infected, na.rm = TRUE),
              retired_infected = mean(retired_infected, na.rm = TRUE))

  print(df_cummulative_infectee_avg_day %>% group_by(ratio_of_app_users) %>% summarise(young_infected   = max(young_infected, na.rm = TRUE),
                                                                                       student_infected = max(student_infected, na.rm = TRUE),
                                                                                       worker_infected  = max(worker_infected, na.rm = TRUE),
                                                                                       retired_infected = max(retired_infected, na.rm = TRUE)))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_cummulative_infectee_avg_day, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  #seg_infectortee_day <- gather(df_cummulative_infectee_avg_day, key=Age_group, value=measurement, young_infected:retired_infector)
  seg_infectee_day <- gather(df_cummulative_infectee_avg_day, key=Age_group, value=measurement, young_infected:retired_infected)
  
  print(paste(name, " making plots", sep=""))
  
  limits = coord_cartesian(ylim = c(0, max(seg_infectee_day$measurement)))
  for(i in unique(seg_infectee_day$ratio_of_app_users)) {
    dmfPdfOpen(output_dir, paste("s6_cummulative_infected_age_app_", i, sep=""))
    print(plot_ggplot(seg_infectee_day[seg_infectee_day$ratio_of_app_users==i, ], i, limits))
    dmfPdfClose()
  }  
  
  dmfPdfOpen(output_dir, "s6_infectee_app_facet_wrap")
  print(plot_ggplot_facet(seg_infectee_day))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================
plot_ggplot <- function(data_to_plot, app_use, p_limits) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement,
               group = Age_group,
               fill = Age_group), fill=NA) +
    geom_line(aes(col=Age_group)) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) +
    xlab("Days") +
    ylab("Cumulative agents infected") +
    labs(title=paste("Cummulative infected by age group - app usage ratio:", app_use),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides + gl_plot_theme + p_limits +
    scale_colour_manual(values=c('#391c1c', '#16727e', '#de9236', '#cedecb'))
}

plot_ggplot_facet <- function(data_to_plot, p_limits) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement,
               group = Age_group,
               fill = Age_group), fill=NA) +
    geom_line(aes(col=Age_group)) + facet_wrap(~ratio_of_app_users) +
    xlab("Days") +
    ylab("Cumulative agents infected") +
    labs(title=paste("Cummulative infected by age group dependent on app usage ratio"),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides_side + gl_plot_theme_side +
    scale_colour_manual(values=c('#391c1c', '#16727e', '#de9236', '#cedecb'))
}