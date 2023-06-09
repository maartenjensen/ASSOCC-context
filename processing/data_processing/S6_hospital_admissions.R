#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6HospitalAdmissions <- function(df_scenario6, output_dir, one_plot) {
  
  name = "s6_hospital_admissions"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  df_hospital_admissions <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(Young = mean(hospitalizations_youngs_this_tick, na.rm = TRUE),
              Student = mean(hospitalizations_students_this_tick, na.rm = TRUE),
              Worker = mean(hospitalizations_workers_this_tick, na.rm = TRUE),
              Retired = mean(hospitalizations_retired_this_tick, na.rm = TRUE))
  
  df_hospital_admissions_cumulative <- df_hospital_admissions %>% 
    group_by(ratio_of_app_users) %>% mutate(Cummulative_young   = cumsum(Young),
                                            Cummulative_student = cumsum(Student),
                                            Cummulative_worker   = cumsum(Worker),
                                            Cummulative_retired = cumsum(Retired))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_hospital_admissions_cumulative, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_hospital_admissions <- gather(df_hospital_admissions, Hospitalized, measurement, Young:Retired)
  seg_hospital_admissions_cumsum <- gather(df_hospital_admissions_cumulative, Hospitalized, measurement, Cummulative_young:Cummulative_retired)
  
  print(paste(name, " making plots", sep=""))
  
  #limits = coord_cartesian(ylim = c(0, max(seg_hospital_admissions$measurement)))
  #for(i in unique(seg_hospital_admissions$ratio_of_app_users)) {
  #  dmfPdfOpen(output_dir, paste("s6_hospital_admissions_app_", i, sep=""))
  #  p_title = paste("Hospitalizations per tick : App usage", i)
  #  print(plot_ggplot(seg_hospital_admissions[seg_hospital_admissions$ratio_of_app_users==i, ], limits, p_title))
  #  dmfPdfClose()
  #}
  
  limits = coord_cartesian(ylim = c(0, 0.5*max(seg_hospital_admissions$measurement)))
  for(i in unique(seg_hospital_admissions$ratio_of_app_users)) {
    dmfPdfOpen(output_dir, paste("s6_hospital_admissions_app_", i, sep=""))
    p_title = paste("Hospitalizations per tick (smoothed) - app usage ratio:", i)
    print(plot_ggplot_smooth(seg_hospital_admissions[seg_hospital_admissions$ratio_of_app_users==i, ], limits, p_title, "Average hospitalizations"))
    dmfPdfClose()
  }
  
  limits = coord_cartesian(ylim = c(0, max(seg_hospital_admissions_cumsum$measurement)))
  for(i in unique(seg_hospital_admissions_cumsum$ratio_of_app_users)) {
    dmfPdfOpen(output_dir, paste("s6_hospital_admissions_cumsum_app_", i, sep=""))
    p_title = paste("Hospitalizations per tick cummulative (smoothed) - app usage ratio:", i)
    print(plot_ggplot_smooth(seg_hospital_admissions_cumsum[seg_hospital_admissions_cumsum$ratio_of_app_users==i, ], limits, p_title, "Cumulative hospitalizations"))
    dmfPdfClose()
  }
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================
plot_ggplot <- function(data_to_plot, p_limits, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement,
               group = Hospitalized,
               fill = Hospitalized), fill=NA) +
    geom_line(aes(col=Hospitalized)) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) +
    xlab("Ticks") +
    ylab(p_y_lab) + 
    labs(title=p_title,
         fill="Age",
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides + gl_plot_theme + p_limits
}

plot_ggplot_smooth <- function(data_to_plot, p_limits, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement,
               group = Hospitalized,
               fill = Hospitalized), fill=NA) +
    stat_smooth(aes(col=Hospitalized), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) + 
    xlab("Ticks") +
    ylab(p_y_lab) + 
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides + gl_plot_theme + p_limits
}
