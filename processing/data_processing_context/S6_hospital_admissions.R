#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6HospitalAdmissions <- function(df_scenario6, output_dir, one_plot) {
  
  name = "s6_hospital_admissions"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  # determine start and end of global quarantine
  df_hospital_admissions <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(Young = mean(hospitalizations_youngs_this_tick, na.rm = TRUE),
              Student = mean(hospitalizations_students_this_tick, na.rm = TRUE),
              Worker = mean(hospitalizations_workers_this_tick, na.rm = TRUE),
              Retired = mean(hospitalizations_retired_this_tick, na.rm = TRUE))
  
  df_hospital_admissions_cumulative <- df_hospital_admissions %>% 
    group_by(ratio_of_app_users) %>% mutate(Young   = cumsum(Young),
                                            Student = cumsum(Student),
                                            Worker   = cumsum(Worker),
                                            Retired = cumsum(Retired))
  
  df_taken_hospital_beds <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(taken_hospital_beds = mean(taken_hospital_beds, na.rm = TRUE))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_hospital_admissions, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_hospital_admissions_cumulative, file=paste(output_dir, "/plot_data_", name, "_cumulative.csv", sep=""))
  write.csv(df_taken_hospital_beds, file=paste(output_dir, "/plot_data_", name, "_hospital_beds.csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_hospital_admissions <- gather(df_hospital_admissions, Hospitalized, measurement, Young:Retired)
  seg_hospital_admissions_cumsum <- gather(df_hospital_admissions_cumulative, Hospitalized, measurement, Young:Retired)
  seg_taken_hospital_beds <- gather(df_taken_hospital_beds, Hospitalized, measurement, taken_hospital_beds)
  
  print(paste(name, " making plots", sep=""))
  
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, max(seg_hospital_admissions$measurement)))
  for(i in unique(seg_hospital_admissions$ratio_of_app_users)) {
    dmfPdfOpen(output_dir, paste("s6_hospital_admissions_app_", i, sep=""))
    p_title = paste("Hospitalizations per tick : App usage", i)
    print(plot_ggplot(seg_hospital_admissions[seg_hospital_admissions$ratio_of_app_users==i, ], limits, p_title))
    dmfPdfClose()
  }
  
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, 0.5*max(seg_hospital_admissions$measurement)))
  for(i in unique(seg_hospital_admissions$ratio_of_app_users)) {
    dmfPdfOpen(output_dir, paste("s6_hospital_admissions_app_", i, sep=""))
    p_title = paste("Hospitalizations per tick (smoothed) - app usage ratio:", i)
    print(plot_ggplot_smooth(seg_hospital_admissions[seg_hospital_admissions$ratio_of_app_users==i, ], limits, p_title, "Average hospitalizations"))
    dmfPdfClose()
  }
  
  p_title = paste("Hospitalizations per tick (smoothed) dependent on app usage ratio")
  dmfPdfOpen(output_dir, "s6_hospital_admissions_app_facet_wrap")
  print(plot_ggplot_smooth_facet(seg_hospital_admissions, p_title, "Hospitalizations"))
  dmfPdfClose()
  
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, max(seg_hospital_admissions_cumsum$measurement)))
  for(i in unique(seg_hospital_admissions_cumsum$ratio_of_app_users)) {
    dmfPdfOpen(output_dir, paste("s6_hospital_admissions_cumsum_app_", i, sep=""))
    p_title = paste("Hospitalizations cummulative (smoothed) - app usage ratio:", i)
    print(plot_ggplot_smooth_cumsum(seg_hospital_admissions_cumsum[seg_hospital_admissions_cumsum$ratio_of_app_users==i, ], limits, p_title, "Cumulative hospitalizations"))
    dmfPdfClose()
  }
  
  dmfPdfOpen(output_dir, "s6_hospital_admissions_cumsum_app_facet_wrap")
  p_title = paste("Hospitalizations cummulative (smoothed) dependent on app usage ratio")
  print(plot_ggplot_smooth_facet_day(seg_hospital_admissions_cumsum, p_title, "Cumulative hospitalizations"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "s6_hospital_current_agents")
  p_title = paste("Current hospitalized agents dependent on app usage ratio")
  print(plot_ggplot_day_smooth(seg_taken_hospital_beds, p_title, "Hospitalizations", gl_mean_start_quaran_day, gl_mean_end_quaran_day))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================

plot_ggplot_tick_smooth <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                                    p_mean_start_quaran = 0, p_mean_end_quaran = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement)) +
    stat_smooth(aes(col=as.factor(ratio_of_app_users)), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Ticks", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran, xmax=p_mean_end_quaran, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days) +
    scale_colour_manual(values=c('#391c1c', '#16727e', '#de9236', '#cedecb'))
}

plot_ggplot_day_smooth <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                                    p_mean_start_quaran = 0, p_mean_end_quaran = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement)) +
    stat_smooth(aes(col=as.factor(ratio_of_app_users)), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    scale_colour_viridis_d(name="App users ratio") +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    annotate("rect", xmin=p_mean_start_quaran, xmax=p_mean_end_quaran, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days) 
}

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
    gl_plot_guides + gl_plot_theme + p_limits +
    scale_colour_manual(values=c('#391c1c', '#16727e', '#de9236', '#cedecb'))
}

plot_ggplot_smooth <- function(data_to_plot, p_limits, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = tick/4, 
               y = measurement,
               group = Hospitalized,
               fill = Hospitalized), fill=NA) +
    stat_smooth(aes(col=Hospitalized), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) + 
    xlab("Days") +
    ylab(p_y_lab) + 
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides + gl_plot_theme + p_limits +
    scale_colour_manual(values=c('#391c1c', '#16727e', '#de9236', '#cedecb'))
}

plot_ggplot_smooth_cumsum <- function(data_to_plot, p_limits, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = tick/4, 
               y = measurement,
               group = Hospitalized), fill=NA) +
    stat_smooth(aes(col=Hospitalized), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) + 
    xlab("Days") +
    ylab(p_y_lab) + 
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         colour = "Cummulative hospitalized") +
    gl_plot_guides + gl_plot_theme + p_limits +
    scale_colour_manual(values=c('#391c1c', '#16727e', '#de9236', '#cedecb'))
}

plot_ggplot_smooth_facet <- function(data_to_plot, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement,
               group = Hospitalized,
               fill = Hospitalized), fill=NA) +
    stat_smooth(aes(col=Hospitalized), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    facet_wrap(~ratio_of_app_users) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) + 
    xlab("Ticks") +
    ylab(p_y_lab) + 
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides_side + gl_plot_theme_side +
    scale_colour_manual(values=c('#391c1c', '#16727e', '#de9236', '#cedecb'))
}

plot_ggplot_smooth_facet_day <- function(data_to_plot, p_title = "None", p_y_lab = "None") {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4, 
               y = measurement,
               group = Hospitalized,
               fill = Hospitalized), fill=NA) +
    stat_smooth(aes(col=Hospitalized), method = "loess", formula = y ~ x, se = FALSE, span=0.1) +
    facet_wrap(~ratio_of_app_users) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) + 
    xlab("Days") +
    ylab(p_y_lab) + 
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides_side + gl_plot_theme_side +
    scale_colour_manual(values=c('#391c1c', '#16727e', '#de9236', '#cedecb'))
}