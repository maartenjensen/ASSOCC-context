behaviourPlot7InfectionsComparison <- function() {
  
  print("-- Comparison of infections plot for normal infection curve --")
  
  
  df_infections_comparison <- df_final
  df_infections_comparison <- select(df_infections_comparison, tick, ce_context_depth, random_seed, ce_context_experiment_presets, infected, believe_infected, healthy)
  #df_infections_comparison$run_setting <- paste("CE:", df_infections_comparison$ce_context_depth, ", R:", df_infections_comparison$random_seed , sep="")
  
  df_infections_comparison <- df_infections_comparison[df_final$random_seed == 0, ]
  
  # In df_infections_comparison filter the ce_context_experiment_presets = "0.2 Original ASSOCC-lockdown" and "5.2 DCSD-5-optimisation-lockdown"
  df_infections_comparison_normal <- df_infections_comparison[df_infections_comparison$ce_context_experiment_presets %in% c("0.1 Original ASSOCC" , "5.1 DCSD-5-optimisation"), ]
  
  p <- ggplot(df_infections_comparison_normal, aes(x = tick, y = infected, col=ce_context_experiment_presets)) + geom_line()
  
  #p <- p + scale_colour_manual(
  #  labels=c('infected'='Infected',
  #           'believe_infected' ='Believe Infected', 
  #           'healthy' ='Healthy'),
  #  values=c('#ff7c73', '#b00300', '#afd16f'),
  #  breaks=c('infected','believe_infected','healthy'))
  p <- p + labs(x = "Ticks", y = "Status of n agents", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Infections comparison (", experiment_preset,")", sep=""))
  #p <- p + geom_line(data = filter(df_population_status, `Population Status` == "believe_infected"), linewidth = 2)
  if (plot_type == "one") { behaviourEnablePdf(paste(directory_files, "_infections_comparison_normal", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- Comparison of infections plot for global lockdown --")

  
  # In df_infections_comparison filter the ce_context_experiment_presets = "0.2 Original ASSOCC-lockdown" and "5.2 DCSD-5-optimisation-lockdown"
  df_infections_comparison_lockdown <- df_infections_comparison[df_infections_comparison$ce_context_experiment_presets %in% c("0.2 Original ASSOCC-lockdown" , "5.2 DCSD-5-optimisation-lockdown"), ]
  
  p <- ggplot(df_infections_comparison_lockdown, aes(x = tick, y = infected, col=ce_context_experiment_presets)) + geom_line()
  
  #p <- p + scale_colour_manual(
  #  labels=c('infected'='Infected',
  #           'believe_infected' ='Believe Infected', 
  #           'healthy' ='Healthy'),
  #  values=c('#ff7c73', '#b00300', '#afd16f'),
  #  breaks=c('infected','believe_infected','healthy'))
  p <- p + labs(x = "Ticks", y = "Status of n agents", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, 480), ylim = c(0, 1020)) + labs(title=paste("Infections comparison (", experiment_preset,")", sep=""))
  #p <- p + geom_line(data = filter(df_population_status, `Population Status` == "believe_infected"), linewidth = 2)
  if (plot_type == "one") { behaviourEnablePdf(paste(directory_files, "_infections_comparison_global_lockdown", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
}

behaviourPlot7MeanOfActivities <- function() {
  
  print("-- Plot mean of activities...")
  
  #-------------------------------------------------------
  #================== Experiment preset ==================
  experiment_presets = sort(unique(df_final_filtered$ce_context_experiment_presets))
  
  str_activities_print <- "\nPreset & RH/WH & WW/S & GROC & LUX & LEI \\\\ \n"
  str_population_status_print <- "\nPreset & Infected & Believe Infected & Healthy \\\\ \n"
  str_quarantine_print <- "\nPreset & Should quarantine & Breaking & Perc \\\\ \n"
  
  for (experiment_preset in experiment_presets)
  {
    subset_df <- filter(df_final_filtered, ce_context_experiment_presets == experiment_preset)
    
    #------------------------------------------------------
    #==================== Activities Mean =================
    
    df_activities <- select(subset_df, tick, ce_context_depth, people_alive, shop_groceries_perc, rest_at_home_perc, shop_luxury_perc,
                            at_private_leisure_perc, at_public_leisure_perc, study_at_school_perc,
                            study_at_university_perc, work_at_work_perc, work_at_home_perc, at_treatment_perc)
    
    df_activities$rest_at_home_perc[1] <- 100 # Everyone is at home, but since the motivation is not taken into account, all cells in the first row indicate 0
    
    #----- Prepare the data frame ------
    df_activities_mean <- df_activities %>% mutate(work_rest_at_home = rest_at_home_perc + work_at_home_perc)
    df_activities_mean <- df_activities_mean %>% mutate(obligation_out = study_at_school_perc + study_at_university_perc + work_at_work_perc)
    df_activities_mean <- df_activities_mean %>% mutate(shop_grocery = shop_groceries_perc)
    df_activities_mean <- df_activities_mean %>% mutate(shop_luxury = shop_luxury_perc)
    df_activities_mean <- df_activities_mean %>% mutate(leisure = at_private_leisure_perc + at_public_leisure_perc)
    
    #----- Print the data for the activity averages -----
    str_activities_print <- paste(str_activities_print, experiment_preset, " & ",
                                  round(mean(df_activities_mean$work_rest_at_home), digits = 2), " & ",
                                  round(mean(df_activities_mean$obligation_out), digits = 2), " & ", 
                                  round(mean(df_activities_mean$shop_grocery), digits = 2), " & ", round(mean(df_activities_mean$shop_luxury), digits = 2)," & ",
                                  round(mean(df_activities_mean$leisure), digits = 2), " \\\\ \n", sep="")
    
    #------------------------------------------------------
    #===================== Infected Mean ==================
    df_population_status <- select(subset_df, tick, ce_context_depth, infected, believe_infected, healthy)
    
    str_population_status_print <- paste(str_population_status_print,
                                         experiment_preset, " & ",
                                         round(mean(df_population_status$infected), digits = 2), " & ",
                                         round(mean(df_population_status$believe_infected), digits = 2), " & ", 
                                         round(mean(df_population_status$healthy), digits = 2), " \\\\ \n", sep="")
    
    #------------------------------------------------------
    #==================== Quarantine Mean =================
    
    df_quarantiners <- select(subset_df, tick, ce_context_depth, quarantine_asked_to_perc, quarantine_breaking_perc)
    
    # Solve divide by zero of quarantine_breaking_perc
    if (mean(df_quarantiners$quarantine_asked_to_perc) > 0) {
      percentage_breaking_quarantine_of_quarantiners <- round(mean(df_quarantiners$quarantine_breaking_perc)/mean(df_quarantiners$quarantine_asked_to_perc)*100, digits = 2)
    } else {
      percentage_breaking_quarantine_of_quarantiners <- 0 
    } 
    
    str_quarantine_print <- paste(str_quarantine_print,
                                  experiment_preset, " & ",
                                  round(mean(df_quarantiners$quarantine_asked_to_perc), digits = 2), " & ",
                                  round(mean(df_quarantiners$quarantine_breaking_perc), digits = 2), " & ", 
                                  percentage_breaking_quarantine_of_quarantiners,"\\% \\\\ \n", sep="")
  }
  
  cat(str_activities_print)
  cat(str_population_status_print)
  cat(str_quarantine_print)
}