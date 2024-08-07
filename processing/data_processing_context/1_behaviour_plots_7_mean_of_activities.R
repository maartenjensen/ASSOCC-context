behaviourPlot7MeanOfActivities <- function() {
  
  print("-- Plot mean of activities...")
  
  #-------------------------------------------------------
  #================== Experiment preset ==================
  experiment_presets = sort(unique(df_final_filtered$ce_context_experiment_presets))
  
  str_activities_print <- "\nPreset & RH/WH & WW/S & GROC & LUX & LEI \\\\ \n"
  str_population_status_print <- "\nPreset & Infected & Believe Infected & Healthy \\\\ \n"
  str_quarantine_print <- "\nPreset & Asked to quarantine & Breaking Quarantine & Perc \\\\ \n"
  
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
    
    
    str_quarantine_print <- paste(str_quarantine_print,
                                  experiment_preset, " & ",
                                  round(mean(df_quarantiners$quarantine_asked_to_perc), digits = 2), " & ",
                                  round(mean(df_quarantiners$quarantine_breaking_perc), digits = 2), " & ", 
                                  round(mean(df_quarantiners$quarantine_breaking_perc)/mean(df_quarantiners$quarantine_asked_to_perc)*100, digits = 2),"% \\\\ \n", sep="")
  }
  
  cat(str_activities_print)
  cat(str_population_status_print)
  cat(str_quarantine_print)
}