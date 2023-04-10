#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6RCumulativeInfections <- function(df_scenario6, output_dir, one_plot) {

  name = "s6R_infected_compliance_tests"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  #CUMULATIVE INFECTIONS PER APP USAGE SCENARIO ----------------------------
  df_infections <- df_scenario6 %>% 
    group_by(tick, ratio_population_randomly_tested_daily) %>% 
    summarise(cumulative_youngs_infected = mean(cumulative_youngs_infected, na.rm = TRUE),
              cumulative_students_infected = mean(cumulative_students_infected, na.rm = TRUE),
              cumulative_workers_infected = mean(cumulative_workers_infected, na.rm = TRUE),
              cumulative_retireds_infected = mean(cumulative_retireds_infected, na.rm = TRUE))
  
  df_total_cummul_infections <- df_infections %>% 
    group_by(tick, ratio_population_randomly_tested_daily) %>% 
    summarise(total_cumulative_infections = sum(cumulative_youngs_infected,
                                                cumulative_students_infected,
                                                cumulative_workers_infected,
                                                cumulative_retireds_infected))
  
  df_total_cummul_infections$ratio_population_randomly_tested_daily <- as.factor(df_total_cummul_infections$ratio_population_randomly_tested_daily)

  print(paste(name, " writing CSV", sep=""))
  write.csv(df_total_cummul_infections, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, "s6R_cumulative_infections")
  print(plot_ggplot(df_total_cummul_infections))
  dmfPdfClose()
}

plot_ggplot <- function(data_to_plot) {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4,
               y = total_cumulative_infections,
               group = ratio_population_randomly_tested_daily,
               fill = ratio_population_randomly_tested_daily), fill=NA) +
    geom_line(aes(col=as.factor(ratio_population_randomly_tested_daily))) +
    xlab("Days") +
    ylab("Cumulative number of infections") + 
    labs(title="Cumulative Infections depending on the random tests per day ratio",
         fill="Daily random tests ratio",
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    scale_colour_brewer(palette = "Spectral", name="Random tests ratio") +
    gl_plot_guides + gl_plot_theme
}
