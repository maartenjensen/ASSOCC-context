#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6CumulativeInfections <- function(df_scenario6, output_dir, one_plot) {

  name = "s6_infected_cumulative"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  #CUMULATIVE INFECTIONS PER APP USAGE SCENARIO ----------------------------
  df_infections <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(cumulative_youngs_infected = mean(cumulative_youngs_infected, na.rm = TRUE),
              cumulative_students_infected = mean(cumulative_students_infected, na.rm = TRUE),
              cumulative_workers_infected = mean(cumulative_workers_infected, na.rm = TRUE),
              cumulative_retireds_infected = mean(cumulative_retireds_infected, na.rm = TRUE))
  
  df_total_cummul_infections <- df_infections %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(total_cumulative_infections = sum(cumulative_youngs_infected,
                                                cumulative_students_infected,
                                                cumulative_workers_infected,
                                                cumulative_retireds_infected))
  
  df_total_cummul_infections$ratio_of_app_users <- as.factor(df_total_cummul_infections$ratio_of_app_users)

  print(paste(name, " print data", sep=""))
  print(df_total_cummul_infections %>% group_by(ratio_of_app_users) %>% summarise(total_cumulative_infections = max(total_cumulative_infections)))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_total_cummul_infections, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, name)
  print(plot_ggplot(df_total_cummul_infections))
  dmfPdfClose()
}

plot_ggplot <- function(data_to_plot) {
  
  data_to_plot %>%
    ggplot(aes(x = tick / 4,
               y = total_cumulative_infections,
               group = ratio_of_app_users,
               fill = ratio_of_app_users), fill=NA) +
    geom_line(aes(col=as.factor(ratio_of_app_users))) +
    xlab("Days") +
    ylab("Cumulative number of infections") + 
    labs(title="Cumulative Infections depending on the app usage ratio",
         fill="Proportion of App Users",
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    annotate("rect", xmin=gl_mean_start_quaran_day, xmax=gl_mean_end_quaran_day, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = gl_mean_start_quaran_day, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = gl_mean_end_quaran_day, linetype="dashed", color="red", size=0.3) +
    #scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = gl_x_lim_days)
}