#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6ContactsPerDay <- function(df_scenario6, output_dir, one_plot) {

  name = "s6_contacts_per_day"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  
  # CONTACTS PER LOCATION TYPE PER APP USAGE SCENARIO ----------------------------
  df_contacts_gp <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
    summarise(contacts_in_essential_shops = mean(contacts_in_essential_shops, na.rm = TRUE),
              contacts_in_homes = mean(contacts_in_homes, na.rm = TRUE),
              contacts_in_hospitals = mean(contacts_in_hospitals, na.rm = TRUE),
              contacts_in_non_essential_shops = mean(contacts_in_non_essential_shops, na.rm = TRUE),
              contacts_in_private_leisure = mean(contacts_in_private_leisure, na.rm = TRUE),
              contacts_in_public_leisure = mean(contacts_in_public_leisure, na.rm = TRUE),
              contacts_in_pubtrans = mean(contacts_in_pubtrans, na.rm = TRUE),
              contacts_in_queuing = mean(contacts_in_queuing, na.rm = TRUE),
              contacts_in_schools = mean(contacts_in_schools, na.rm = TRUE),
              contacts_in_shared_cars = mean(contacts_in_shared_cars, na.rm = TRUE),
              contacts_in_universities = mean(contacts_in_universities, na.rm = TRUE),
              contacts_in_workplaces = mean(contacts_in_workplaces, na.rm = TRUE),
              dead_people = mean(dead_people, na.rm = TRUE))
  colnames(df_contacts_gp)

  # Remove ticks that do not form a complete day
  df_contacts_gp <- filter(df_contacts_gp, tick < 1484) #1500), this is for proper weeks
  
  # Count total people then add column for dead people
  total_people = dmfGetTotalAmountOfPeople(df_scenario6)
  df_contacts_gp$total_people = total_people - df_contacts_gp$dead_people

  # Add days converted from ticks
  df_contacts_gp$day <- dmfConvertTicksToDay(df_contacts_gp$tick)  
  
  df_contacts_gp$week <- dmfConvertTicksToWeek(df_contacts_gp$tick)

  # Calculate contacts per day per location type
  df_contacts_gp_by_day <- df_contacts_gp %>% 
    group_by(day, ratio_of_app_users) %>% 
    summarise(contacts_in_essential_shops = sum(contacts_in_essential_shops, na.rm = TRUE),
              contacts_in_homes = sum(contacts_in_homes, na.rm = TRUE),
              contacts_in_hospitals = sum(contacts_in_hospitals, na.rm = TRUE),
              contacts_in_non_essential_shops = sum(contacts_in_non_essential_shops, na.rm = TRUE),
              contacts_in_private_leisure = sum(contacts_in_private_leisure, na.rm = TRUE),
              contacts_in_public_leisure = sum(contacts_in_public_leisure, na.rm = TRUE),
              contacts_in_pubtrans = sum(contacts_in_pubtrans, na.rm = TRUE),
              contacts_in_queuing = sum(contacts_in_queuing, na.rm = TRUE),
              contacts_in_schools = sum(contacts_in_schools, na.rm = TRUE),
              contacts_in_shared_cars = sum(contacts_in_shared_cars, na.rm = TRUE),
              contacts_in_universities = sum(contacts_in_universities, na.rm = TRUE),
              contacts_in_workplaces = sum(contacts_in_workplaces, na.rm = TRUE),
              total_people = mean(total_people, na.rm = TRUE),
              week = mean(week, na.rm = TRUE))
  
  # Divide by total_people
  df_contacts_gp_by_day_per_agent <- df_contacts_gp_by_day %>%
    group_by(day, ratio_of_app_users) %>% 
    summarise(AvgNumberOfContactsDay = sum(contacts_in_essential_shops, contacts_in_homes,
                                          contacts_in_hospitals, contacts_in_non_essential_shops,
                                          contacts_in_private_leisure, contacts_in_public_leisure,
                                          contacts_in_pubtrans, contacts_in_queuing,
                                          contacts_in_schools, contacts_in_shared_cars,
                                          contacts_in_universities, contacts_in_workplaces, na.rm = TRUE) / total_people,
              week = week)
  
  # Divide by week
  df_contacts_gp_by_day_per_agent_week_average <- df_contacts_gp_by_day_per_agent %>%
    group_by(week, ratio_of_app_users) %>% 
    summarise(AvgNumberOfContactsDayWeekAvg = mean(AvgNumberOfContactsDay, na.rm = TRUE),
              day = mean(day))
  
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_contacts_gp_by_day_per_agent, file=paste(output_dir, "/plot_data_", name, ".csv", sep=""))
  write.csv(df_contacts_gp_by_day_per_agent_week_average, file=paste(output_dir, "/plot_data_", name, "_week_avg.csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_contacts_gp_by_day_per_agent <- gather(df_contacts_gp_by_day_per_agent, variable, measurement, AvgNumberOfContactsDay)
  seg_contacts_gp_by_day_per_agent_week_avg <- gather(df_contacts_gp_by_day_per_agent_week_average, variable, measurement, AvgNumberOfContactsDayWeekAvg)
  
  print(paste(name, " making plots", sep=""))

  dmfPdfOpen(output_dir, "s6_contacts_per_agent")
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, max(seg_contacts_gp_by_day_per_agent$measurement)))
  print(plot_ggplot(seg_contacts_gp_by_day_per_agent, gl_mean_start_quaran_day, gl_mean_end_quaran_day, limits))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "s6_contacts_per_agent_smooth")
  # manual limit
  #limits = coord_cartesian(xlim=c(0,375 - 30), ylim = c(0, max(seg_contacts_gp_by_day_per_agent$measurement)))
  limits = coord_cartesian(xlim=gl_x_lim_days, ylim = c(0, max(seg_contacts_gp_by_day_per_agent$measurement)))
  print(plot_ggplot_smooth(seg_contacts_gp_by_day_per_agent, gl_mean_start_quaran_day, gl_mean_end_quaran_day, limits))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, "s6_contacts_per_agent_week_avg")
  limits = coord_cartesian(xlim = gl_x_lim_days, ylim = c(0, 1.1 * max(seg_contacts_gp_by_day_per_agent_week_avg$measurement)))
  print(plot_ggplot(seg_contacts_gp_by_day_per_agent_week_avg, gl_mean_start_quaran_day, gl_mean_end_quaran_day, limits, p_size = 1.2, p_title_add = " (averaged per week)"))
  dmfPdfClose()
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================
plot_ggplot <- function(data_to_plot, p_mean_start_quaran = 0, p_mean_end_quaran = 0, p_limits = c(0,100), p_size = 1, p_title_add = "") {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_line(aes(col=as.factor(ratio_of_app_users)), size=p_size) +
    #scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    xlab("Days") +
    ylab("Average contacts per agent") + 
    labs(title=paste("Number of contacts average per agent per day", p_title_add, sep=""), 
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    annotate("rect", xmin=p_mean_start_quaran, xmax=p_mean_end_quaran, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + p_limits
}

plot_ggplot_smooth <- function(data_to_plot, p_mean_start_quaran = 0, p_mean_end_quaran = 0, p_limits = c(0,100)) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement)) +
    geom_smooth(aes(col=as.factor(ratio_of_app_users)), span=0.1, se=FALSE) +
    #scale_colour_brewer(palette = "Spectral", name="App users ratio") +
    scale_colour_viridis_d(name=gl_plot_variable_name) +
    xlab("Days") +
    ylab("Average contacts per agent") + 
    labs(title=paste("Number of contacts average per agent per day (smoothed)"),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    annotate("rect", xmin=p_mean_start_quaran, xmax=p_mean_end_quaran, ymin=-Inf, ymax=Inf, alpha=0.1, fill="red") +
    geom_vline(xintercept = p_mean_start_quaran, linetype="dashed", color="red", size=0.3) +
    geom_vline(xintercept = p_mean_end_quaran, linetype="dashed", color="red", size=0.3) +
    gl_plot_guides + gl_plot_theme + p_limits
}