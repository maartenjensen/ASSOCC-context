#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6RInfectionRatioGP <- function(df_scenario6, output_dir, one_plot) {

  name = "s6R_infection_ratio_gp"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  df_inf_gp <- df_scenario6 %>% 
    group_by(tick, ratio_population_randomly_tested_daily) %>% 
    summarise(essential_shops = mean(people_infected_in_essential_shops, na.rm = TRUE),
              homes = mean(people_infected_in_homes, na.rm = TRUE),
              hospitals = mean(people_infected_in_hospitals, na.rm = TRUE),
              non_essential_shops = mean(people_infected_in_non_essential_shops, na.rm = TRUE),
              private_leisure = mean(people_infected_in_private_leisure, na.rm = TRUE),
              public_leisure = mean(people_infected_in_public_leisure, na.rm = TRUE),
              pubtrans = mean(people_infected_in_pubtrans, na.rm = TRUE),
              queuing = mean(people_infected_in_queuing, na.rm = TRUE),
              schools = mean(people_infected_in_schools, na.rm = TRUE),
              shared_cars = mean(people_infected_in_shared_cars, na.rm = TRUE),
              universities = mean(people_infected_in_universities, na.rm = TRUE),
              workplaces = mean(people_infected_in_workplaces, na.rm = TRUE))
  colnames(df_inf_gp)
  
  df_inf_gp$day <- dmfConvertTicksToDay(df_inf_gp$tick)

  # Sum for every day (combine the four ticks)
  df_inf_gp_accumulated <- df_inf_gp %>% 
    group_by(day, ratio_population_randomly_tested_daily) %>% 
    summarise(essential_shops = sum(essential_shops, na.rm = TRUE),
              homes = sum(homes, na.rm = TRUE),
              hospitals = sum(hospitals, na.rm = TRUE),
              non_essential_shops = sum(non_essential_shops, na.rm = TRUE),
              private_leisure = sum(private_leisure, na.rm = TRUE),
              public_leisure = sum(public_leisure, na.rm = TRUE),
              pubtrans = sum(pubtrans, na.rm = TRUE),
              queuing = sum(queuing, na.rm = TRUE),
              schools = sum(schools, na.rm = TRUE),
              shared_cars = sum(shared_cars, na.rm = TRUE),
              universities = sum(universities, na.rm = TRUE),
              workplaces = sum(workplaces, na.rm = TRUE))
  
  df_infections_sum <- df_scenario6 %>% 
    group_by(run_number, ratio_population_randomly_tested_daily) %>% 
    summarise(essential_shops = max(people_infected_in_essential_shops, na.rm = TRUE),
              homes = max(people_infected_in_homes, na.rm = TRUE),
              hospitals = max(people_infected_in_hospitals, na.rm = TRUE),
              non_essential_shops = max(people_infected_in_non_essential_shops, na.rm = TRUE),
              private_leisure = max(people_infected_in_private_leisure, na.rm = TRUE),
              public_leisure = max(people_infected_in_public_leisure, na.rm = TRUE),
              pubtrans = max(people_infected_in_pubtrans, na.rm = TRUE),
              queuing = max(people_infected_in_queuing, na.rm = TRUE),
              schools = max(people_infected_in_schools, na.rm = TRUE),
              shared_cars = max(people_infected_in_shared_cars, na.rm = TRUE),
              universities = max(people_infected_in_universities, na.rm = TRUE),
              workplaces = max(people_infected_in_workplaces, na.rm = TRUE))
  
  df_infections_mean <- df_infections_sum %>% 
    group_by(ratio_population_randomly_tested_daily) %>% 
    summarise(essential_shops = mean(essential_shops, na.rm = TRUE),
              homes = mean(homes, na.rm = TRUE),
              hospitals = mean(hospitals, na.rm = TRUE),
              non_essential_shops = mean(non_essential_shops, na.rm = TRUE),
              private_leisure = mean(private_leisure, na.rm = TRUE),
              public_leisure = mean(public_leisure, na.rm = TRUE),
              pubtrans = mean(pubtrans, na.rm = TRUE),
              queuing = mean(queuing, na.rm = TRUE),
              schools = mean(schools, na.rm = TRUE),
              shared_cars = mean(shared_cars, na.rm = TRUE),
              universities = mean(universities, na.rm = TRUE),
              workplaces = mean(workplaces, na.rm = TRUE))
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_inf_gp_accumulated, file=paste(output_dir, "/plot_data_", name, "accumulated.csv", sep=""))
  write.csv(df_infections_mean, file=paste(output_dir, "/plot_data_", name, "mean.csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_acc_gp <- gather(df_inf_gp_accumulated, Gathering_point, measurement, essential_shops:workplaces)
  seg_total_gp <- gather(df_infections_mean, Gathering_point, measurement, essential_shops:workplaces)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, paste("s6R_infections_per_gp_random_tests_bar", sep=""))
  print(plot_ggplot_bar(seg_total_gp))
  dmfPdfClose()
  
  #for(i in unique(seg_acc_gp$ratio_population_randomly_tested_daily)) {
  #  dmfPdfOpen(output_dir, paste("s6r_infections_per_gp_random_tests_", i, "_smooth", sep=""))
  #  print(plot_ggplot_smooth(seg_acc_gp[seg_acc_gp$ratio_population_randomly_tested_daily==i, ], i))
  #  dmfPdfClose()
  #}  
  
  for(i in unique(seg_acc_gp$ratio_population_randomly_tested_daily)) {
    dmfPdfOpen(output_dir, paste("s6R_infections_per_gp_random_tests_", i, sep=""))
    print(plot_ggplot(seg_acc_gp[seg_acc_gp$ratio_population_randomly_tested_daily==i, ], i))
    dmfPdfClose()
  }  
  
  #==================== DIVIDED PLOTS ==========================
  #seg_acc_contacts1 <- gather(accumulatedContacts, variable, measurement, essential_shops:public_leisure)
  #seg_acc_contacts2 <- gather(accumulatedContacts, variable, measurement, pubtrans:workplaces)
  
  #for(i in c(0, 0.6, 0.8, 1)) {
  #  dmfOpenPdf(output_dir, paste("s6_contacts_per_gp1_app", i, sep=""))
  #  print(plot_ggplot(seg_acc_contacts1[seg_acc_contacts1$ratio_of_app_users==i, ], i))
  #  dmfClosePdf()
  #  
  #  dmfOpenPdf(output_dir, paste("s6_contacts_per_gp2_app", i, sep=""))
  #  print(plot_ggplot(seg_acc_contacts2[seg_acc_contacts1$ratio_of_app_users==i, ], i))
  #  dmfClosePdf()
  #}  
  
  #for(i in c(0, 0.6, 0.8, 1)) {
  #  dmfOpenPdf(output_dir, paste("s6_contacts_per_gp1_smooth_app", i, sep=""))
  #  print(plot_ggplot_smooth(seg_acc_contacts1[seg_acc_contacts1$ratio_of_app_users==i, ], i))
  #  dmfClosePdf()
  #  
  #  dmfOpenPdf(output_dir, paste("s6_contacts_per_gp2_smooth_app", i, sep=""))
  #  print(plot_ggplot_smooth(seg_acc_contacts2[seg_acc_contacts2$ratio_of_app_users==i, ], i))
  #  dmfClosePdf()
  #}  
}

#=============================================================
#=================== PLOTTING FUNCTIONS ======================
#=============================================================
plot_ggplot <- function(data_to_plot, daily_random_tests) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement,
               group = Gathering_point,
               fill = Gathering_point), fill=NA) +
    geom_line(aes(col=Gathering_point)) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title=paste("Cummulative infections per location type - daily random tests:", daily_random_tests),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_smooth <- function(data_to_plot, daily_random_tests) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement,
               group = Gathering_point,
               fill = Gathering_point), fill=NA) +
    geom_smooth(aes(col=Gathering_point), span=0.1, se=FALSE) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title=paste("Cummulative infections per location type (Smoothed) - daily random tests:", daily_random_tests),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_bar <- function(data_to_plot) {
  
  #data_to_plot %>%
  ggplot(data = data_to_plot,aes(x = Gathering_point, 
                                 y = measurement,
                                 fill = as.factor(ratio_population_randomly_tested_daily)), fill=NA) +
    geom_bar(stat='identity', position=position_dodge()) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title=paste("Cummulative infections per location type"),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         fill="Daily random tests") +
    gl_plot_guides + gl_plot_theme +
    theme(axis.text.x = element_text(angle = 90))
}