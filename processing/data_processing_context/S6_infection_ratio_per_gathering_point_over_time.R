#=============================================================
#====================== MAIN FUNCTION ========================
#=============================================================

plotS6InfectionRatioGP <- function(df_scenario6, output_dir, one_plot) {
  
  name = "s6_infection_ratio_gp"
  
  #-------------------------------------------------------------
  #-------------------- DATA MANIPULATION ----------------------
  #-------------------------------------------------------------
  print(paste(name, " performing data manipulation", sep=""))
  df_inf_gp <- df_scenario6 %>% 
    group_by(tick, ratio_of_app_users) %>% 
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
  
  # Mean for every day (combine the four ticks), since it is accumulated
  df_inf_gp_accumulated <- df_inf_gp %>% 
    group_by(day, ratio_of_app_users) %>% 
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
  
  df_infections_sum <- df_scenario6 %>% 
    group_by(run_number, ratio_of_app_users) %>% 
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
    group_by(ratio_of_app_users) %>% 
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
  
  # Remove day 375, since it is not a complete day (only measured one tick)
  #df_inf_gp_accumulated <- filter(df_inf_gp_accumulated, day < 375)
  
  
  print(paste(name, " writing CSV", sep=""))
  write.csv(df_inf_gp_accumulated, file=paste(output_dir, "/plot_data_", name, "accumulated.csv", sep=""))
  write.csv(df_infections_mean, file=paste(output_dir, "/plot_data_", name, "mean.csv", sep=""))
  
  #-------------------------------------------------------------
  #------------------------- Plotting --------------------------
  #-------------------------------------------------------------
  seg_acc_gp <- gather(df_inf_gp_accumulated, location_type, measurement, essential_shops:workplaces)
  seg_total_gp <- gather(df_infections_mean, location_type, measurement, essential_shops:workplaces)
  
  print(paste(name, " making plots", sep=""))
  
  dmfPdfOpen(output_dir, paste("s6_infections_per_gp_random_tests_bar", sep=""))
  print(plot_ggplot_bar(seg_total_gp))
  dmfPdfClose()
  
  #for(i in unique(seg_acc_gp$ratio_of_app_users)) {
  #  dmfPdfOpen(output_dir, paste("s6_infections_per_gp_random_tests_", i, "_smooth", sep=""))
  #  print(plot_ggplot_smooth(seg_acc_gp[seg_acc_gp$ratio_of_app_users==i, ], i))
  #  dmfPdfClose()
  #}  
  
  for(i in unique(seg_acc_gp$ratio_of_app_users)) {
    dmfPdfOpen(output_dir, paste("s6_infections_per_gp_random_tests_", i, sep=""))
    print(plot_ggplot(seg_acc_gp[seg_acc_gp$ratio_of_app_users==i, ], i))
    dmfPdfClose()
  }  
  
  seg_acc_gp_1 = subset(seg_acc_gp, location_type!="homes" & location_type!="schools" & location_type!="pubtrans" &
                        location_type!="non_essential_shops" & location_type!="universities" & location_type!="shared_cars")
  #seg_acc_gp_1$location_type[seg_acc_gp_1$location_type == "essential_shops"] <- "essential_shops         "
  
  seg_acc_gp_2 = subset(seg_acc_gp, location_type!="essential_shops" & location_type!="hospitals" & location_type!="private_leisure" &
                          location_type!="public_leisure" & location_type!="queuing" & location_type!="workplaces")
  
  dmfPdfOpen(output_dir, paste("s6_infections_per_gp_random_tests_facet_wrap_a", sep=""), p_width=9, p_height=5)
  print(plot_ggplot_facet_extra_a(seg_acc_gp_2, "Cummulative infections per location type dependent on app usage ratio - a"))
  dmfPdfClose()
  
  dmfPdfOpen(output_dir, paste("s6_infections_per_gp_random_tests_facet_wrap_b", sep=""), p_width=9, p_height=5)
  print(plot_ggplot_facet_extra_b(seg_acc_gp_1, "Cummulative infections per location type dependent on app usage ratio - b"))
  dmfPdfClose()
  
  
  
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

plot_ggplot_facet_extra_a <- function(data_to_plot, pTitle) {
  
  ggplot(data_to_plot) +
    geom_line(aes(x = day, y = measurement,
                  color=location_type, linetype = location_type)) + facet_wrap(~ratio_of_app_users) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title=pTitle) +
    theme_bw() + theme(legend.position="bottom", legend.text = element_text(size = rel(1 * multiplier)), legend.title = element_text(size = rel(1.2 * multiplier))) +# + theme(legend.position="right") +
    guides(linetype = guide_legend(nrow=1)) +
    scale_linetype_manual(name = "Locations", values = c(1,2,1,1,2,2)) +
    #scale_color_manual(name = "Locations", values = rep(c(gl_col_2, gl_col_3, gl_col_4), 2)) #gl_col_1
    scale_color_manual(name = "Locations", values = c(gl_col_retired, gl_col_retired, gl_col_worker, gl_col_student, gl_col_worker, gl_col_student))
}

plot_ggplot_facet_extra_b <- function(data_to_plot, pTitle) {
  
  ggplot(data_to_plot) +
    geom_line(aes(x = day, y = measurement,
                  color=location_type, linetype = location_type)) + facet_wrap(~ratio_of_app_users) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title=pTitle) +
    theme_bw() + theme(legend.position="bottom", legend.text = element_text(size = rel(1 * multiplier)), legend.title = element_text(size = rel(1.2 * multiplier))) +# + theme(legend.position="right") +
    guides(linetype = guide_legend(nrow=1)) +
    scale_linetype_manual(name = "Locations", values = c(1,2,1,2,1,2)) +
    #scale_color_manual(name = "Locations", values = rep(c(gl_col_2, gl_col_3, gl_col_4), 2)) #gl_col_1
    scale_color_manual(name = "Locations", values = c(gl_col_retired, gl_col_retired, gl_col_student, gl_col_student, gl_col_worker, gl_col_worker))
}

plot_ggplot <- function(data_to_plot, app_usage) {
  
    ggplot(data_to_plot) +
  #fill = location_type, linetype = location_type), fill=NA) +
    geom_line(aes(x = day, 
                  y = measurement,
                  color=location_type, linetype = location_type)) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title=paste("Cummulative infections per location type - app usage ratio:", app_usage),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_theme +
    scale_linetype_manual(name = "Locations", values = c(1,1,1,1,2,2,2,2,4,4,4,4)) +
    scale_color_manual(name = "Locations", values = rep(c(gl_col_1, gl_col_2, gl_col_3, gl_col_4), 3)) 
}

plot_ggplot_facet <- function(data_to_plot) {
  
  ggplot(data_to_plot) +
    geom_line(aes(x = day, y = measurement,
          color=location_type, linetype = location_type)) + facet_wrap(~ratio_of_app_users) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title="Cummulative infections per location type dependent on app usage ratio",
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_theme +
      scale_linetype_manual(name = "Locations", values = c(1,1,1,1,2,2,2,2,4,4,4,4)) +
      scale_color_manual(name = "Locations", values = rep(c(gl_col_1, gl_col_2, gl_col_3, gl_col_4), 3)) 
}

plot_ggplot_smooth <- function(data_to_plot, app_usage) {
  
  data_to_plot %>%
    ggplot(aes(x = day, 
               y = measurement,
               group = location_type,
               fill = location_type), fill=NA) +
    geom_smooth(aes(col=location_type), span=0.1, se=FALSE) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title=paste("Cummulative infections per location type (Smoothed) - app usage ratio:", app_usage),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides + gl_plot_theme
}

plot_ggplot_bar <- function(data_to_plot) {
  
  #data_to_plot %>%
  ggplot(data = data_to_plot,aes(x = location_type, 
                                 y = measurement,
                                 fill = as.factor(ratio_of_app_users)), fill=NA) +
    geom_bar(stat='identity', position=position_dodge()) +
    xlab("Days") +
    ylab("Number of infections") +
    labs(title=paste("Cummulative infections per location type"),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         fill="App users ratio") +
    gl_plot_guides_side + gl_plot_theme_side +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_viridis_d(name=gl_plot_variable_name)
}