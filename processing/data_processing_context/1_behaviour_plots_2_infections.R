# 1_behaviour_plots_2_infections.R
behaviourPlot2Infections <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #=============================================================
  #==================== PLOT INFECTIONS  =======================
  #=============================================================
  
  df_population_status <- select(subset_df, tick, ce_context_depth, uninfected, infected, believe_infected, dead_people, healthy) #, immune, believe_immune, 
  df_population_status <- gather(df_population_status, `Population Status`, measurement, uninfected:healthy)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('uninfected'='Uninfected', 'infected'='Infected',
             'believe_infected' ='Believe Infected', 'dead_people' ='Dead People', 
             'healthy' ='Healthy'),
    values=c('#afd16f', '#b00300', '#ff7c73', '#000000', '#3c9e34'),
    breaks=c('uninfected', 'infected','believe_infected','dead_people','healthy'))
  p <- p + labs(x = "Ticks", y = "Status of n agents", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Population Status (", experiment_preset,") - Overall", sep=""))
  if (plot_type == "one") { pdf(paste(plot_base_name, "_population_status_overall.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  #p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=2, byrow=TRUE))
  print("-- ... finished!")
}

behaviourPlot2InfectionsBelieveInfected <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #==================== Minimal infection curve ===================
  df_population_status <- select(subset_df, tick, ce_context_depth, infected, believe_infected, healthy) # uninfected, dead_people, immune, believe_immune, 
  df_population_status <- gather(df_population_status, `Population Status`, measurement, infected:healthy)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('infected'='Infected',
             'believe_infected' ='Believe Infected', 
             'healthy' ='Healthy'),
    values=c('#ff7c73', '#b00300', '#afd16f'),
    breaks=c('infected','believe_infected','healthy'))
  p <- p + labs(x = "Ticks", y = "Status of n agents", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Population Status (", experiment_preset,")", sep=""))
  p <- p + geom_line(data = filter(df_population_status, `Population Status` == "believe_infected"), size = 2)
  if (plot_type == "one") { pdf(paste(plot_base_name, "_population_status_simplified.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}