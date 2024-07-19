behaviourPlot3Quarantiners <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #=============================================================
  #====================== QUARANTINERS  ========================
  #=============================================================
  
  df_population_status <- select(subset_df, tick, ce_context_depth, count_officially_quarantiners, count_people_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine) # uninfected, dead_people, immune, believe_immune, 
  df_population_status <- gather(df_population_status, `Population Status`, measurement, count_officially_quarantiners, count_people_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('count_officially_quarantiners'='Officially asked to quarantine',
             'count_people_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine' ='Breaking quarantine'),
    values=c('#2269ee', '#b00300'),
    breaks=c('count_officially_quarantiners','count_people_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine'))
  p <- p + labs(x = "Time (Ticks)", y = "Status of n agents", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Quarantining (", experiment_preset,")", sep=""))
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_quarantining", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}

behaviourPlot3QuarantinersAgeGroup <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #=============================================================
  #====================== QUARANTINERS  ========================
  #=============================================================
  
  df_population_status <- select(subset_df, tick, ce_context_depth, count_officially_quarantiners_children, count_children_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  df_population_status <- gather(df_population_status, `Population Status`, measurement, count_officially_quarantiners_children, count_children_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('count_officially_quarantiners_children'='Children officially asked to quarantine',
             'count_children_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine' ='Children breaking quarantine'),
    values=c('#2269ee', '#b00300'),
    breaks=c('count_officially_quarantiners_children','count_children_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine'))
  p <- p + labs(x = "Time (Ticks)", y = "Status of n children", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Quarantining (", experiment_preset,")", sep=""))
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_quarantining_children", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  df_population_status <- select(subset_df, tick, ce_context_depth, count_officially_quarantiners_students, count_students_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  df_population_status <- gather(df_population_status, `Population Status`, measurement, count_officially_quarantiners_students, count_students_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('count_officially_quarantiners_students'='Students officially asked to quarantine',
             'count_students_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine' ='Students breaking quarantine'),
    values=c('#2269ee', '#b00300'),
    breaks=c('count_officially_quarantiners_students','count_students_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine'))
  p <- p + labs(x = "Time (Ticks)", y = "Status of n students", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Quarantining (", experiment_preset,")", sep=""))
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_quarantining_students", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  df_population_status <- select(subset_df, tick, ce_context_depth, count_officially_quarantiners_workers, count_workers_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  df_population_status <- gather(df_population_status, `Population Status`, measurement, count_officially_quarantiners_workers, count_workers_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('count_officially_quarantiners_workers'='Workers officially asked to quarantine',
             'count_workers_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine' ='Workers breaking quarantine'),
    values=c('#2269ee', '#b00300'),
    breaks=c('count_officially_quarantiners_workers','count_workers_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine'))
  p <- p + labs(x = "Time (Ticks)", y = "Status of n workers", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Quarantining (", experiment_preset,")", sep=""))
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_quarantining_workers", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  df_population_status <- select(subset_df, tick, ce_context_depth, count_officially_quarantiners_retireds, count_retireds_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  df_population_status <- gather(df_population_status, `Population Status`, measurement, count_officially_quarantiners_retireds, count_retireds_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('count_officially_quarantiners_retireds'='Retireds officially asked to quarantine',
             'count_retireds_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine' ='Retireds breaking quarantine'),
    values=c('#2269ee', '#b00300'),
    breaks=c('count_officially_quarantiners_retireds','count_retireds_with_is_officially_asked_to_quarantine_for_plots_and_not_is_in_quarantine'))
  p <- p + labs(x = "Time (Ticks)", y = "Status of n retireds", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Quarantining (", experiment_preset,")", sep=""))
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_quarantining_retireds", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}