# 1_behaviour_plots_1_deliberation_type.R
# subset_df, plot_type, plot_base_name, experiment_preset, gl_limits_x_max
behaviourPlot1DeliberationType <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  # Create the dataframe for plotting
  df_deliberation_type <- select(subset_df, tick, ce_context_depth, people_alive, `Minimal context perc`, `Most salient need perc`, `Compare need levels perc`, `Normative deliberation perc`, `Conformity deliberation perc`, `Full need perc`)
  df_deliberation_type <- gather(df_deliberation_type, `Deliberation Type`, measurement, `Minimal context perc`:`Full need perc`)

  # col  = for the outline
  # fill = for filling the line (which then makes the whole line black because if col is not specified the outline will be the thing seen)
  p <- ggplot(df_deliberation_type, aes(x = tick, y = measurement, col=`Deliberation Type`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('Minimal context perc'='Minimal context','Compare need levels perc'='Compare need levels',
             'Most salient need perc'='Most salient need','Normative deliberation perc'='Normative deliberation',
             'Conformity deliberation perc'='Conformity deliberation','Full need perc'='Full need'),
    values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'),
    breaks=c('Minimal context perc','Most salient need perc','Compare need levels perc','Normative deliberation perc','Conformity deliberation perc','Full need perc'))
  p <- p + xlab("Ticks") + ylab("% used by agents")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=2, byrow=TRUE))
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_deliberation_type_overall", sep="")) }
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - Overall", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_deliberation_type_at_beginning", sep="")) }
  p <- p + coord_cartesian(xlim = c(0, 53), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - At Beginning", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_deliberation_type_at_peak_infections", sep="")) }
  p <- p + coord_cartesian(xlim = c(84, 138), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - At Peak Infections", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}

behaviourPlot1DeliberationTypeConformity <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  # Create the dataframe for plotting
  df_deliberation_type <- select(subset_df, tick, ce_context_depth, people_alive, `Minimal context perc`, `Most salient need perc`, `Compare need levels perc`, `Normative deliberation perc`, `Conformity deliberation perc`, `Full need perc`)
  df_deliberation_type <- gather(df_deliberation_type, `Deliberation Type`, measurement, `Minimal context perc`:`Full need perc`)
  
  # col  = for the outline
  # fill = for filling the line (which then makes the whole line black because if col is not specified the outline will be the thing seen)
  p <- ggplot(df_deliberation_type, aes(x = tick, y = measurement, col=`Deliberation Type`)) + geom_line() + gghighlight::gghighlight(`Deliberation Type` == "Conformity deliberation perc", use_direct_label = FALSE)
  p <- p + scale_colour_manual(
    labels=c('Minimal context perc'='Minimal context','Compare need levels perc'='Compare need levels',
             'Most salient need perc'='Most salient need','Normative deliberation perc'='Normative deliberation',
             'Conformity deliberation perc'='Conformity deliberation','Full need perc'='Full need'),
    values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'),
    breaks=c('Minimal context perc','Most salient need perc','Compare need levels perc','Normative deliberation perc','Conformity deliberation perc','Full need perc'))
  p <- p + xlab("Ticks") + ylab("% used by agents")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=2, byrow=TRUE))
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_deliberation_type_conformity", sep="")) }
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - Overall", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}

behaviourPlot1DeliberationTypeBar <- function(plot_specific_f_name) {
  
  #----------------  THE BAR PLOT: PROPORTIONS  ----------------
  # This bar plot doesn't properly represent how much is used of the deliberation type.
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  # Create the dataframe for plotting
  df_deliberation_type <- select(subset_df, tick, ce_context_depth, people_alive, `Minimal context perc`, `Most salient need perc`, `Compare need levels perc`, `Normative deliberation perc`, `Conformity deliberation perc`, `Full need perc`)
  df_deliberation_type <- gather(df_deliberation_type, `Deliberation Type`, measurement, `Minimal context perc`:`Full need perc`)
  
  # I want to sum the columns `Minimal context` until `Full need` in the df_deliberation_type dataframe
  df_deliberation_type_sum <- df_deliberation_type %>% group_by(`Deliberation Type`) %>% summarise_all(sum)
  df_deliberation_type_sum$`Deliberation Type` <- factor(df_deliberation_type_sum$`Deliberation Type`, levels = c(df_deliberation_type_sum$`Deliberation Type`))
  #levels = c(3, 5, 6, 1, 2, 4))
  
  # Now i want an additional column for df_deliberation_type_sum that is the sum of the single measurement column
  df_deliberation_type_sum$measurement_sum <- rep(sum(df_deliberation_type_sum$measurement), nrow(df_deliberation_type_sum))
  df_deliberation_type_sum$deliberation_type_proportions <- df_deliberation_type_sum$measurement / df_deliberation_type_sum$measurement_sum * 100
  
  # Now i want to plot df_deliberation_type_sum in a bar plot
  p <- ggplot(df_deliberation_type_sum, aes(x = `Deliberation Type`, y = deliberation_type_proportions, fill = `Deliberation Type`)) +
    geom_bar(stat="identity") + theme_bw() + ylab("Overall % used by agents") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
    scale_fill_manual(
      labels=c('Minimal context perc'='Minimal context','Compare need levels perc'='Compare need levels',
               'Most salient need perc'='Most salient need','Normative deliberation perc'='Normative deliberation',
               'Conformity deliberation perc'='Conformity deliberation','Full need perc'='Full need'),
      values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'),
      breaks=c('Minimal context perc','Most salient need perc','Compare need levels perc','Normative deliberation perc','Conformity deliberation perc','Full need perc'))
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_deliberation_type_bar_plot", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}