# 1_behaviour_plots_1_deliberation_type.R
# subset_df, plot_type, plot_base_name, experiment_preset, gl_limits_x_max
behaviourPlot1DeliberationType <- function() {
  
  print("-- Plot deliberation types ...")
  # Gather (tidyr) and select (dyplr), maybe its not a good idea to mix these?
  df_deliberation_type <- select(subset_df, tick, ce_context_depth, people_alive, `Minimal context`, `Most salient need`, `Compare need levels`, `Normative deliberation`, `Conformity deliberation`, `Full need`)
  # Now I want to for each of the columns `Minimal context` until `Full need` divide it by the people_alive column
  df_deliberation_type <- df_deliberation_type %>% mutate(`Minimal context` = (`Minimal context` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Most salient need` = (`Most salient need` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Compare need levels` = (`Compare need levels` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Normative deliberation` = (`Normative deliberation` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Conformity deliberation` = (`Conformity deliberation` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Full need` = (`Full need` / people_alive) * 100)
  
  df_deliberation_type <- gather(df_deliberation_type, `Deliberation Type`, measurement, `Minimal context`:`Full need`)
  
  # Can remove: p <- ggplot(seg_acc_deliberation_type, aes(tick, measurement)) + geom_boxplot(aes(fill=Status), alpha=0.5)
  
  # col  = for the outline
  # fill = for filling the line (which then makes the whole line black because if col is not specified the outline will be the thing seen)
  p <- ggplot(df_deliberation_type, aes(x = tick, y = measurement, col=`Deliberation Type`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('Minimal context'='Minimal context','Compare need levels'='Compare need levels',
             'Most salient need'='Most salient need','Normative deliberation'='Normative deliberation',
             'Conformity deliberation'='Conformity deliberation','Full need'='Full need'),
    values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'),
    breaks=c('Minimal context','Most salient need','Compare need levels','Normative deliberation','Conformity deliberation','Full need'))
  p <- p + xlab("Ticks") + ylab("% used by agents")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=2, byrow=TRUE))
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_deliberation_type_overall.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - Overall", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_deliberation_type_at_beginning.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(0, 53), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - At Beginning", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_deliberation_type_at_peak_infections.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(84, 138), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - At Peak Infections", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  
  #----------------  THE BAR PLOT: PROPORTIONS  ----------------
  
  # Now I want to plot the proportions of the different types of deliberation
  
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
      labels=c('Minimal context'='Minimal context','Compare need levels'='Compare need levels',
               'Most salient need'='Most salient need','Normative deliberation'='Normative deliberation',
               'Conformity deliberation'='Conformity deliberation','Full need'='Full need'),
      values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'),
      breaks=c('Minimal context','Most salient need','Compare need levels','Normative deliberation','Conformity deliberation','Full need'))
  if (plot_type == "one") { pdf(paste(plot_base_name, "_deliberation_type_bar_plot.pdf", sep=""), width=6, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}