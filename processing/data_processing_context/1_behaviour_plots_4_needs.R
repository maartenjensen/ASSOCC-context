#=============================================================
#======================= PLOT NEEDS  =========================
#=============================================================

behaviourPlot4Needs <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #========== Needs and need level ===========
  df_needs <- select(subset_df, tick, ce_context_depth, autonomy, belonging, compliance, conformity, financial_stability,
                     financial_survival, food_safety, health, leisure, luxury, risk_avoidance, sleep)
  df_needs <- gather(df_needs, `Need Type`, measurement, autonomy:sleep)
  
  p <- ggplot(df_needs, aes(x = tick, y = measurement, col=`Need Type`))
  p <- p + scale_colour_manual(
    labels=c('autonomy'='AUT', 'belonging'='BEL', 'compliance'='COM', 'conformity'='CON',
             'financial_stability'='FST', 'financial_survival'='FSU', 'food_safety'='FOO',
             'health'='HEA', 'leisure'='LEI', 'luxury'='LUX', 'risk_avoidance'='RIS', 'sleep'='SLE'),
    values=c('#f16a15','#000000','#9d6e48','#43a0a0',
             '#E69F00','#881556','#1A4B09',
             '#d73229','#f2ccd5','#80e389','#345da9','#8d8d8d'),
    breaks=c('autonomy', 'belonging', 'compliance', 'conformity',
             'financial_stability', 'financial_survival', 'food_safety',
             'health', 'leisure', 'luxury', 'risk_avoidance', 'sleep'))
  p <- p + xlab("Ticks") + ylab("Need Level") + labs(col = "Need")
  p <- p + theme_bw() + theme(text = element_text(size=16))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1)) + labs(title=paste("Need Levels (", experiment_preset,") - Overall", sep=""))  
  p_smooth <- p + geom_smooth()
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_needs_overall.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_needs_overall_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}

behaviourPlot4NeedsLeisureAndShopping <- function(plot_specific_f_name) {
  
  # + gghighlight::gghighlight(`Need Type` == "leisure")
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #========== Needs and need level ===========
  df_needs <- select(subset_df, tick, ce_context_depth, food_safety, leisure, luxury)
  df_needs <- gather(df_needs, `Need Type`, measurement, food_safety:luxury)
  
  p <- ggplot(df_needs, aes(x = tick, y = measurement, col=`Need Type`))
  p <- p + scale_colour_manual(
    labels=c('food_safety'='FOO','leisure'='LEI','luxury'='LUX'),
    values=c('#1A4B09','#f16a15','#881556'),
    breaks=c('food_safety','leisure', 'luxury'))
  p <- p + xlab("Ticks") + ylab("Need Level") + labs(col = "Need")
  p <- p + theme_bw() + theme(text = element_text(size=16))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1)) + labs(title=paste("Need Levels (", experiment_preset,") - Overall", sep=""))
  p_smooth <- p + geom_smooth()
  p <- p + geom_line() 
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_needs_overall.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_needs_overall_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}