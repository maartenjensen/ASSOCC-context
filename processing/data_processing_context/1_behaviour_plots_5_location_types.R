behaviourPlot5LocationTypes <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #=============================================================
  #==================== LOCATION TYPES  ========================
  #=============================================================
  
  df_location_types <- select(subset_df, tick, ce_context_depth, at_essential_shops, at_homes, at_non_essential_shops,
                              at_private_leisure, at_public_leisure, at_schools, at_universities, at_workplaces, at_treatment)
  df_location_types <- gather(df_location_types, `Location Type`, measurement, at_essential_shops:at_treatment)
  
  p <- ggplot(df_location_types, aes(x = tick, y = measurement, col=`Location Type`))
  p <- p + scale_colour_manual(
    labels=c('at_essential_shops'='Essential Shops', 'at_homes'='Homes', 'at_non_essential_shops'='Non-essential Shops',
             'at_private_leisure'='Private Leisure', 'at_public_leisure'='Public Leisure', 'at_schools'='Schools',
             'at_universities'='Universities', 'at_workplaces'='Workplaces', 'at_treatment'='Treatment'),
    values=c('#881556','#80e389','#f2ccd5',
             '#f16a15','#d73229','#9d6e48', 
             '#E69F00','#345da9','#8d8d8d'),
    breaks=c('at_essential_shops', 'at_homes', 'at_non_essential_shops',
             'at_private_leisure', 'at_public_leisure', 'at_schools',
             'at_universities', 'at_workplaces', 'at_treatment'))
  p <- p + xlab("Ticks") + ylab("Agents at Location Type") + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Location Types (", experiment_preset,") - Overall", sep=""))  
  p_smooth <- p + geom_smooth()
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_location_types", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_location_types_smooth", sep="")) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}