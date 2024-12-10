safe_to_pdf <- function(fname, p_plot, p_w = gl_pdf_width, p_h = gl_pdf_height) {
  
  show(p_plot)
  pdf(fname, width=p_w, height=p_h) 
  show(p_plot)
  dev.off()
}

#================================
# TIME DELIBERATION, GO, FULL ASSOCC
#================================

# Deliberation
plot_time_comparison_deliberation <- function(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY, directory_files, n_experiments_active, plot_type) {
  
  p <- ggplot(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY, aes(x = agents, y = incl_t_ms_mean, 
                                                              color = preset,
                                                              fill = preset,
                                                              linetype = preset)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("Execution time comparison of Deliberation (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents at start",
         y = "Incl execution time (ms)",
         color = "Model",
         fill = "Model",
         linetype = "Model")
  
  p <- p + theme_bw() +
    theme(legend.position="bottom", text = element_text(size=16)) + 
    guides(fill=guide_legend(nrow=1, byrow=TRUE), linetype = guide_legend(nrow = 1, byrow = TRUE))
  
  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_time_comparison_deliberation.pdf", sep=""), p) }
  else { show(p) }
}

# GO
plot_time_comparison_go <- function(df_p_overview_mean_GO, directory_files, n_experiments_active, plot_type) {
  
  p <- ggplot(df_p_overview_mean_GO, aes(x = agents, y = incl_t_ms_mean, 
                                         color = preset,
                                         fill = preset,
                                         linetype = preset)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("Execution time comparison of complete run (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents at start",
         y = "Incl execution time (ms)",
         color = "Model",
         fill = "Model",
         linetype = "Model")
  
  p <- p + theme_bw() +
    theme(legend.position="bottom", text = element_text(size=16)) +
    guides(fill=guide_legend(nrow=1, byrow=TRUE), linetype = guide_legend(nrow = 1, byrow = TRUE))
  
  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_time_comparison_go.pdf", sep=""), p) }
  else { show(p) }
}

# FULL ASSOCC 
plot_time_comparison_full_assocc <- function(df_p_overview_mean_FULL_ASSOCC_DELIBERATION, directory_files, n_experiments_active, plot_type) {
  
  p <- ggplot(df_p_overview_mean_FULL_ASSOCC_DELIBERATION, aes(x = agents, y = incl_t_ms_mean, 
                                                               color = preset,
                                                               fill = preset)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("Execution time comparison of Full ASSOCC Delib (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents at start",
         y = "Incl execution time (ms)",
         color = "Model",
         fill = "Model") +
    theme_minimal()
  
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  
  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_time_comparison_full_assocc.pdf", sep=""), p) }
  else { show(p) }
}

#================================
# DETAILED TIME COMPARISON
#================================

# Time comparison DCSD in detail: Total deliberation, DCSD and Full ASSOCC.
plot_time_comparison_dcsd_detailed <- function(df_p_overview_mean_DCSD_selection, directory_files, n_experiments_active, plot_type) {
  p <- ggplot(df_p_overview_mean_DCSD_selection, aes(x = agents, y = incl_t_ms_mean,
                                                     group = function_name,
                                                     color = function_name,
                                                     fill = function_name,
                                                     linetype = function_name)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("DCSD execution time in depth (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents",
         y = "Incl execution time (ms)",
         color = "Type",
         fill = "Type",
         linetype = "Type")
  
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) +
    guides(fill=guide_legend(nrow=2, byrow=TRUE), linetype = guide_legend(nrow = 1, byrow = TRUE))
  
  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_time_comparison_dcsd_in_detail.pdf", sep=""), p) }
  else { show(p) }
}

# Time comparison non-deliberation and delibreration within Original assocc
plot_time_comparison_original_assocc <- function(df_time_original_assocc, directory_files, n_experiments_active, plot_type) {
  p <- ggplot(df_time_original_assocc, aes(x = Agents, y = incl_t_ms_mean, 
                                           color = time_type,
                                           fill = time_type,
                                           linetype = time_type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("Original ASSOCC - Time Comparison (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents at start",
         y = "Incl execution time (ms)",
         color = "Time Type",
         fill = "Time Type",
         linetype = "Time Type") +
    theme_minimal()
  
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))

  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_time_comparison_original_assocc.pdf", sep=""), p) }
  else { show(p) }
}

# Time comparison non-deliberation and delibreration within DCSD assocc
plot_time_comparison_dcsd_assocc <- function(df_time_dcsd_assocc, directory_files, n_experiments_active, plot_type) {
  
  p <- ggplot(df_time_dcsd_assocc, aes(x = Agents, y = incl_t_ms_mean, 
                                       color = time_type,
                                       fill = time_type,
                                       linetype = time_type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("DCSD ASSOCC - Time Comparison (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents at start",
         y = "Incl execution time (ms)",
         color = "Time Type",
         fill = "Time Type",
         linetype = "Time Type") +
    theme_minimal()
  
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))

  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_time_comparison_dcsd_assocc.pdf", sep=""), p) }
  else { show(p) }
}

# Total execution time
plot_estimated_total_execution_time <- function(df_incl_t_ms_mean_all, directory_files, n_experiments_active, plot_type) {
  
  p <- ggplot(df_incl_t_ms_mean_all, aes(x = Agents, y = `Total Time New`, 
                                         color = Preset,
                                         fill = Preset,
                                         linetype = Preset)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("Estimated Total Execution Time (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents at start",
         y = "Incl t ms",
         color = "Model",
         fill = "Model",
         linetype = "Model") +
    theme_minimal()
  
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  
  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_estimated_total_execution_time.pdf", sep=""), p) }
  else { show(p) }
}

# Speed-Up plot normal 
plot_possible_speed_up_normal <- function(df_incl_t_ms_mean_all, directory_files, n_experiments_active, plot_type) {
  
  p <- ggplot(df_incl_t_ms_mean_all, aes(x = Agents, y = `Required Speed-Up`, 
                                         color = Preset,
                                         fill = Preset,
                                         linetype = Preset)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("Possible Speed-Up of Non-Deliberation (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents at start",
         y = "Speed-up To Equalise",
         color = "Model",
         fill = "Model",
         linetype = "Model") +
    theme_minimal() + ylim(0, 150)
  
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))

  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_non_deliberation_speed_up.pdf", sep=""), p) }
  else { show(p) }
}

# Speed-Up plot Original ASSOCC
plot_possible_speed_up_original_assocc <- function(df_incl_t_ms_mean_all, directory_files, n_experiments_active, plot_type) {
  
  p <- ggplot(df_incl_t_ms_mean_all, aes(x = Agents, y = `Required Speed-Up Original`, 
                                         color = Preset,
                                         fill = Preset,
                                         linetype = Preset)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5, show.legend = FALSE) +
    labs(title = paste("Possible Speed-Up of Original ASSOCC Non-Deliberation (n = ", n_experiments_active, ")", sep = ""),
         x = "Agents at start",
         y = "Speed-up To Equalise",
         color = "Model",
         fill = "Model",
         linetype = "Model") +
    theme_minimal() + ylim(0, 150)
  
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  
  if (plot_type == "one") { safe_to_pdf(paste("plot_", directory_files, "_profiler_non_deliberation_speed_up_original_assocc.pdf", sep=""), p) }
  else { show(p) }
}