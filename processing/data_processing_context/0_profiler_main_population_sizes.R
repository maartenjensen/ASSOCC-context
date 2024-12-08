#-------------------------------
#---  INITIALISE LIBRARIES   ---
#-------------------------------

# Install the libraries
#install.packages()
#install.packages("viridis")

#% Make the libraries the same as in 1_behaviour_main
# Open the libraries
if (!exists("libraries_loaded") || getwd() == "C:/Users/maart/OneDrive/Documenten")
{
  library(tidyverse)
  library(ggplot2)
  library(sjmisc)
  library(readr)
  library(viridis)
  
  #first empty working memory 
  rm(list=ls())
  libraries_loaded = TRUE
} else {
  #first empty working memory 
  rm(list=ls()) 
  libraries_loaded = TRUE
}



#-------------------------------
#---     INITIALIZATION      ---
#-------------------------------

#-   GENERAL PARAMETERS   -
options(scipen=100) # This is for the profiler results

gl_pdf_width  = 10
gl_pdf_height = 7

# One of: "none", "one", "all"
plot_type <- "none"
plot_type <- "one" 
#plot_type <- "all"

directory_r <- "D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context"
directory_files <- "2024_09_23_scalability_hospital_fix"
directory_files <- "2024_12_07_scalability_wh_autonomy"

#--- WORKSPACE AND DIRECTORY ---
#-   CHANGE DIRECTORY   -
setwd(paste(directory_r, directory_files, sep="/"))
getwd()

source("../0_profiler_support_population_sizes.R")

# C = context depth, H = households, A = action space, R = random seed

n_experiments_active = 1
random_seeds = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")[1:n_experiments_active]

if (directory_files == "2024_12_07_scalability_wh_autonomy")
{
  filenames_profiler <- retrieve_filenames_profiler(c("0.1 Original ASSOCC", "5.1 DCSD-5-optimisation"),
                                                    c("350", "700", "1400", "2100", "2800", "3500"),
                                                    c("6"),
                                                    random_seeds)
}


#--------------------------------------
#---    LOAD ALL PROFILER DATA      ---
#--------------------------------------
p_filepath_workspace <- paste(directory_r, directory_files, sep="/")
p_filenames_profiler <- filenames_profiler

df_profiler = profilerLoadData(paste(directory_r, directory_files, sep="/"), filenames_profiler)
df_p_overview = profilerLoadSpecificData(df_profiler, c("GO", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR", "CONTEXT-SELECT-ACTIVITY", "SELECT-ACTIVITY"))
# Remove the "CONTEXT-DELIBERATION-SELECT-ACTIVITY"
df_p_overview <- df_p_overview[!grepl("CONTEXT-DELIBERATION-SELECT-ACTIVITY", df_p_overview$function_name), ]
# Rename
df_p_overview$function_name[df_p_overview$function_name == "SELECT-ACTIVITY"] <- "CONTEXT-SELECT-ACTIVITY"
# Rename the "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR" to "FULL ASSOCC DELIBERATION"
df_p_overview$function_name[df_p_overview$function_name == "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR"] <- "FULL ASSOCC DELIBERATION"

# There are some double entries in the dataframe, so I need to remove them
df_p_overview <- df_p_overview[!duplicated(df_p_overview), ]

# The profiler function that summarizes all the important results
profilerSummarize(df_profiler, df_p_overview)


#--------------------------------------
#---      ADD SPECIFIC ROWS         ---
#--------------------------------------

# for df_p_overview I want to divide incl_t_ms by the number of calls
df_p_overview$incl_t_ms_per_call <- df_p_overview$incl_t_ms / df_p_overview$calls


# in df_p_overview I want to add another column for the number of agents
# for each number of households there are specific number of agents, see below where households = agents, households = agents, ...
# 350 = 1004, 700 = 2008, 1400 = 4016, 2100 = 6016, 2800 = 8024, 3500 = 10028
df_p_overview$agents <- c(1004, 2008, 4016, 6016, 8024, 10028)[match(df_p_overview$households, c(350, 700, 1400, 2100, 2800, 3500))]



#----------------------------------------
# Calculate the mean and sd of the data
#----------------------------------------

# In the following functions and plots the households column is exchanged for the agents column
df_p_overview_mean <- df_p_overview %>% 
  group_by(preset, function_name, agents) %>% 
  summarise(calls_mean = mean(calls),
            calls_sd = sd(calls),
            incl_t_ms_mean = mean(incl_t_ms),
            incl_t_ms_sd = sd(incl_t_ms),
            excl_t_ms_mean = mean(excl_t_ms),
            excl_t_ms_sd = sd(excl_t_ms),
            excl_calls_mean = mean(excl_calls),
            excl_calls_sd = sd(excl_calls))

# --- NOTE!! ---
# if there is only n_experiments_active, df_p_overview_mean

#--------------------------------------
# PLOTTING
#--------------------------------------

#==============================================
#==============================================
# SELECT ACTIVITY EXECUTION TIME
#==============================================
#==============================================


#========= SELECT ACTIVITY =========
df_p_overview_mean_CONTEXT_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]

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

#==============================================
# PLOTTING 11111111111111111111
#==============================================

show(p)

#p <- p + coord_cartesian(xlim = c(0, 240), ylim = c(0, 1020)) # + labs(title=paste("Infections comparison (With infections)", sep=""))
if (plot_type == "one") 
{ 
  pdf(paste("plot_", directory_files, "_profiler_time_comparison_deliberation.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) 
  show(p)
  dev.off()
}

#==============================================
# DATA PRINTING 1111111111111111111
#==============================================

agents = df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$agents[1:6]
incl_t_original_assocc = df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean[1:6]
incl_t_dcsd_assocc = df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean[7:12]

df_speed_up <- data.frame(agents, incl_t_original_assocc, incl_t_dcsd_assocc)
df_speed_up$speed_up <- df_speed_up$incl_t_original_assocc/df_speed_up$incl_t_dcsd_assocc



#\begin{table}[!ht]
#\begin{tabular}{ll|lll}
#\textbf{Agents} & \textbf{Households} & \textbf{\begin{tabular}[c]{@{}l@{}}Original\\ ASSOCC\end{tabular}} & \textbf{\begin{tabular}[c]{@{}l@{}}DCSD\\ ASSOCC\end{tabular}} & \textbf{Speed-up factor} \\ \hline
#\textbf{\begin{tabular}[c]{@{}l@{}}DCSD\\ ASSOCC\end{tabular}} & \textbf{Speed-up factor} \\ \hline
for (i in 1:6) {
  cat(df_speed_up$agents[i], "&", round(df_speed_up$incl_t_original_assocc[i], digits = 0), "ms &",
      round(df_speed_up$incl_t_dcsd_assocc[i], digits = 0), "ms &",  round(df_speed_up$speed_up[i], digits = 1), "\\\\ \n") 
}

#\end{tabular}
#\caption{\textcolor{red}{Execution time with speed-up factor - Original VS DCSD}}
#\label{tab:results_scalability_n_agents_deliberation_comparison_and_speed_up}




#==============================================
#==============================================
# GO FUNCTION EXECUTION TIME
#==============================================
#==============================================

#========= GO =========
df_p_overview_mean_GO <- df_p_overview_mean[df_p_overview_mean$function_name == "GO", ]

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

#==============================================
# PLOTTING 3333333333333333333333
#==============================================

show(p)

#p <- p + coord_cartesian(xlim = c(0, 240), ylim = c(0, 1020)) # + labs(title=paste("Infections comparison (With infections)", sep=""))
if (plot_type == "one") 
{ 
  pdf(paste("plot_", directory_files, "_profiler_time_comparison_go.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) 
  show(p)
  dev.off()
}

#========= FULL ASSOCC DELIBERATION =========
df_p_overview_mean_FULL_ASSOCC_DELIBERATION <- df_p_overview_mean[df_p_overview_mean$function_name == "FULL ASSOCC DELIBERATION", ]

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

#p <- p + coord_cartesian(xlim = c(0, 240), ylim = c(0, 1020)) # + labs(title=paste("Infections comparison (With infections)", sep=""))
#if (plot_type == "one") { behaviourEnablePdf(paste("plot_", directory_files, "_infections_comparison_normal_n_", n_samples, sep="")) }
#show(p)
#if (plot_type == "one") { dev.off() }

#==============================================
# DATA PRINTING 333333333333333333
#==============================================
df_p_overview_mean_FULL_ASSOCC_DELIBERATION[, c(1,2,3,6)]
df_p_overview_mean_GO[, c(1,2,3,6)]



#==============================================
#==============================================
# INDEPTH ANALYSIS OF DCSD EXECUTION TIME
#==============================================
#==============================================

# From df_p_overview_mean remove all rows with preset 0.1 Original ASSOCC
df_p_overview_mean_DCSD <- df_p_overview_mean[df_p_overview_mean$preset != "0.1 Original ASSOCC", ]
# Remove GO from df_p_overview_mean_DCSD
df_p_overview_mean_DCSD <- df_p_overview_mean_DCSD[df_p_overview_mean_DCSD$function_name != "GO", ]

# I just want to select from df_p_overview_mean_DCSD, the preset, function_name, agents, and incl_t_ms_mean
df_p_overview_mean_DCSD_selection <- df_p_overview_mean_DCSD %>% 
  select(preset, function_name, agents, incl_t_ms_mean)

preset = c()
function_name = c()
agents = c()
incl_t_ms_mean = c()

for (i in 1:6) {
  
  preset = c(preset, df_p_overview_mean_DCSD_selection$preset[i])
  function_name = c(function_name, "DCSD Time")
  agents = c(agents, df_p_overview_mean_DCSD_selection$agents[i])
  incl_t_ms_mean = c(incl_t_ms_mean, df_p_overview_mean_DCSD_selection$incl_t_ms_mean[i] - df_p_overview_mean_DCSD_selection$incl_t_ms_mean[i + 6])
}

df_p_overview_mean_DCSD_temporary = data.frame(preset, function_name, agents, incl_t_ms_mean)

df_p_overview_mean_DCSD_selection <- rbind(df_p_overview_mean_DCSD_selection, df_p_overview_mean_DCSD_temporary)

#==============================================
# PLOTTING 22222222222222222222
#==============================================
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

# Actual plotting
show(p)

if (plot_type == "one") 
{
  pdf(paste("plot_", directory_files, "_profiler_time_comparison_dcsd_in_detail.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) 
  show(p)
  dev.off()
}


#==============================================
# DATA PRINTING 22222222222222222222
#==============================================
df_p_overview_mean_DCSD_selection





#==============================================
# PLOTTING 4444444444444444444: Total, Deliberation and Non-Deliberation Time
#==============================================

# Data preparations
round_i = 0
v_incl_t_ms_mean_non_deliberation = df_p_overview_mean_GO$incl_t_ms_mean - df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean

df_incl_t_ms_mean_all <- data.frame(df_p_overview_mean_GO$agents, df_p_overview_mean_GO$preset, 
                                    round(df_p_overview_mean_GO$incl_t_ms_mean, round_i),
                                    round(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean, round_i),
                                    round(v_incl_t_ms_mean_non_deliberation, round_i))

colnames(df_incl_t_ms_mean_all) = c("Agents", "Preset", "Total Time", "Deliberation Time", "Non-Deliberation Time")

# plot for Original ASSOCC
df_time_original_assocc <- gather(df_incl_t_ms_mean_all[1:6, c(1,2,4,5)], time_type, incl_t_ms_mean, `Deliberation Time`:`Non-Deliberation Time`)

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
p

if (plot_type == "one") 
{
  pdf(paste("plot_", directory_files, "_profiler_time_comparison_original_assocc.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) 
  show(p)
  dev.off()
}

# plot for DCSD ASSOCC
df_time_dcsd_assocc <- gather(df_incl_t_ms_mean_all[7:12, c(1,2,4,5)], time_type, incl_t_ms_mean, `Deliberation Time`:`Non-Deliberation Time`)

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
p

if (plot_type == "one") 
{
  pdf(paste("plot_", directory_files, "_profiler_time_comparison_dcsd_assocc.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) 
  show(p)
  dev.off()
}


#==============================================
# PLOTTING 55555555555555: Plot for required speed-up of non-deliberation time to be equal to deliberation time
#==============================================

# Calculate the speed up factor
df_incl_t_ms_mean_all$`Required Speed-Up` = round(df_incl_t_ms_mean_all$`Non-Deliberation Time`/df_incl_t_ms_mean_all$`Deliberation Time`, 1)

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
p

if (plot_type == "one") 
{
  pdf(paste("plot_", directory_files, "_profiler_non_deliberation_speed_up.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) 
  show(p)
  dev.off()
}

# From the figures above it can be seen that the Non-Deliberation time of Original ASSOCC
# is about twice as much as the Non-Deliberation of DCSD ASSOCC. Thus what if we take the
# Original ASSOCC Non-Deliberation time and calculate how much the speed-up factor required is then.

# Data preparation
df_incl_t_ms_mean_all$`Required Speed-Up Original` = df_incl_t_ms_mean_all$`Required Speed-Up`
df_incl_t_ms_mean_all$`Required Speed-Up Original`[7:12] = round(df_incl_t_ms_mean_all$`Non-Deliberation Time`[1:6]/df_incl_t_ms_mean_all$`Deliberation Time`[7:12], 1)
df_incl_t_ms_mean_all

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
p

if (plot_type == "one") 
{
  pdf(paste("plot_", directory_files, "_profiler_non_deliberation_speed_up_original_assocc.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) 
  show(p)
  dev.off()
}





#===============================
# STOP
#===============================



# This part should be reworked??
#--------------------------------------
# PREPARE FOR PRINTING SPECIFIC PERCENTAGES
#--------------------------------------

speed_up_factor <- c()
for (i in 1:6) {
  speed_up_factor = c(speed_up_factor, df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean[i] / df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean[i + 6])
}

percentage_deliberation_from_go_original <- c()
for (i in 1:6) {
  percentage_deliberation_from_go_original = c(percentage_deliberation_from_go_original, round(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean[i] / df_p_overview_mean_GO$incl_t_ms_mean[i] * 100, 2))
}

percentage_deliberation_from_go_dcsd <- c()
for (i in 7:12) {
  percentage_deliberation_from_go_dcsd = c(percentage_deliberation_from_go_dcsd, round(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean[i] / df_p_overview_mean_GO$incl_t_ms_mean[i] * 100, 2))
}

#--------------------------------------
# PRINT SPECIFIC PERCENTAGES FOR THE TEXT
#--------------------------------------

print("Printing the speed up factor for each agent setting")
print(speed_up_factor)

print("Printing the deliberation percentage from go for original")
print(percentage_deliberation_from_go_original)

print("Printing the deliberation percentage from go for DCSD")
print(percentage_deliberation_from_go_dcsd)

# from df_p_overview_mean get the row with preset = 0.1 Original ASSOCC, function_name = FULL ASSOCC DELIBERATION, agents = 10028
df_p_overview_mean_ORIGINAL_FULL_ASSOCC_DELIBERATION_10028 <- df_p_overview_mean[df_p_overview_mean$preset == "0.1 Original ASSOCC" & df_p_overview_mean$function_name == "FULL ASSOCC DELIBERATION" & df_p_overview_mean$agents == 10028, ]

#--------------------------------------
# PRINT TABLE
#--------------------------------------


# from df_p_overview_mean get the row with preset = 0.1 Original ASSOCC, function_name = FULL ASSOCC DELIBERATION, agents = 10028
df_p_overview_mean_ORIGINAL_FULL_ASSOCC_DELIBERATION_10028 <- df_p_overview_mean[df_p_overview_mean$preset == "0.1 Original ASSOCC" & df_p_overview_mean$function_name == "FULL ASSOCC DELIBERATION" & df_p_overview_mean$agents == 10028, ]

print("Printing execution time and calls for 0.1 Original ASSOCC, FULL ASSOCC DELIBERATION, 10028 agents")
print(paste(df_p_overview_mean_ORIGINAL_FULL_ASSOCC_DELIBERATION_10028$incl_t_ms_mean, "ms"))
print(paste(df_p_overview_mean_ORIGINAL_FULL_ASSOCC_DELIBERATION_10028$calls_mean, "calls"))

# from df_p_overview_mean get the row with preset = 5.1 DCSD-5-optimisation, function_name = FULL ASSOCC DELIBERATION, agents = 10028
df_p_overview_mean_DCSD_FULL_ASSOCC_DELIBERATION_10028 <- df_p_overview_mean[df_p_overview_mean$preset == "5.1 DCSD-5-optimisation" & df_p_overview_mean$function_name == "FULL ASSOCC DELIBERATION" & df_p_overview_mean$agents == 10028, ]

print("Printing execution time and calls for 5.1 DCSD-5-optimisation, FULL ASSOCC DELIBERATION, 10028 agents")
print(paste(df_p_overview_mean_DCSD_FULL_ASSOCC_DELIBERATION_10028$incl_t_ms_mean, "ms"))
print(paste(df_p_overview_mean_DCSD_FULL_ASSOCC_DELIBERATION_10028$calls_mean, "calls"))




# Deliberation analysis DCSD
plot_incl_t_ms_function_name <- function(dataframe, p_title = "No title") {
  ggplot(dataframe, aes(x = agents, y = incl_t_ms_mean, group = function_name, colour = function_name)) +
    geom_line() +
    geom_point() +
    labs(title = p_title,
         x = "Agents",
         y = "Incl execution time (ms)") +
    theme_minimal() + scale_colour_viridis_d() +
    theme(text = element_text(size=16))
}

# From df_p_overview_mean remove all rows with preset 0.1 Original ASSOCC
df_p_overview_mean_DCSD <- df_p_overview_mean[df_p_overview_mean$preset != "0.1 Original ASSOCC", ]
# Remove GO from df_p_overview_mean_DCSD
df_p_overview_mean_DCSD <- df_p_overview_mean_DCSD[df_p_overview_mean_DCSD$function_name != "GO", ]

# I just want to select from df_p_overview_mean_DCSD, the preset, function_name, agents, and incl_t_ms_mean
df_p_overview_mean_DCSD_selection <- df_p_overview_mean_DCSD %>% select(preset, function_name, agents, incl_t_ms_mean)

# Create a dataframe and add 

preset = c()
function_name = c()
agents = c()
incl_t_ms_mean = c()

for (i in 1:6) {
  
  preset = c(preset, df_p_overview_mean_DCSD_selection$preset[i])
  function_name = c(function_name, "DCSD Time")
  agents = c(agents, df_p_overview_mean_DCSD_selection$agents[i])
  incl_t_ms_mean = c(incl_t_ms_mean, df_p_overview_mean_DCSD_selection$incl_t_ms_mean[i] - df_p_overview_mean_DCSD_selection$incl_t_ms_mean[i + 6])
}

df_p_overview_mean_DCSD_temporary = data.frame(preset, function_name, agents, incl_t_ms_mean)

df_p_overview_mean_DCSD_selection <- rbind(df_p_overview_mean_DCSD_selection, df_p_overview_mean_DCSD_temporary)

# --- The plotting function ---
plot_incl_t_ms_function_name(df_p_overview_mean_DCSD_selection, paste("In detail DCSD execution time n=", n_experiments_active))

# Remove everything except the function_name "DCSD Time", df_p_overview_mean_DCSD_selection
df_p_overview_mean_DCSD_selection_DCSD_time <- df_p_overview_mean_DCSD_selection[df_p_overview_mean_DCSD_selection$function_name == "DCSD Time", ]
plot_incl_t_ms_function_name(df_p_overview_mean_DCSD_selection_DCSD_time, paste("Execution time DCSD Time to show its linear n=", n_experiments_active))
# This is obsolete

# So which numbers do I need?
df_p_overview_mean

# Do the tests and see how much the results vary, e.g. plot the line with just 1 experiment run, then with 5, then 10 and see.
# Then dependent on what I want to say I need to run more or not. But probably since we only look at a trend that's kind of
# linear its fine to just do it with 10 runs.


# -------- Divide execution time ----------

# select in df_p_overview_mean only the context-select-activity columns
# df_p_overview_mean_CONTEXT_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]

# df_p_overview_mean_divide_execution_time <- df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_recalculated[1:6] / df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_recalculated[7:12]

# plot the df_p_overview_mean_divide_execution_time in a line plot
# data_divide_execution_time <- df_p_overview_mean_divide_execution_time #c(15.34235, 15.74206, 15.85221, 15.82626, 15.95528, 15.99495)

















#--------------------------------------
# OLDER CODE FOR CHECKING CALLS NUMBER DIFFERENCES
#--------------------------------------

# in df_p_overview select only the rows with context-select-activity
df_p_overview_mean_CONTEXT_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]

for (i in 1:6) {
  t_dif = abs(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$calls_mean[i] - df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$calls_mean[i + 6])
  print(paste("Difference:", t_dif,
              "Percentage:" , (t_dif/df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$calls_mean[i]) * 100))
}

# For 3500 households the percentage difference is 0.06 %
# In the text I could say the percentage is around 0.1 % for all experiments, so the difference is negligible for this comparison.
# Therefore the code below is obsolete

#-----------------------------------------
# OLDER OBSOLETE CODE FOR: Recalculate the mean calls and incl_t_ms
#-----------------------------------------

# Average the calls over the two presets
df_p_overview_mean_calls <- df_p_overview_mean %>% 
  group_by(function_name, agents) %>% 
  summarise(calls_mean = mean(calls, na.rm = TRUE))

v_calls_mean = c()
for (i in 1:2) {
  for (j in 1:nrow(df_p_overview_mean_calls)) {
    if (df_p_overview_mean_calls$function_name[j] == df_p_overview_mean$function_name[j + (i - 1) * nrow(df_p_overview_mean_calls)] &&
        df_p_overview_mean_calls$agents[j] == df_p_overview_mean$agents[j + (i - 1) * nrow(df_p_overview_mean_calls)])
    {
      v_calls_mean = c(v_calls_mean, df_p_overview_mean_calls$calls_mean[j])
    }
    else {
      stop(paste("Error for calculating the mean calls at i = ", i, ", j = ", j, sep = ""))
    }
  }
}

df_p_overview_mean$calls_mean <- v_calls_mean

df_p_overview_mean$incl_t_ms_recalculated <- df_p_overview_mean$incl_t_ms_per_call * df_p_overview_mean$calls_mean

#-----------------------------------------
# OLDER OBSOLETE, SINCE PLOTTING THE RIBBON DOES NOT SHOW ANYTHING NEW SINCE THE SD IS SO SMALL
#-----------------------------------------

df_p_overview_mean_CONTEXT_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]

p <- ggplot(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY, aes(x = agents, y = incl_t_ms_mean, 
                                                            color = preset,
                                                            fill = preset)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = incl_t_ms_lower, ymax = incl_t_ms_upper), alpha = 0.2) +
  labs(title = paste("Execution time", sep = ""),
       x = "Agents at start",
       y = "Included execution time mean",
       color = "Model",
       fill = "Model") +
  theme_minimal()

p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))

#p <- p + coord_cartesian(xlim = c(0, 240), ylim = c(0, 1020)) # + labs(title=paste("Infections comparison (With infections)", sep=""))
#if (plot_type == "one") { behaviourEnablePdf(paste("plot_", directory_files, "_infections_comparison_normal_n_", n_samples, sep="")) }
show(p)
#if (plot_type == "one") { dev.off() }
