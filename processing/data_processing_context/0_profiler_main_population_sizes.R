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
#plot_type <- "one" 
#plot_type <- "all"

directory_r <- "D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context"
directory_files <- "2024_09_23_scalability_hospital_fix"

#--- WORKSPACE AND DIRECTORY ---
#-   CHANGE DIRECTORY   -
setwd(paste(directory_r, directory_files, sep="/"))
getwd()

source("../0_profiler_support_population_sizes.R")

# C = context depth, H = households, A = action space, R = random seed

n_experiments_active = 10
random_seeds = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")[1:n_experiments_active]

if (directory_files == "2024_09_23_scalability_hospital_fix")
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

#--------------------------------------
# PLOTTING
#--------------------------------------

#========= SELECT ACTIVITY =========
df_p_overview_mean_CONTEXT_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]

p <- ggplot(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY, aes(x = agents, y = incl_t_ms_mean, 
                                                            color = preset,
                                                            fill = preset)) +
  geom_line(linewidth = 1.2) +
  labs(title = paste("Execution time comparison of Deliberation", sep = ""),
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

#========= FULL ASSOCC DELIBERATION =========
df_p_overview_mean_FULL_ASSOCC_DELIBERATION <- df_p_overview_mean[df_p_overview_mean$function_name == "FULL ASSOCC DELIBERATION", ]

p <- ggplot(df_p_overview_mean_FULL_ASSOCC_DELIBERATION, aes(x = agents, y = incl_t_ms_mean, 
                                                            color = preset,
                                                            fill = preset)) +
  geom_line(linewidth = 1.2) +
  labs(title = paste("Execution time comparison of Full ASSOCC Delib", sep = ""),
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


#========= GO =========
df_p_overview_mean_GO <- df_p_overview_mean[df_p_overview_mean$function_name == "GO", ]

p <- ggplot(df_p_overview_mean_GO, aes(x = agents, y = incl_t_ms_mean, 
                                                             color = preset,
                                                             fill = preset)) +
  geom_line(linewidth = 1.2) +
  labs(title = paste("Execution time comparison of GO", sep = ""),
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









# Deliberation analysis DCSD

plot_incl_t_ms_function_name <- function(dataframe, p_title = "No title") {
  ggplot(dataframe, aes(x = agents, y = incl_t_ms, group = function_name, colour = function_name)) +
    geom_line() +
    geom_point() +
    labs(title = p_title,
         x = "Agents",
         y = "Incl time") +
    theme_minimal() + scale_colour_viridis_d() +
    theme(text = element_text(size=16))
}

# From df_p_overview_mean remove all rows with preset 0.1 Original ASSOCC
df_p_overview_mean_DCSD <- df_p_overview_mean[df_p_overview_mean$preset != "0.1 Original ASSOCC", ]
# Remove GO from df_p_overview_mean_DCSD
df_p_overview_mean_DCSD <- df_p_overview_mean_DCSD[df_p_overview_mean_DCSD$function_name != "GO", ]

# I just want to select from df_p_overview_mean_DCSD, the preset, function_name, agents, and incl_t_ms
df_p_overview_mean_DCSD_selection <- df_p_overview_mean_DCSD %>% 
  select(preset, function_name, agents, incl_t_ms)

# Create a dataframe and add 

preset = c()
function_name = c()
agents = c()
incl_t_ms = c()

for (i in 1:6) {
  
  preset = c(preset, df_p_overview_mean_DCSD_selection$preset[i])
  function_name = c(function_name, "DCSD Time")
  agents = c(agents, df_p_overview_mean_DCSD_selection$agents[i])
  incl_t_ms = c(incl_t_ms, df_p_overview_mean_DCSD_selection$incl_t_ms[i] - df_p_overview_mean_DCSD_selection$incl_t_ms[i + 6])
}

df_p_overview_mean_DCSD_temporary = data.frame(preset, function_name, agents, incl_t_ms)

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
df_p_overview_mean_CONTEXT_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]

df_p_overview_mean_divide_execution_time <- df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_recalculated[1:6] / df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_recalculated[7:12]

# plot the df_p_overview_mean_divide_execution_time in a line plot
data_divide_execution_time <- df_p_overview_mean_divide_execution_time #c(15.34235, 15.74206, 15.85221, 15.82626, 15.95528, 15.99495)








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




#--------------------------------------
# OLDER CODE FOR PLOTTING
#--------------------------------------

plot_calls <- function(dataframe) {
  ggplot(dataframe, aes(x = function_name, y = incl_t_ms, fill = as.factor(preset))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Execution time for different context-depths",
         x = "Function Name",
         y = "Incl time",
         fill = "CD") +
    theme_minimal() + scale_fill_viridis_d() +
    theme(axis.text.x = element_text(angle = 7, hjust = 0.5, vjust = 0.5), text = element_text(size=16))
}
# + theme(legend.position="bottom", text = element_text(size=16))

# I want to plot the same function as before. However with the viridis colour palette.


if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_execution_context_depths.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }
plot_calls(df_p_mean_summarized)
if (plot_type == "one") { dev.off() }

