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

# This is just a string with the directory name
directory_files <- "2024_07_21_scalability"
directory_files <- "2026_01_01_scalability_everything"
directory_files <- "2024_09_23_scalability_hospital_fix"

#--- WORKSPACE AND DIRECTORY ---
#-   CHANGE DIRECTORY   -
setwd(paste(directory_r, directory_files, sep="/"))
getwd()

source("../0_profiler_support_population_sizes.R")

# C = context depth
# H = households
# A = action space
# R = random seed

#2024_07_21_scalability
if (directory_files == "2024_07_21_scalability")
{
  filenames_profiler <- retrieve_filenames_profiler(c("0.1 Original ASSOCC", "5.1 DCSD-5-optimisation"),
                                                    c("350", "700", "1400", "2100", "2800", "3500"),
                                                    c("6"),
                                                    c("5", "6", "7"))
}

if (directory_files == "2026_01_01_scalability_everything")
{
  filenames_profiler <- retrieve_filenames_profiler(c("0.1 Original ASSOCC", "5.1 DCSD-5-optimisation"),
                                                    c("350", "700", "1400", "2100", "2800", "3500"),
                                                    c("6"),
                                                    c("10", "11", "12", "13", "14"))
}

if (directory_files == "2024_09_23_scalability_hospital_fix")
{
  filenames_profiler <- retrieve_filenames_profiler(c("0.1 Original ASSOCC", "5.1 DCSD-5-optimisation"),
                                                    c("350", "700", "1400", "2100", "2800", "3500"),
                                                    c("6"),
                                                    c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
}
# Households, Random seed, Action space, Preset
# report-[-P= 0.1 Original ASSOCC -H= 350 -A= 6 -R= 0]
# report-[-P= 0.2 Original ASSOCC-lockdown -H= 350 -A= 6 -R= 0]

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

# The profiler function that summarizes all the important results
profilerSummarize(df_profiler, df_p_overview)


#--------------------------------------
# Recalculate data and take mean
#--------------------------------------
# for df_p_overview I want to divide incl_t_ms by the number of calls
df_p_overview$incl_t_ms_per_call <- df_p_overview$incl_t_ms / df_p_overview$calls

# in df_p_overview I want to add another column for the number of agents
# for each number of households there are specific number of agents, see below where households = agents, households = agents, ...
# 350 = 1004, 700 = 2008, 1400 = 4016, 2100 = 6016, 2800 = 8024, 3500 = 10028
df_p_overview$agents <- c(1004, 2008, 4016, 6016, 8024, 10028)[match(df_p_overview$households, c(350, 700, 1400, 2100, 2800, 3500))]

# In the following functions and plots the households column is exchanged for the agents column
df_p_overview_mean <- df_p_overview %>% 
  group_by(preset, function_name, agents) %>% 
  summarise(calls = mean(calls, na.rm = TRUE),
            incl_t_ms = mean(incl_t_ms, na.rm = TRUE),
            excl_t_ms = mean(excl_t_ms, na.rm = TRUE),
            excl_calls = mean(excl_calls, na.rm = TRUE),
            incl_t_ms_per_call = mean(incl_t_ms_per_call, na.rm = TRUE))

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



#--------------------------------------
# PLOTTING
#--------------------------------------

# I want a line plot with the different agents
# Using ggplot and the df_p_overview_mean dataframe, the agents as x-axis, and the incl_t_ms_recalculated column as y-axis

plot_incl_t_ms_recalculated <- function(dataframe, p_title = "No title") {
  ggplot(dataframe, aes(x = agents, y = incl_t_ms_recalculated, group = preset, colour = preset)) +
    geom_line() +
    geom_point() +
    labs(title = p_title,
         x = "Agents",
         y = "Incl time") +
    theme_minimal() + scale_colour_viridis_d() +
    theme(text = element_text(size=16))
}

plot_calls <- function(dataframe, p_title = "No title") {
  ggplot(dataframe, aes(x = agents, y = calls, group = preset, colour = preset)) +
    geom_line() +
    geom_point() +
    labs(title = p_title,
         x = "Agents",
         y = "Calls") +
    theme_minimal() + scale_colour_viridis_d() +
    theme(text = element_text(size=16))
}

# Filter the df_p_overview_mean dataframe, by only retaining the GO function

df_p_overview_mean_GO <- df_p_overview_mean[df_p_overview_mean$function_name == "GO", ]
plot_incl_t_ms_recalculated(df_p_overview_mean_GO, "Execution time GO function")

df_p_overview_mean_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]
plot_incl_t_ms_recalculated(df_p_overview_mean_SELECT_ACTIVITY, "Execution time Select-activity")

df_p_overview_mean_FULL_ASSOCC_DELIBERATION <- df_p_overview_mean[df_p_overview_mean$function_name == "FULL ASSOCC DELIBERATION", ]
plot_calls(df_p_overview_mean_FULL_ASSOCC_DELIBERATION, "Calls of Full ASSOCC Deliberation")

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
plot_incl_t_ms_function_name(df_p_overview_mean_DCSD_selection, "In detail DCSD execution time")

# Remove everything except the function_name "DCSD Time", df_p_overview_mean_DCSD_selection
df_p_overview_mean_DCSD_selection_DCSD_time <- df_p_overview_mean_DCSD_selection[df_p_overview_mean_DCSD_selection$function_name == "DCSD Time", ]
plot_incl_t_ms_function_name(df_p_overview_mean_DCSD_selection_DCSD_time, "Execution time DCSD Time to show its linear")
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
data_divide_execution_time <- c(15.34235, 15.74206, 15.85221, 15.82626, 15.95528, 15.99495)

# plot data in a line plot
plot(data_divide_execution_time, type = "o", col = "blue", xlab = "Agents", ylab = "Execution time ratio", main = "Execution time ratio of context-select-activity")

# I'm not sure if it says anything so I don't need to include it.












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

