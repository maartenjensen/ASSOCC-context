# Next GOAL: Check the number of agents, check if its the same for different random seeds, and then plot the number of agents instead of the households

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

df_p_overview_mean <- df_p_overview %>% 
  group_by(preset, function_name, households) %>% 
  summarise(calls = mean(calls, na.rm = TRUE),
            incl_t_ms = mean(incl_t_ms, na.rm = TRUE),
            excl_t_ms = mean(excl_t_ms, na.rm = TRUE),
            excl_calls = mean(excl_calls, na.rm = TRUE),
            incl_t_ms_per_call = mean(incl_t_ms_per_call, na.rm = TRUE))

# Average the calls over the two presets
df_p_overview_mean_calls <- df_p_overview_mean %>% 
  group_by(function_name, households) %>% 
  summarise(calls_mean = mean(calls, na.rm = TRUE))

v_calls_mean = c()
for (i in 1:2) {
  for (j in 1:nrow(df_p_overview_mean_calls)) {
    if (df_p_overview_mean_calls$function_name[j] == df_p_overview_mean$function_name[j + (i - 1) * nrow(df_p_overview_mean_calls)] &&
        df_p_overview_mean_calls$households[j] == df_p_overview_mean$households[j + (i - 1) * nrow(df_p_overview_mean_calls)])
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
# Now it is time to plothttp://127.0.0.1:36269/graphics/plot_zoom_png?width=1105&height=861
#--------------------------------------

# I want a line plot with the different households
# Using ggplot and the df_p_overview_mean dataframe, the households as x-axis, and the incl_t_ms_recalculated column as y-axis

plot_incl_t_ms_recalculated <- function(dataframe, p_title = "No title") {
  ggplot(dataframe, aes(x = households, y = incl_t_ms_recalculated, group = preset, colour = preset)) +
    geom_line() +
    geom_point() +
    labs(title = p_title,
         x = "Households",
         y = "Incl time") +
    theme_minimal() + scale_colour_viridis_d() +
    theme(text = element_text(size=16))
}

plot_calls <- function(dataframe, p_title = "No title") {
  ggplot(dataframe, aes(x = households, y = calls, group = preset, colour = preset)) +
    geom_line() +
    geom_point() +
    labs(title = p_title,
         x = "Households",
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
  ggplot(dataframe, aes(x = households, y = incl_t_ms, group = function_name, colour = function_name)) +
    geom_line() +
    geom_point() +
    labs(title = p_title,
         x = "Households",
         y = "Incl time") +
    theme_minimal() + scale_colour_viridis_d() +
    theme(text = element_text(size=16))
}

# From df_p_overview_mean remove all rows with preset 0.1 Original ASSOCC
df_p_overview_mean_DCSD <- df_p_overview_mean[df_p_overview_mean$preset != "0.1 Original ASSOCC", ]
# Remove GO from df_p_overview_mean_DCSD
df_p_overview_mean_DCSD <- df_p_overview_mean_DCSD[df_p_overview_mean_DCSD$function_name != "GO", ]

# I just want to select from df_p_overview_mean_DCSD, the preset, function_name, households, and incl_t_ms
df_p_overview_mean_DCSD_selection <- df_p_overview_mean_DCSD %>% 
  select(preset, function_name, households, incl_t_ms)

# Create a dataframe and add 

preset = c()
function_name = c()
households = c()
incl_t_ms = c()

for (i in 1:6) {
  
  preset = c(t_preset, df_p_overview_mean_DCSD_selection$preset[i])
  function_name = c(t_function_name, "DCSD Time")
  households = c(t_households, df_p_overview_mean_DCSD_selection$households[i])
  incl_t_ms = c(t_incl_t_ms, df_p_overview_mean_DCSD_selection$incl_t_ms[i] - df_p_overview_mean_DCSD_selection$incl_t_ms[i + 6])
}

df_p_overview_mean_DCSD_temporary = data.frame(preset, function_name, households, incl_t_ms)

df_p_overview_mean_DCSD_selection <- rbind(df_p_overview_mean_DCSD_selection, df_p_overview_mean_DCSD_temporary)

# --- The plotting function ---
plot_incl_t_ms_function_name(df_p_overview_mean_DCSD_selection)

# Remove everything except the function_name "DCSD Time", df_p_overview_mean_DCSD_selection
df_p_overview_mean_DCSD_selection_DCSD_time <- df_p_overview_mean_DCSD_selection[df_p_overview_mean_DCSD_selection$function_name == "DCSD Time", ]
plot_incl_t_ms_function_name(df_p_overview_mean_DCSD_selection_DCSD_time, "Execution time DCSD Time to show its linear")

# next task, use the excel sheet to put the number of agents on the x-axis, rather than the households






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

