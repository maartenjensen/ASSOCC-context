#-------------------------------
#---  INITIALISE LIBRARIES   ---
#-------------------------------

# Install the libraries
#install.packages()

# Open the libraries
if (!exists("libraries_loaded"))
{
  library(tidyverse)
  library(ggplot2)
  library(sjmisc)
  library(readr)
  
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

# This is just a string with the directory name
directory_files <- "2024-02-14-no-lockdown"

# filenames_profiler <- c("report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= false].csv",
#                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= false].csv",
#                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= true].csv")
# #"report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= true].csv" is omitted.

# C = context depth
# H = households
# R = random see
# A = action space
# L = lockdown
# DCC = disable conflict checking
# SRFQ = should rigidly follow quarantine

# behavior-space-export-profiling (list "C=" ce-context-depth "-H=" ce-households-for-context-scenario 
#   "-R=" #random-seed "-A=" ce-action-space "-L=" ce-enable-global-lockdown "-DCC=" ce-disable-conflict-checking
#   "-SRFQ=" ce-should-rigidly-follow-quarantine)

filenames_profiler <- c("report-[C= 0 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 0 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 0 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 0 -H= 350 -R= 4 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 0 -H= 350 -R= 5 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 1 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 1 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 1 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 1 -H= 350 -R= 4 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 1 -H= 350 -R= 5 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 2 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 2 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 2 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 2 -H= 350 -R= 4 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 2 -H= 350 -R= 5 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 3 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 3 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 3 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 3 -H= 350 -R= 4 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 3 -H= 350 -R= 5 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 4 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 4 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 4 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 4 -H= 350 -R= 4 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 4 -H= 350 -R= 5 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 5 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 5 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 5 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 5 -H= 350 -R= 4 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 5 -H= 350 -R= 5 -A= 6 -L= false -DCC= false -SRFQ= false].csv")
                        #"report-[C= 0 -H= 350 -R= 1 -A= 6 -L= true].csv",
                        #"report-[C= 1 -H= 350 -R= 1 -A= 6 -L= false].csv",
                        #"report-[C= 1 -H= 350 -R= 1 -A= 6 -L= true].csv")


#--- WORKSPACE AND DIRECTORY ---
#-   CHANGE DIRECTORY   -
setwd(paste(directory_r, directory_files, sep="/"))
getwd()


#--------------------------------------
#---    LOAD ALL PROFILER DATA      ---
#--------------------------------------
source("../0_profiler_support.R")
df_profiler = profilerLoadData(paste(directory_r, directory_files, sep="/"), filenames_profiler)

df_p_overview = profilerLoadSpecificData(df_profiler, c("GO", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR", "CONTEXT-DELIBERATION-SELECT-ACTIVITY"))

df_p_csn = profilerLoadSpecificData(df_profiler, "CSN")
df_p_cssn = profilerLoadSpecificData(df_profiler, "CSSN")
df_p_cso = profilerLoadSpecificData(df_profiler, "CSO-")
df_p_csso = profilerLoadSpecificData(df_profiler, "CSSO-")
df_p_csowh = profilerLoadSpecificData(df_profiler, "CSOWH")
df_p_cssowh = profilerLoadSpecificData(df_profiler, "CSSOWH")
df_p_csft = profilerLoadSpecificData(df_profiler, "CSFT")
df_p_cssft = profilerLoadSpecificData(df_profiler, "CSSFT")

# The profiler function that summarizes all the important results
profilerSummarize(df_profiler, df_p_overview)



#---------------------------------------
#---    PREPARE AND COMBINE DATA     ---
#---------------------------------------
if (plot_type == "all") { pdf(paste("plot_", directory_files, "_profiler_results_each_context_no_lockdown.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

full_assocc_deliberation = "FULL ASSOCC DELIBERATION"

# Converting the data frame to an aggregate
df_p_mean <- df_profiler %>% 
  group_by(context, function_name) %>% 
  summarise(calls = mean(calls, na.rm = TRUE),
            incl_t_ms = mean(incl_t_ms, na.rm = TRUE),
            excl_t_ms = mean(excl_t_ms, na.rm = TRUE),
            excl_calls = mean(excl_calls, na.rm = TRUE))

df_p_mean_std <- df_profiler %>% 
  group_by(context, function_name) %>% 
  summarise(calls = sd(calls, na.rm = TRUE),
            incl_t_ms = sd(incl_t_ms, na.rm = TRUE),
            excl_t_ms = sd(excl_t_ms, na.rm = TRUE),
            excl_calls = sd(excl_calls, na.rm = TRUE))

df_p_mean$calls <- round(df_p_mean$calls, digits=2)
df_p_mean$incl_t_ms <- round(df_p_mean$incl_t_ms, digits=4)
df_p_mean$excl_t_ms <- round(df_p_mean$excl_t_ms, digits=4)
df_p_mean$excl_calls <- round(df_p_mean$excl_calls, digits=6)

df_p_mean$calls_sd <- df_p_mean_std$calls
df_p_mean$incl_t_ms_sd <- df_p_mean_std$incl_t_ms
df_p_mean$excl_t_ms_sd <- df_p_mean_std$excl_t_ms
df_p_mean$excl_calls_sd <- df_p_mean_std$excl_calls

# I want to have the standard deviations of df_p_mean$calls



# Determining the most interesting names
selected_strings <- c("GO", "SELECT-ACTIVITY", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")

# Filter the dataframe
df_p_mean_summarized <- df_p_mean[grep(paste(selected_strings, collapse="|"), df_p_mean$function_name), ]

# Remove a specific string
string_to_remove <- "CONTEXT-DELIBERATION-SELECT-ACTIVITY"
df_p_mean_summarized <- subset(df_p_mean_summarized, !grepl(string_to_remove, function_name))

# Replace specific in the 'function_name' column
for (i in 1:nrow(df_p_mean_summarized)) {
  if (df_p_mean_summarized$function_name[i] == "SELECT-ACTIVITY")
  { df_p_mean_summarized$function_name[i] <- "CONTEXT-SELECT-ACTIVITY" }
  if (df_p_mean_summarized$function_name[i] == "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")
  { df_p_mean_summarized$function_name[i] <- full_assocc_deliberation }
}

df_p_mean_summarized <- df_p_mean_summarized[order(df_p_mean_summarized$context, df_p_mean_summarized$function_name), ]




#-----------------------------------------
#--- Plot the data                     ---
#-----------------------------------------

plot_calls <- function(dataframe) {
  ggplot(dataframe, aes(x = function_name, y = incl_t_ms, fill = as.factor(context))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Execution time for different context-depths",
         x = "Function Name",
         y = "Incl time",
         fill = "Context") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_execution_context_depths.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }
plot_calls(df_p_mean_summarized)
if (plot_type == "one") { dev.off() }

#-----------------------------------------
#--- Prepare the data                  ---
#-----------------------------------------

# Taking out the interesting function names
selected_strings <- c("GO", "SELECT-ACTIVITY", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")

# Filter the dataframe
df_p_filtered <- df_profiler[grep(paste(selected_strings, collapse="|"), df_profiler$function_name), ]

# Remove a specific string
string_to_remove <- "CONTEXT-DELIBERATION-SELECT-ACTIVITY"
df_p_filtered <- subset(df_p_filtered, !grepl(string_to_remove, function_name))

# Replace occurrences in the 'function_name' column
for (i in 1:nrow(df_p_filtered)) {
  if (df_p_filtered$function_name[i] == "SELECT-ACTIVITY")
  { df_p_filtered$function_name[i] <- "CONTEXT-SELECT-ACTIVITY" }
  if (df_p_filtered$function_name[i] == "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")
  { df_p_filtered$function_name[i] <- full_assocc_deliberation }
}

# Select only CONTEXT and Full ASSOCC
selected_strings <- c("CONTEXT-SELECT-ACTIVITY", full_assocc_deliberation)
df_p_context_and_full_assocc <- df_p_filtered[grep(paste(selected_strings, collapse="|"), df_p_filtered$function_name), ]


#-----------------------------------------
#--- Plot the results                  ---
#-----------------------------------------

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_results_deliberation.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

# Combined
ggplot(df_p_context_and_full_assocc, aes(x = factor(context), y = incl_t_ms, fill = function_name)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for Deliberation",
       x = "Context",
       y = "incl_t_ms",
       fill = "Function Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

if (plot_type == "one") { dev.off() }


# Select only GO
selected_strings <- c("GO")
df_p_go <- df_p_filtered[grep(paste(selected_strings, collapse="|"), df_p_filtered$function_name), ]

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_results_go.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }
# Separated
ggplot(df_p_go, aes(x = factor(context), y = incl_t_ms)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for GO",
       x = "Context-depth",
       y = "incl_t_ms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

if (plot_type == "one") { dev.off() }


# Select only Context
selected_strings <- c("CONTEXT-SELECT-ACTIVITY")
df_p_context <- df_p_filtered[grep(paste(selected_strings, collapse="|"), df_p_filtered$function_name), ]

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_results_select_activity.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }
# Separated
ggplot(df_p_context, aes(x = factor(context), y = incl_t_ms)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for CONTEXT-SELECT-ACTIVITY",
       x = "Context-depth",
       y = "incl_t_ms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

if (plot_type == "one") { dev.off() }


# Select only Full ASSOCC
selected_strings <- c(full_assocc_deliberation)
df_p_full_ASSOCC <- df_p_filtered[grep(paste(selected_strings, collapse="|"), df_p_filtered$function_name), ]

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_results_full_ASSOCC.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }
# Separated
ggplot(df_p_full_ASSOCC, aes(x = factor(context), y = incl_t_ms)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for Full ASSOCC",
       x = "Context-depth",
       y = "incl_t_ms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

if (plot_type == "one") { dev.off() }

if (plot_type == "all") { dev.off() }


#-----------------------------------------
#--- Profiler function calls and time? ---
#-----------------------------------------

depth_value = 1

for (depth_value in 1:5)
{
depth_value_str = toString(depth_value)

selected_strings <- c("CSN-FUNCTION", "CSN-FUNCTION-SUCCEEDED",
                      "CSFT-FUNCTION", "CSFT-FUNCTION-SUCCEEDED",
                      "CSO-FUNCTION", "CSO-FUNCTION-SUCCEEDED",
                      "CSOWH-FUNCTION", "CSOWH-FUNCTION-SUCCEEDED",
                      "CSSN-FUNCTION", "CSSN-FUNCTION-SUCCEEDED",
                      "CSSFT-FUNCTION", "CSSFT-FUNCTION-SUCCEEDED",
                      "CSSO-FUNCTION", "CSSO-FUNCTION-SUCCEEDED",
                      "CSSOWH-FUNCTION", "CSSOWH-FUNCTION-SUCCEEDED")

df_p_mean_filtered <- df_p_mean[grep(paste(selected_strings, collapse="|"), df_p_mean$function_name), ]
df_p_mean_filtered <- df_p_mean_filtered[df_p_mean_filtered$context == depth_value, ]

for (i in 1:length(selected_strings))
{
  if (!selected_strings[i] %in% df_p_mean_filtered$function_name)
  {
    df_p_mean_filtered <- rbind(df_p_mean_filtered, data.frame(context = depth_value_str,
                                                                                         function_name = selected_strings[i], calls = 0, incl_t_ms = 0, excl_t_ms = 0, excl_calls = 0))
  }
}

factors = c()
state = c()

# If it is not in there it should be added with a zero number?? I think so...

for (i in 1:nrow(df_p_mean_filtered))
{
  if (str_contains(df_p_mean_filtered$function_name[i], "-FUNCTION-SUCCEEDED"))
  { factors <- c(factors, "Succeeded") }
  else
  { factors <- c(factors, "Calls") }

  print(df_p_mean_filtered$function_name[i])
  if (str_contains(df_p_mean_filtered$function_name[i], "CSN-"))
  {
    state <- c(state, "Night")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSFT-"))
  {
    state <- c(state, "Freetime")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSO-"))
  {
    state <- c(state, "Obligation")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSOWH-"))
  {
    state <- c(state, "Obligation WH")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSSN-"))
  {
    state <- c(state, "Night Sick")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSSFT-"))
  {
    state <- c(state, "Freetime Sick")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSSO-"))
  {
    state <- c(state, "Obligation Sick")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSSOWH-"))
  {
    state <- c(state, "Obligation WH Sick")
  }
}

df_p_mean_filtered$factors <- factors
df_p_mean_filtered$state <- state

plot_calls <- function(dataframe, context_depth) {
  ggplot(dataframe, aes(x = state, y = calls, fill = as.factor(factors))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Context State Success - CD: ", context_depth, sep=""),
         x = "Function Name",
         y = "Calls",
         fill = "Context") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(0, 85000)) #+
    #scale_colour_manual(labels=c('Calls'='Calls', 'Succeeded'='Succeeded'), breaks=c('Calls','Succeeded'), values=c('#33ddff', '#48bf3f'))
}

show(plot_calls(df_p_mean_filtered, depth_value))

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_cd_", depth_value, "_context_state_success.pdf", sep=""), width=9, height=5) }
show(plot_calls(df_p_mean_filtered, depth_value))
if (plot_type == "one") { dev.off() }

}

#-----------------------------------------
#---          Combined plots           ---
#-----------------------------------------
depth_values <- 1:5

selected_strings <- c("CSN-FUNCTION", "CSN-FUNCTION-SUCCEEDED",
                      "CSFT-FUNCTION", "CSFT-FUNCTION-SUCCEEDED",
                      "CSO-FUNCTION", "CSO-FUNCTION-SUCCEEDED",
                      "CSOWH-FUNCTION", "CSOWH-FUNCTION-SUCCEEDED",
                      "CSSN-FUNCTION", "CSSN-FUNCTION-SUCCEEDED",
                      "CSSFT-FUNCTION", "CSSFT-FUNCTION-SUCCEEDED",
                      "CSSO-FUNCTION", "CSSO-FUNCTION-SUCCEEDED",
                      "CSSOWH-FUNCTION", "CSSOWH-FUNCTION-SUCCEEDED")

combined_df <- data.frame()

for (depth_value in depth_values) {
  depth_value_str <- toString(depth_value)
  
  df_p_mean_filtered <- df_p_mean[grep(paste(selected_strings, collapse = "|"), df_p_mean$function_name), ]
  df_p_mean_filtered <- df_p_mean_filtered[df_p_mean_filtered$context == depth_value, ]
  
  for (i in 1:length(selected_strings)) {
    if (!selected_strings[i] %in% df_p_mean_filtered$function_name) {
      df_p_mean_filtered <- rbind(df_p_mean_filtered, data.frame(context = depth_value_str,
                                                                                           function_name = selected_strings[i], calls = 0, incl_t_ms = 0, excl_t_ms = 0, excl_calls = 0))
    }
  }
  
  factors <- c()
  state <- c()
  
  for (i in 1:nrow(df_p_mean_filtered)) {
    if (str_contains(df_p_mean_filtered$function_name[i], "-FUNCTION-SUCCEEDED")) {
      factors <- c(factors, "Succeeded")
    } else {
      factors <- c(factors, "Calls")
    }
    
    if (str_contains(df_p_mean_filtered$function_name[i], "CSN-")) {
      state <- c(state, "Night")
    }
    if (str_contains(df_p_mean_filtered$function_name[i], "CSFT-")) {
      state <- c(state, "Freetime")
    }
    if (str_contains(df_p_mean_filtered$function_name[i], "CSO-")) {
      state <- c(state, "Obligation")
    }
    if (str_contains(df_p_mean_filtered$function_name[i], "CSOWH-")) {
      state <- c(state, "Obligation WH")
    }
    if (str_contains(df_p_mean_filtered$function_name[i], "CSSN-")) {
      state <- c(state, "Night Sick")
    }
    if (str_contains(df_p_mean_filtered$function_name[i], "CSSFT-")) {
      state <- c(state, "Freetime Sick")
    }
    if (str_contains(df_p_mean_filtered$function_name[i], "CSSO-")) {
      state <- c(state, "Obligation Sick")
    }
    if (str_contains(df_p_mean_filtered$function_name[i], "CSSOWH-")) {
      state <- c(state, "Obligation WH Sick")
    }
  }
  
  df_p_mean_filtered$factors <- factors
  df_p_mean_filtered$state <- state
  
  combined_df <- rbind(combined_df, df_p_mean_filtered)
}

plot_calls <- function(dataframe) {
  ggplot(dataframe, aes(x = state, y = calls, fill = as.factor(factors))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Context State Success",
         x = "Function Name",
         y = "Calls",
         fill = "Context") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(0, 85000)) +
    facet_wrap(~ context, nrow = 3) # Combining plots into subplots
}

plot_calls(combined_df)

#TODO: change the color of these plots, light normal state, dark succesful state. Green (freetime), night (gray), obligation (blue?)

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_cd_all_context_state_success.pdf", sep=""), width = 9, height = 12) } # Setting up PDF for multiple plots
plot_calls(combined_df)
if (plot_type == "one") { dev.off() }


#-----------------------------------------
#--- Exporting the data for the tables ---
#-----------------------------------------

# Write in the thesis
# There is a different number of agents function calls because there are some agents that passed away due to Covid.
# I don't have to normalize the execution time data, if I normalize it will probably only change the results by 0.1%.
# Since we are only interested in the trend, then it is not important to be so precise and therefore we chose to
# not normalize all the data and rather leave it as it is.


# I want to multiply the column df_p_mean_summarized$calls with df_p_mean_summarized$normalise_with
# and then add the result to a new column df_p_mean_summarized$normalised_calls

df_p_mean_summarized

# Step two, output the data to a nice table

for (ce in 0:5) {
  
  df_p_mean_summarized_temp <- df_p_mean_summarized[df_p_mean_summarized$context==ce, ]
  str = paste(ce, "&", sep = " ")
  str = paste(str, df_p_mean_summarized_temp$incl_t_ms[df_p_mean_summarized_temp$function_name=="GO"],  "&" )
  str = paste(str, df_p_mean_summarized_temp$incl_t_ms_sd[df_p_mean_summarized_temp$function_name=="GO"],  "&" )
  str = paste(str, df_p_mean_summarized_temp$incl_t_ms[df_p_mean_summarized_temp$function_name=="CONTEXT-SELECT-ACTIVITY"],  "&" )
  str = paste(str, df_p_mean_summarized_temp$incl_t_ms_sd[df_p_mean_summarized_temp$function_name=="CONTEXT-SELECT-ACTIVITY"],  "&" )
  str = paste(str, df_p_mean_summarized_temp$incl_t_ms[df_p_mean_summarized_temp$function_name=="FULL ASSOCC DELIBERATION"],  "&" )
  str = paste(str, df_p_mean_summarized_temp$incl_t_ms_sd[df_p_mean_summarized_temp$function_name=="FULL ASSOCC DELIBERATION"],  "\\" )
  print(str)
  
}



  
  
  


  

# !!!!!!!!!!!!  OLD CODE !!!!!!!!!!!!!!!!
# !!!!!!!!!!!!  OLD CODE !!!!!!!!!!!!!!!!
# !!!!!!!!!!!!  OLD CODE !!!!!!!!!!!!!!!!
# !!!!!!!!!!!!  OLD CODE !!!!!!!!!!!!!!!!
# !!!!!!!!!!!!  OLD CODE !!!!!!!!!!!!!!!!
# !!!!!!!!!!!!  OLD CODE !!!!!!!!!!!!!!!!

#-----------------------------------------
#--- Profiler function calls and time? ---
#-----------------------------------------

# I think I want to automize this, that it finds the right factor for the right row, probably a for loop
# going through every row and reading the line if CSN, then add CSN, etc.

# Or should I make all those function names unique?? I don't know

depth_value = 1
depth_value_str = toString(depth_value)

selected_strings <- c("CSN-FUNCTION",    "CSN-DEFAULT",   "CSN-AFTER-MINIMAL-CONTEXT-F",
                      "CSFT-FUNCTION",   "CSFT-DEFAULT",  "CSFT-AFTER-MINIMAL-CONTEXT-F",
                      "CSO-FUNCTION",    "CSO-DEFAULT",   "CSO-AFTER-MINIMAL-CONTEXT-F",
                      "CSOWH-FUNCTION",  "CSOWH-DEFAULT", "CSOWH-AFTER-MINIMAL-CONTEXT-F",
                      "CSSN-FUNCTION",   "CSSN-DEFAULT",  "CSSN-AFTER-MINIMAL-CONTEXT-F",
                      "CSSFT-FUNCTION",  "CSSFT-DEFAULT", "CSSFT-AFTER-MINIMAL-CONTEXT-F",
                      "CSSO-FUNCTION",   "CSSO-DEFAULT",  "CSSO-AFTER-MINIMAL-CONTEXT-F",
                      "CSSOWH-FUNCTION", "CSSOWH-DEFAULT","CSSOWH-AFTER-MINIMAL-CONTEXT-F")

df_p_mean_filtered <- df_p_mean[grep(paste(selected_strings, collapse="|"), df_p_mean$function_name), ]
df_p_mean_filtered <- df_p_mean_filtered[df_p_mean_filtered$context == depth_value, ]

# Remove a specific string
string_to_remove <- "SUCCEEDED"
df_p_mean_filtered <- subset(df_p_mean_filtered, !grepl(string_to_remove, function_name))

for (i in 1:length(selected_strings))
{
  if (!selected_strings[i] %in% df_p_mean_filtered$function_name)
  {
    df_p_mean_filtered <- rbind(df_p_mean_filtered, data.frame(context = depth_value_str,
                                             function_name = selected_strings[i], calls = 0, incl_t_ms = 0, excl_t_ms = 0, excl_calls = 0))
  }
}

factors = c()
state = c()

# If it is not in there it should be added with a zero number?? I think so...

for (i in 1:nrow(df_p_mean_filtered))
{
  if (str_contains(df_p_mean_filtered$function_name[i], "-FUNCTION"))
  { factors <- c(factors, "0. Function") }
  if (str_contains(df_p_mean_filtered$function_name[i], "-AFTER-MINIMAL-CONTEXT"))
  { factors <- c(factors, "6. Full ASSOCC") }
  if (str_contains(df_p_mean_filtered$function_name[i], "-DEFAULT"))
  { factors <- c(factors, "1. Habitual") }
  
  
  print(df_p_mean_filtered$function_name[i])
  if (str_contains(df_p_mean_filtered$function_name[i], "CSN-"))
  {
    state <- c(state, "Night")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSFT-"))
  {
    state <- c(state, "Freetime")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSO-"))
  {
    state <- c(state, "Obligation")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSOWH-"))
  {
    state <- c(state, "Obligation WH")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSSN-"))
  {
    state <- c(state, "Night Sick")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSSFT-"))
  {
    state <- c(state, "Freetime Sick")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSSO-"))
  {
    state <- c(state, "Obligation Sick")
  }
  if (str_contains(df_p_mean_filtered$function_name[i], "CSSOWH-"))
  {
    state <- c(state, "Obligation WH Sick")
  }
}

df_p_mean_filtered$factors <- factors
df_p_mean_filtered$state <- state

plot_calls <- function(dataframe) {
  ggplot(dataframe, aes(x = state, y = calls, fill = as.factor(factors))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Context State Success per Deliberation Type",
         x = "Function Name",
         y = "Calls",
         fill = "Context") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_cd_", depth_value, "_context_state_success.pdf", sep=""), width=9, height=5) }
plot_calls(df_p_mean_filtered)
if (plot_type == "one") { dev.off() }
