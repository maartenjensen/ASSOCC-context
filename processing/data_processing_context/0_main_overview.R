#--- LIBRARIES ---
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
directory_r <- "D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context"

directory_files <- "2024-02-04 No Lockdown Full"

# filenames_profiler <- c("report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= false].csv",
#                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= false].csv",
#                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= true].csv")
# #"report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= true].csv" is omitted.

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

#filenames_realism  <- c("covid-sim-small-full-test-scalability.csv") # covid-sim ContextNeedBalancing-first-test.csv")

one_plot = TRUE

#--- WORKSPACE AND DIRECTORY ---
#-   CHANGE DIRECTORY   -
setwd(paste(directory_r, directory_files, sep="/"))
getwd()

#--------------------------------------
#---    PROFILER - SCALABILITY      ---
#--------------------------------------
source("1_profiler_overview.R")
df_profiler = profilerLoadData(paste(directory_r, directory_files, sep="/"), filenames_profiler)

df_profiler_overview = profilerLoadSpecificData(df_profiler, c("GO", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR", "CONTEXT-DELIBERATION-SELECT-ACTIVITY"))

df_profiler_csn = profilerLoadSpecificData(df_profiler, "CSN")
df_profiler_cssn = profilerLoadSpecificData(df_profiler, "CSSN")
df_profiler_cso = profilerLoadSpecificData(df_profiler, "CSO-")
df_profiler_csso = profilerLoadSpecificData(df_profiler, "CSSO-")
df_profiler_csowh = profilerLoadSpecificData(df_profiler, "CSOWH")
df_profiler_cssowh = profilerLoadSpecificData(df_profiler, "CSSOWH")
df_profiler_csft = profilerLoadSpecificData(df_profiler, "CSFT")
df_profiler_cssft = profilerLoadSpecificData(df_profiler, "CSSFT")

# The profiler function that summarizes all the important results
profilerSummarize(df_profiler, df_profiler_overview)

# A plotting function to plot the comparison in speed. I can always just plot this, 
# but for this case its not so relevant. What I can actually do is plot the specific
# calls of functions, such as the leisure functions.
profilerPlotOverview(df_profiler, "", one_plot)

options(scipen=100) # This is for the profiler results

one_plot = FALSE
gl_pdf_width  = 10
gl_pdf_height = 7
if (one_plot) { pdf("plot_profiler_results_each_context_no_lockdown.pdf", width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

full_assocc_deliberation = "FULL ASSOCC DELIBERATION"

# Converting the data frame to an aggregate
df_profiler_mean_times <- df_profiler %>% 
  group_by(context, function_name) %>% 
  summarise(calls = mean(calls, na.rm = TRUE),
            incl_t_ms = mean(incl_t_ms, na.rm = TRUE),
            excl_t_ms = mean(excl_t_ms, na.rm = TRUE),
            excl_calls = mean(excl_calls, na.rm = TRUE))

df_profiler_mean_times$calls <- round(df_profiler_mean_times$calls, digits=2)
df_profiler_mean_times$incl_t_ms <- round(df_profiler_mean_times$incl_t_ms, digits=4)
df_profiler_mean_times$excl_t_ms <- round(df_profiler_mean_times$excl_t_ms, digits=4)
df_profiler_mean_times$excl_calls <- round(df_profiler_mean_times$excl_calls, digits=6)

# Taking out the interesting function names
selected_strings <- c("GO", "SELECT-ACTIVITY", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")

# Filter the dataframe
df_profiler_mean_times_summarized <- df_profiler_mean_times[grep(paste(selected_strings, collapse="|"), df_profiler_mean_times$function_name), ]

# Remove a specific string
string_to_remove <- "CONTEXT-DELIBERATION-SELECT-ACTIVITY"
df_profiler_mean_times_summarized <- subset(df_profiler_mean_times_summarized, !grepl(string_to_remove, function_name))

# Replace specific in the 'function_name' column
for (i in 1:nrow(df_profiler_mean_times_summarized)) {
  if (df_profiler_mean_times_summarized$function_name[i] == "SELECT-ACTIVITY")
  { df_profiler_mean_times_summarized$function_name[i] <- "CONTEXT-SELECT-ACTIVITY" }
  if (df_profiler_mean_times_summarized$function_name[i] == "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")
  { df_profiler_mean_times_summarized$function_name[i] <- full_assocc_deliberation }
}

df_profiler_mean_times_summarized <- df_profiler_mean_times_summarized[order(df_profiler_mean_times_summarized$context, df_profiler_mean_times_summarized$function_name), ]

plot_calls <- function(dataframe) {
  ggplot(dataframe, aes(x = function_name, y = incl_t_ms, fill = as.factor(context))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Calls for Each Context and Function Name Combination",
         x = "Function Name",
         y = "Incl time",
         fill = "Context") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_calls(df_profiler_mean_times_summarized)

# Taking out the interesting function names
selected_strings <- c("GO", "SELECT-ACTIVITY", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")

# Filter the dataframe
df_profiler_filtered <- df_profiler[grep(paste(selected_strings, collapse="|"), df_profiler$function_name), ]

# Remove a specific string
string_to_remove <- "CONTEXT-DELIBERATION-SELECT-ACTIVITY"
df_profiler_filtered <- subset(df_profiler_filtered, !grepl(string_to_remove, function_name))

# Replace occurrences in the 'function_name' column
for (i in 1:nrow(df_profiler_filtered)) {
  if (df_profiler_filtered$function_name[i] == "SELECT-ACTIVITY")
  { df_profiler_filtered$function_name[i] <- "CONTEXT-SELECT-ACTIVITY" }
  if (df_profiler_filtered$function_name[i] == "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")
  { df_profiler_filtered$function_name[i] <- full_assocc_deliberation }
}

# Select only CONTEXT and Full ASSOCC
selected_strings <- c("CONTEXT-SELECT-ACTIVITY", full_assocc_deliberation)
df_profiler_context_and_full_assocc <- df_profiler_filtered[grep(paste(selected_strings, collapse="|"), df_profiler_filtered$function_name), ]

pdf("plot_profiler_results_deliberation.pdf", width=gl_pdf_width, height=gl_pdf_height, pointsize=12)
# Combined
ggplot(df_profiler_context_and_full_assocc, aes(x = factor(context), y = incl_t_ms, fill = function_name)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for Deliberation",
       x = "Context",
       y = "incl_t_ms",
       fill = "Function Name") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

pdf("plot_profiler_results_go.pdf", width=gl_pdf_width, height=gl_pdf_height, pointsize=12)
# Select only GO
selected_strings <- c("GO")
df_profiler_go <- df_profiler_filtered[grep(paste(selected_strings, collapse="|"), df_profiler_filtered$function_name), ]

# Separated
ggplot(df_profiler_go, aes(x = factor(context), y = incl_t_ms)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for GO",
       x = "Context-depth",
       y = "incl_t_ms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Select only Context
selected_strings <- c("CONTEXT-SELECT-ACTIVITY")
df_profiler_context <- df_profiler_filtered[grep(paste(selected_strings, collapse="|"), df_profiler_filtered$function_name), ]

# Separated
ggplot(df_profiler_context, aes(x = factor(context), y = incl_t_ms)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for CONTEXT-SELECT-ACTIVITY",
       x = "Context-depth",
       y = "incl_t_ms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Select only Full ASSOCC
selected_strings <- c(full_assocc_deliberation)
df_profiler_full_ASSOCC <- df_profiler_filtered[grep(paste(selected_strings, collapse="|"), df_profiler_filtered$function_name), ]

# Separated
ggplot(df_profiler_full_ASSOCC, aes(x = factor(context), y = incl_t_ms)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for Full ASSOCC",
       x = "Context-depth",
       y = "incl_t_ms") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

if (one_plot) { dev.off() }

#--------------------------------------
#--- Profiler function calls and time? ---
#--------------------------------------

# I think I want to automize this, that it finds the right factor for the right row, probably a for loop
# going through every row and reading the line if CSN, then add CSN, etc.

selected_strings <- c("CONTEXT-STATE-NIGHT", "CSN-AFTER-MINIMAL-CONTEXT-F", "CSN-DEFAULT",
                      "CONTEXT-STATE-FREE-TIME", "CSFT-AFTER-MINIMAL-CONTEXT-F", "CSFT-DEFAULT")

df_profiler_mean_times_filtered <- df_profiler_mean_times[grep(paste(selected_strings, collapse="|"), df_profiler_mean_times$function_name), ]
df_profiler_mean_times_filtered <- df_profiler_mean_times_filtered[df_profiler_mean_times_filtered$context == 1, ]

df_profiler_mean_times_filtered$factors <- c("Function", "Function", "Unsolved", "Habitual", "Unsolved", "Habitual")
df_profiler_mean_times_filtered$state <- c("Freetime", "Night", "Freetime", "Freetime", "Night", "Night")

plot_calls <- function(dataframe) {
  ggplot(dataframe, aes(x = state, y = calls, fill = as.factor(factors))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Calls for Each Context and Function Name Combination",
         x = "Function Name",
         y = "Calls",
         fill = "Context") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_calls(df_profiler_mean_times_filtered)
