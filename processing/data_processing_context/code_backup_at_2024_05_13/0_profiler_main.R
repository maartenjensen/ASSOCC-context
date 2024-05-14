#-------------------------------
#---  INITIALISE LIBRARIES   ---
#-------------------------------

# Install the libraries
#install.packages()
#install.packages("viridis")

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

# This is just a string with the directory name
directory_files <- "2024_03_13_full_no_lockdown"
#directory_files <- "2024_03_13_full_yes_lockdown"
#directory_files <- "2024_03_13_no_conflict"
#directory_files <- "2024_03_13_rigid_norms"
#directory_files <- "2024_03_21_n_agents"
directory_files <- "2024_04_11_forced_habits"

#--- WORKSPACE AND DIRECTORY ---
#-   CHANGE DIRECTORY   -
setwd(paste(directory_r, directory_files, sep="/"))
getwd()

source("../0_profiler_support.R")

# behavior-space-export-profiling (list "C=" ce-context-depth "-H=" ce-households-for-context-scenario "-R=" #random-seed
# "-A=" ce-action-space "-L=" ce-enable-global-lockdown "-DCC=" ce-disable-conflict-checking
# "-SRFQ=" ce-should-rigidly-follow-quarantine "-SRFH=" ce-should-rigidly-follow-habits "-B=" ce-enable-need-balancing)

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


if (directory_files == "2024_03_13_full_no_lockdown")
{
  filenames_profiler <- retrieve_filenames_profiler(c("0", "1", "2", "3", "4", "5"),
                                                    c("350"),
                                                    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                    c("6"), c("false"), c("false"), c("false"))
}
if (directory_files == "2024_03_13_full_yes_lockdown")
{
  filenames_profiler <- retrieve_filenames_profiler(c("0", "1", "2", "3", "4", "5"),
                                                    c("350"),
                                                    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                    c("6"), c("true"), c("false"), c("false"))
}
if (directory_files == "2024_03_13_no_conflict")
{
  filenames_profiler <- retrieve_filenames_profiler(c("1"),
                                                    c("350"),
                                                    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                    c("6"), c("false"), c("false", "true"), c("false"))
}
if (directory_files == "2024_03_13_rigid_norms")
{
  filenames_profiler <- retrieve_filenames_profiler(c("3"),
                                                    c("350"),
                                                    c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                    c("6"), c("false"),  c("false"), c("false", "true"))
}
if (directory_files == "2024_03_21_n_agents")
{
  filenames_profiler <- retrieve_filenames_profiler(c("0", "5"),
                                                    c("350", "700", "1400", "2100", "2800", "3500"),  
                                                    c("1", "2", "3"),
                                                    c("6"), c("false"),  c("false"), c("false"))
}


#--------------------------------------
#---    LOAD ALL PROFILER DATA      ---
#--------------------------------------
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
if (plot_type == "all") { pdf(paste("plot_", directory_files, "_profiler_all_plots.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

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
         fill = "CD") +
    theme_minimal() + scale_fill_viridis_d() +
    theme(axis.text.x = element_text(angle = 7, hjust = 0.5, vjust = 0.5), text = element_text(size=16))
}
# + theme(legend.position="bottom", text = element_text(size=16))

# I want to plot the same function as before. However with the viridis colour palette.


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

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_incl_t_ms_deliberation.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

# Combined
ggplot(df_p_context_and_full_assocc, aes(x = factor(context), y = incl_t_ms, fill = function_name)) +
  geom_boxplot() +
  labs(title = "Box Plots of incl_t_ms for Deliberation",
       x = "Context",
       y = "incl_t_ms",
       fill = "Function Name") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Select only GO
selected_strings <- c("GO")
df_p_go <- df_p_filtered[grep(paste(selected_strings, collapse="|"), df_p_filtered$function_name), ]

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_incl_t_ms_results_go.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }
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

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_incl_t_ms_select_activity.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }
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

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_incl_t_ms_full_ASSOCC.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }
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
  
  p <- ggplot(dataframe, aes(x = state, y = calls, fill = as.factor(factors))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Context State Success - CD: ", context_depth, sep=""),
         x = "Function Name",
         y = "Calls",
         fill = "Context") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5), text = element_text(size=15)) +
    coord_cartesian(ylim = c(0, 85000))
  p <- p + scale_fill_manual(values=c('#87e667', '#467536'))
  return(p)
}

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
                                                                 function_name = selected_strings[i],
                                                                 calls = 0, incl_t_ms = 0, excl_t_ms = 0, excl_calls = 0))
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
         fill = "Function") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5), legend.position="bottom", text = element_text(size=15)) +
    coord_cartesian(ylim = c(0, 85000)) +
    facet_wrap(~ context, nrow = 3) + scale_fill_manual(values=c('#87e667', '#467536')) # Combining plots into subplots
}

#p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=2, byrow=TRUE))
# scale_colour_manual(
# labels=c('Freetime'='Freetime', 'Freetime Sick'='Freetime Sick', 'Night'='Night', 'Night Sick'='Night Sick', 'Obligation'='Obligation', 'Obligation Sick'='Obligation Sick', 'Obligation WH'='Obligation WH', 'Obligation WH Sick'='Obligation WH Sick'),
# values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000', '#9911ab', '#000000'),
# breaks=c('Freetime', 'Freetime Sick', 'Night', 'Night Sick', 'Obligation', 'Obligation Sick', 'Obligation WH', 'Obligation WH Sick')) 

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

df_p_mean_summarized$incl_t_ms <- round(df_p_mean_summarized$incl_t_ms, digits=2)
df_p_mean_summarized$incl_t_ms_sd <- round(df_p_mean_summarized$incl_t_ms_sd, digits=2)

# Step two, output the data to a nice table
str_table_1 = ""

for (ce in 0:5) {
  
  df_p_mean_summarized_temp <- df_p_mean_summarized[df_p_mean_summarized$context==ce, ]
  str_table_1 = paste(str_table_1, ce, "&", sep = " ")
  str_table_1 = paste(str_table_1, df_p_mean_summarized_temp$incl_t_ms[df_p_mean_summarized_temp$function_name=="CONTEXT-SELECT-ACTIVITY"],  "&" )
  str_table_1 = paste(str_table_1, df_p_mean_summarized_temp$incl_t_ms_sd[df_p_mean_summarized_temp$function_name=="CONTEXT-SELECT-ACTIVITY"],  "&" )
  str_table_1 = paste(str_table_1, df_p_mean_summarized_temp$incl_t_ms[df_p_mean_summarized_temp$function_name=="FULL ASSOCC DELIBERATION"],  "&" )
  str_table_1 = paste(str_table_1, df_p_mean_summarized_temp$incl_t_ms_sd[df_p_mean_summarized_temp$function_name=="FULL ASSOCC DELIBERATION"],  "&" )
  str_table_1 = paste(str_table_1, df_p_mean_summarized_temp$incl_t_ms[df_p_mean_summarized_temp$function_name=="GO"],  "&" )
  str_table_1 = paste(str_table_1, df_p_mean_summarized_temp$incl_t_ms_sd[df_p_mean_summarized_temp$function_name=="GO"],  "\\\\ \n" )
}

#--- Exporting the data for the tables ---
df_profiler_ce_4 <- df_profiler[df_profiler$context=="4", ]

# Take the mean of the number of calls
df_profiler_ce_4_mean <- df_profiler_ce_4 %>% 
  group_by(function_name) %>% 
  summarise(calls = mean(calls, na.rm = TRUE),
            incl_t_ms = mean(incl_t_ms, na.rm = TRUE),
            excl_t_ms = mean(excl_t_ms, na.rm = TRUE),
            excl_calls = mean(excl_calls, na.rm = TRUE))

# Extract all the rows with function names ending with -F
df_profiler_ce_4_mean <- df_profiler_ce_4_mean[grep("F$", df_profiler_ce_4_mean$function_name), ]

df_profiler_ce_4_mean$totals <- sum(df_profiler_ce_4_mean$calls)
df_profiler_ce_4_mean$percentage <- df_profiler_ce_4_mean$calls / df_profiler_ce_4_mean$totals * 100


# Order it based on the number of calls (descending)
df_profiler_ce_4_mean <- df_profiler_ce_4_mean[order(-df_profiler_ce_4_mean$calls), ]

# Export as file for latex, just as in line 513 to 525
str_table_2 = ""

for (i in 1:nrow(df_profiler_ce_4_mean)) {
  str_table_2 = paste(str_table_2, df_profiler_ce_4_mean$function_name[i], "&", sep = " ")
  str_table_2 = paste(str_table_2, round(df_profiler_ce_4_mean$calls[i], digits=2),  "& ", sep = " ")
  str_table_2 = paste(str_table_2, round(df_profiler_ce_4_mean$percentage[i], digits=3),  "\\% \\\\ \n", sep = "" )
}


#--- Exporting the data for the tables ---
df_profiler_ce_5 <- df_profiler[df_profiler$context=="5", ]

# Take the mean of the number of calls
df_profiler_ce_5_mean <- df_profiler_ce_5 %>% 
  group_by(function_name) %>% 
  summarise(calls = mean(calls, na.rm = TRUE),
            incl_t_ms = mean(incl_t_ms, na.rm = TRUE),
            excl_t_ms = mean(excl_t_ms, na.rm = TRUE),
            excl_calls = mean(excl_calls, na.rm = TRUE))

# Extract all the rows with function names ending with -F
df_profiler_ce_5_mean <- df_profiler_ce_5_mean[grep("F$", df_profiler_ce_5_mean$function_name), ]

df_profiler_ce_5_mean$totals <- sum(df_profiler_ce_5_mean$calls)
df_profiler_ce_5_mean$percentage <- df_profiler_ce_5_mean$calls / df_profiler_ce_5_mean$totals * 100

# Order it based on the number of calls (descending)
df_profiler_ce_5_mean <- df_profiler_ce_5_mean[order(-df_profiler_ce_5_mean$calls), ]

# Export as file for latex, just as in line 513 to 525
str_table_3 = ""

for (i in 1:nrow(df_profiler_ce_5_mean)) {
  str_table_3 = paste(str_table_3, df_profiler_ce_5_mean$function_name[i], "&", sep = " ")
  str_table_3 = paste(str_table_3, round(df_profiler_ce_5_mean$calls[i], digits=2),  "& ", sep = " ")
  str_table_3 = paste(str_table_3, round(df_profiler_ce_5_mean$percentage[i], digits=5),  "\\% \\\\ \n", sep = "" )
}



writeLines(str_table_1)
writeLines(str_table_2)
writeLines(str_table_3)


# -------------------------------------------------
# Experiments specific to "2024_03_21_n_agents"
# -------------------------------------------------

# Next experiments is with the additional runs and taking the mean

if (directory_files == "2024_03_21_n_agents")
{
  
# Converting the data frame to an aggregate
df_p_mean_households <- df_profiler %>% 
  group_by(context, function_name, households) %>% 
  summarise(calls = mean(calls, na.rm = TRUE),
            incl_t_ms = mean(incl_t_ms, na.rm = TRUE),
            excl_t_ms = mean(excl_t_ms, na.rm = TRUE),
            excl_calls = mean(excl_calls, na.rm = TRUE))


selected_strings <- c("GO", "SELECT-ACTIVITY", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")

# Filter the dataframe
df_p_mean_households_summarized <- df_p_mean_households[grep(paste(selected_strings, collapse="|"), df_p_mean_households$function_name), ]

# Remove a specific string
string_to_remove <- "CONTEXT-DELIBERATION-SELECT-ACTIVITY"
df_p_mean_households_summarized <- subset(df_p_mean_households_summarized, !grepl(string_to_remove, function_name))

# Replace specific in the 'function_name' column
for (i in 1:nrow(df_p_mean_households_summarized)) {
  if (df_p_mean_households_summarized$function_name[i] == "SELECT-ACTIVITY")
  { df_p_mean_households_summarized$function_name[i] <- "CONTEXT-SELECT-ACTIVITY" }
  if (df_p_mean_households_summarized$function_name[i] == "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")
  { df_p_mean_households_summarized$function_name[i] <- full_assocc_deliberation }
}

df_p_mean_households_summarized <- df_p_mean_households_summarized[order(df_p_mean_households_summarized$context,
                                                                         df_p_mean_households_summarized$function_name), ]

df_p_mean_households_summarized_go <- df_p_mean_households_summarized[df_p_mean_households_summarized$function_name == "GO", ]
df_p_mean_households_summarized_activity <- df_p_mean_households_summarized[df_p_mean_households_summarized$function_name == "CONTEXT-SELECT-ACTIVITY", ]
df_p_mean_households_summarized_full_assocc <- df_p_mean_households_summarized[df_p_mean_households_summarized$function_name == full_assocc_deliberation, ]

# Create a dataframe that contains a column context, function_name, households, incl_t_ms
if (unique(df_p_mean_households_summarized_activity$context == df_p_mean_households_summarized_full_assocc$context) &
    unique(df_p_mean_households_summarized_activity$households == df_p_mean_households_summarized_full_assocc$households))
{
  df_p_mean_households_overhead_vector <- df_p_mean_households_summarized_activity$incl_t_ms - df_p_mean_households_summarized_full_assocc$incl_t_ms
  df_p_mean_households_summarized_overhead <- data.frame(context = df_p_mean_households_summarized_activity$context,
                                                         households = df_p_mean_households_summarized_activity$households,
                                                         incl_t_ms = df_p_mean_households_overhead_vector)
}

# Now I want to make a line plot for the incl time for the different context depths, with the y axis being the incl time and the x axis being the households

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_execution_time_households_go.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

plot_n_agents_time_go <- ggplot(df_p_mean_households_summarized_go, aes(x = households, y = incl_t_ms, group = context, color = context)) +
  geom_line() +
  geom_point() +
  labs(title = "Execution time GO for different context-depths",
       x = "Households",
       y = "Incl time",
       color = "Context-Depth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
show(plot_n_agents_time_go)

if (plot_type == "one") { dev.off() }

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_execution_time_households_select_activity.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

plot_n_agents_time_select_activity <- ggplot(df_p_mean_households_summarized_activity, aes(x = households, y = incl_t_ms, group = context, color = context)) +
  geom_line() +
  geom_point() +
  labs(title = "Execution time Context-Select-Activity for different context-depths",
       x = "Households",
       y = "Incl time",
       color = "Context-Depth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
show(plot_n_agents_time_select_activity)

if (plot_type == "one") { dev.off() }

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_execution_time_households_full_assocc.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

plot_n_agents_time_full_assocc <- ggplot(df_p_mean_households_summarized_full_assocc, aes(x = households, y = incl_t_ms, group = context, color = context)) +
  geom_line() +
  geom_point() +
  labs(title = "Execution time Full ASSOCC for different context-depths",
       x = "Households",
       y = "Incl time",
       color = "Context-Depth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
show(plot_n_agents_time_full_assocc)

if (plot_type == "one") { dev.off() }

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_execution_time_households_overhead.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

plot_n_agents_time_full_assocc <- ggplot(df_p_mean_households_summarized_overhead, aes(x = households, y = incl_t_ms, group = context, color = context)) +
  geom_line() +
  geom_point() +
  labs(title = "Execution time Deliberation Overhead for different context-depths",
       x = "Households",
       y = "Extra time",
       color = "Context-Depth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
show(plot_n_agents_time_full_assocc)

if (plot_type == "one") { dev.off() }

if (plot_type == "one") { pdf(paste("plot_", directory_files, "_profiler_execution_time_households_select_activity_cd_5.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height, pointsize=12) }

plot_n_agents_time_select_activity_cd_5 <- ggplot(df_p_mean_households_summarized_activity[df_p_mean_households_summarized_activity$context==5, ], 
                                             aes(x = households, y = incl_t_ms, group = context, color = context)) +
  geom_line() +
  geom_point() +
  labs(title = "Execution time Context-Select-Activity for different context-depths",
       x = "Households",
       y = "Incl time",
       color = "Context-Depth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("blue"))
show(plot_n_agents_time_select_activity_cd_5)

if (plot_type == "one") { dev.off() }

# Interpretation:
# - The go function is exponential for both context-depth of 5 and context-depth of 0, which most probably has to do with the contagiousness calculations.
# - The Select Activity and Full ASSOCC deliberation functions are way quicker for context-depth of 5 than context-depth of 0.

}