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

gl_pdf_width  = 9
gl_pdf_height = 5.5

# One of: "none", "one", "all"
plot_type <- "none"
plot_type <- "one" 
#plot_type <- "all"

directory_r <- "D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context"

directory_files <- "2025_04_01_scalability_lvanhee"
pdf_output_name <- "2025_04_01_scalability_lvanhee"

#--- WORKSPACE AND DIRECTORY ---
#-   CHANGE DIRECTORY   -
setwd(paste(directory_r, directory_files, sep="/"))
getwd()

source("../0_profiler_lois_assocc_support.R")

# C = context depth, H = households, A = action space, R = random seed

n_experiments_active = 1
random_seeds = c("2")[1:n_experiments_active]


if (directory_files == "2025_04_01_scalability_lvanhee")
{
  filenames_profiler <- retrieve_filenames_profiler(c("profiler_lvanhee"),
                                                    c("350", "700", "1400", "2100", "2800", "3500"),
                                                    c("6"), random_seeds)
}

#--------------------------------------
#---    LOAD ALL PROFILER DATA      ---
#--------------------------------------
p_filepath_workspace <- paste(directory_r, directory_files, sep="/")
p_filenames_profiler <- filenames_profiler

df_profiler = profilerLoadData(paste(directory_r, directory_files, sep="/"), filenames_profiler)
df_p_overview = profilerLoadSpecificData(df_profiler, c("GO", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR", "CONTEXT-SELECT-ACTIVITY", "SELECT-ACTIVITY"))
df_p_overview = profilerPrepareOverviewDataframe(df_p_overview)
df_p_overview_mean = profilerGetOverviewMean(df_p_overview)

#==============================================
#==== PLOTTING                              ===
#==============================================
source("../0_profiler_lois_assocc_plots.R")


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#====          SELECT UNTIL HERE            ===
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





#==============================================
# GO
#==============================================
df_p_overview_mean_GO <- df_p_overview_mean[df_p_overview_mean$function_name == "GO", ]
#plot_time_comparison_go(df_p_overview_mean_GO, pdf_output_name, n_experiments_active, plot_type) 

#==============================================
# SELECT ACTIVITY
#==============================================
df_p_overview_mean_CONTEXT_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]
#plot_time_comparison_deliberation(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY, pdf_output_name, n_experiments_active, plot_type)



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
plot_time_comparison_original_assocc(df_time_original_assocc, pdf_output_name, n_experiments_active, plot_type)

# the table data
df_time_original_assocc



#==============================================
# SELECT ACTIVITY
#==============================================
df_p_overview_mean_CONTEXT_SELECT_ACTIVITY <- df_p_overview_mean[df_p_overview_mean$function_name == "CONTEXT-SELECT-ACTIVITY", ]

# Rename preset
df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$preset <- c("Deliberation Time", "Deliberation Time", "Deliberation Time", 
                                                       "Deliberation Time", "Deliberation Time", "Deliberation Time")


plot_time_comparison_deliberation(df_p_overview_mean_CONTEXT_SELECT_ACTIVITY, pdf_output_name, n_experiments_active, plot_type)




#== Data Printing ==

print("Data printing 11111111")
# The Original ASSOCC Line
x = df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$agents[1:6]
y = df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean[1:6]
mod <- lm(y ~ x)
cf <- coef(mod)
print("Linear Equation for Original ASSOCC")
cat("y = ", cf["x"], "x + ", cf["(Intercept)"], sep="")

# The DCSD ASSOCC Line
x = df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$agents[7:12]
y = df_p_overview_mean_CONTEXT_SELECT_ACTIVITY$incl_t_ms_mean[7:12]
mod <- lm(y ~ x)
cf <- coef(mod)
print("Linear Equation for DCSD ASSOCC")
cat("y = ", cf["x"], "x + ", cf["(Intercept)"], sep="")

print("Solving the linear equation, gives us ... agents. This is about 20x more agents within the same deliberation time when compared to Original ASSOCC.
      Since both lines are linear, this property holds also with larger or smaller numbers of agents. Although there might be some inefficiencies with
      much larger agent numbers, generally, this property holds.")

# 116579.3 = 5.591231x + 625.2249
# 115954.1 = 5.591231x
# x = 20738.56

#================================
# Printing the speed up table
#================================
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
# GO
#==============================================
df_p_overview_mean_GO <- df_p_overview_mean[df_p_overview_mean$function_name == "GO", ]
plot_time_comparison_go(df_p_overview_mean_GO, pdf_output_name, n_experiments_active, plot_type) 


#==============================================
# FULL ASSOCC DELIBERATION
#==============================================
df_p_overview_mean_FULL_ASSOCC_DELIBERATION <- df_p_overview_mean[df_p_overview_mean$function_name == "FULL ASSOCC DELIBERATION", ]
plot_time_comparison_full_assocc(df_p_overview_mean_FULL_ASSOCC_DELIBERATION, pdf_output_name, n_experiments_active, plot_type)

#== Data Printing ==
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

#== PLOT DETAILED DCSD ==
plot_time_comparison_dcsd_detailed(df_p_overview_mean_DCSD_selection, pdf_output_name, n_experiments_active, plot_type)

#== PRINT DATA ==
df_p_overview_mean_DCSD_selection



#==============================================
#==============================================
# PLOT: Total, Deliberation and Non-Deliberation Time
#==============================================
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
plot_time_comparison_original_assocc(df_time_original_assocc, pdf_output_name, n_experiments_active, plot_type)

# plot for DCSD ASSOCC
df_time_dcsd_assocc <- gather(df_incl_t_ms_mean_all[7:12, c(1,2,4,5)], time_type, incl_t_ms_mean, `Deliberation Time`:`Non-Deliberation Time`)
plot_time_comparison_dcsd_assocc(df_time_dcsd_assocc, pdf_output_name, n_experiments_active, plot_type)

#==============================================
#==============================================
# PLOT: Estimated total execution time of ASSOCC AND Non-deliberation time of ASSOCC + Deliberation time of DCSD
#==============================================
#==============================================
dev.off()

df_incl_t_ms_mean_all$`Total Time New` = df_incl_t_ms_mean_all$`Total Time`
df_incl_t_ms_mean_all$`Total Time New`[7:12] = df_incl_t_ms_mean_all$`Non-Deliberation Time`[1:6] + df_incl_t_ms_mean_all$`Deliberation Time`[7:12]

plot_estimated_total_execution_time(df_incl_t_ms_mean_all, pdf_output_name, n_experiments_active, plot_type)

#https://www.statology.org/quadratic-regression-r/

print("Quadratic equation")
# ORIGINAL ASSOCC LINE
dataOriginal <- data.frame(x=df_incl_t_ms_mean_all$Agents[1:6], y=df_incl_t_ms_mean_all$`Total Time New`[1:6])
x = dataOriginal$x
x2 = x^2
y = dataOriginal$y
quadraticModelOriginal <- lm(y ~ x + x2, data=dataOriginal)
cfOriginal <- coef(quadraticModelOriginal)
agentsValuesOriginal <- seq(0, 12000, 10)
timePredictOriginal <- predict(quadraticModelOriginal,list(x=agentsValuesOriginal, x2=agentsValuesOriginal^2))

plot(dataOriginal$x, dataOriginal$y, pch=16)
#add predicted lines based on quadratic regression model
lines(agentsValuesOriginal, timePredictOriginal, col='blue')

# DCSD ASSOCC LINE
dataDCSD <- data.frame(x=df_incl_t_ms_mean_all$Agents[7:12], y=df_incl_t_ms_mean_all$`Total Time New`[7:12])
x = dataDCSD$x
x2 = x^2
y = dataDCSD$y
quadraticModelDCSD <- lm(y ~ x + x2, data=dataDCSD)
cfDCSD <- coef(quadraticModelDCSD)
agentsValuesDCSD <- seq(0, 12000, 10)
timePredictDCSD <- predict(quadraticModelDCSD,list(x=agentsValuesDCSD, x2=agentsValuesDCSD^2))

#plot(dataDCSD$x, dataDCSD$y, pch=16)
#add predicted lines based on quadratic regression model
lines(agentsValuesDCSD, timePredictDCSD, col='red')


print("Quadratic Equation for Original ASSOCC")
cat("y = ", cfOriginal["x2"], "*(x)^2 + ", cfOriginal["x"], "*(x) + ", cfOriginal["(Intercept)"], sep="")

print("Quadratic Equation for DCSD ASSOCC")
cat("y = ", cfDCSD["x2"], "*(x)^2 + ", cfDCSD["x"], "*(x) + ", cfDCSD["(Intercept)"], sep="")

timePredictOriginal[100]
timePredictDCSD[144]
timePredictOriginal[1000]
timePredictDCSD[1072]

# https://www.khanacademy.org/math/algebra/x2f8bb11595b61c86:quadratic-functions-equations/x2f8bb11595b61c86:quadratic-formula-a1/a/quadratic-formula-explained-article

n = 1000
a1 = cfOriginal["x2"]
b1 = cfOriginal["x"]
c1 = cfOriginal["(Intercept)"]
y = (a1*(n)^2 + b1*(n) + cfOriginal["(Intercept)"])


leftPart = -1 * b1
rightPart = sqrt(b1^2 - 4*a1*c1)
leftPartFinal = leftPart / (2 * a1)
rightPartFinal = rightPart / (2 * a1)
leftPartFinal
rightPartFinal



#==============================================
#==============================================
# PLOT: Required speed-up of non-deliberation time to be equal to deliberation time
#==============================================
#==============================================

# Calculate the speed up factor
df_incl_t_ms_mean_all$`Required Speed-Up` = round(df_incl_t_ms_mean_all$`Non-Deliberation Time`/df_incl_t_ms_mean_all$`Deliberation Time`, 1)

#=== SPEED UP NORMAL ===
plot_possible_speed_up_normal(df_incl_t_ms_mean_all, pdf_output_name, n_experiments_active, plot_type)


# From the figures above it can be seen that the Non-Deliberation time of Original ASSOCC
# is about twice as much as the Non-Deliberation of DCSD ASSOCC. Thus what if we take the
# Original ASSOCC Non-Deliberation time and calculate how much the speed-up factor required is then.

#=== SPEED UP ORIGINAL ASSOCC ONLY ===
df_incl_t_ms_mean_all$`Required Speed-Up Original` = df_incl_t_ms_mean_all$`Required Speed-Up`
df_incl_t_ms_mean_all$`Required Speed-Up Original`[7:12] = round(df_incl_t_ms_mean_all$`Non-Deliberation Time`[1:6]/df_incl_t_ms_mean_all$`Deliberation Time`[7:12], 1)
df_incl_t_ms_mean_all

plot_possible_speed_up_original_assocc(df_incl_t_ms_mean_all, pdf_output_name, n_experiments_active, plot_type)