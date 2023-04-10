# ASSOCC
# R Coding standard
# - Variables: with line _, e.g. df_test_2
#   * p_test (parameter variable)
#   * gl_test (global variable)
#   * df_test (dataframe variable)
#   * v_test (vector variable)
# - Functions: camelCase, e.g. loadData

#=============================================================
#========================== STARTUP ==========================
#=============================================================

#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("tidyr")
#install.packages("tidyverse")
#install.packages("dplyr")  
#install.packages("here")
#install.packages("plyr") 
#install.packages("viridis")  # Install

#first empty working memory 
rm(list=ls()) 
 
#then load relevant libraries
library(here)
### The manual input of setting the directory is changed with the here() function, this function automatically finds the workplace ###
# If the program gives an error here please reload R so it unloads all the packages (plyr also has a function called here())
# detach("package:plyr", unload=TRUE)
#setwd(here()) 
getwd() 
library(plyr) # the plyr library also create a here() function, therefore we use the here() function before, its pretty cheap but it works
library(plotly) 
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr) 
library("viridis")           # Load
 
setwd("D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context")
getwd()

#=============================================================
#========================= SET FILES =========================
#=============================================================
 
#Make sure the R script with functions is placed in the working directory!
rm(list=ls())
source("S6_1_dataframe_functions.r")

### MANUAL INPUT: Optionally specify filepath (i.e. where the behaviorspace csv is situated) ###
#NOTE: if csv files are placed in the workdirec, then leave filesPath unchanged
filesPath <- "" 

#=================== MANUAL INPUT: specify filenames ====================
#dataFileName <- c("output-s6-app-usage-87220b5.csv")
dataFileName <- c("covid-sim S6-table test-run-context.csv")
randomTesting <- FALSE

filesNames   <- dataFileName

#=============================================================
#========================= LOAD DATA =========================
#=============================================================

df <- dlfLoadData(filesPath, filesNames)

#========================== Drop wrong columns ==========================
#remove duplicate variable (X.contacts.in.pubtrans.1)
drop_names <- c("X.contacts.in.pubtrans.1")
#print(paste("Removing the following columns manually:", drop_names))
#drop irrelevant variables
#df_clean <- dplyr::select(df,-c(drop_names))
df_clean <- df

#========================== Rename the columns ==========================
old_variable_names <- names(df_clean)

df_renamed <- updateColumnNames(df_clean)

colnames(df_renamed)[match("step", colnames(df_renamed))] = "tick";
colnames(df_renamed)[match("ratio_of_people_using_the_tracking_app", colnames(df_renamed))] = "ratio_of_app_users";
colnames(df_renamed)[match("ratio_of_anxiety_avoidance_tracing_app_users", colnames(df_renamed))] = "ratio_of_anxiety_app_users";
colnames(df_renamed)[match("count_people_with_epistemic_infection_status_infected", colnames(df_renamed))] = "believe_infected";

df_names_compare <- data.frame("new" = names(df_renamed), "old" = old_variable_names)
print("Renamed the dateframe, please check the df_names_compare dataframe for correct column translation")

#========================== Remove invalid runs ==========================
#runs that have a lower amount of maximum infected are seen as invalid as the virus did not spread and are therefore removed
#specify the minimum number of infected people for a run to be considered as valid (5 person by default)
minimal_number_infected = 5
df_renamed_infected_remove <- dlfRemoveFailedInfectionsRuns(df_renamed, minimal_number_infected)

# If runs are not finished, if they do not go until the final tick amount they are removed
df_renamed_infected_and_unfinished_remove <- dlfRemoveUnfinishedRuns(df_renamed_infected_remove)
df_scenario6 <- df_renamed_infected_and_unfinished_remove 

#========================== Show invalid runs report ==========================
p_df_full         <- df_renamed %>% select(run_number:tick, -random_seed)
p_df_removed_runs <- df_renamed_infected_and_unfinished_remove %>% select(run_number:tick, -random_seed)
df_included_runs_report  <- dlfCreateIncludedRunsReport(p_df_full, p_df_removed_runs)

#========================== Clean up variables ==========================
rm(list = c("drop_names", "df", "df_clean", "df_renamed", "old_variable_names", "p_df_full", 
            "df_renamed_infected_and_unfinished_remove", "df_renamed_infected_remove", "p_df_removed_runs"))

# Determine start and end of global quarantine 
max_run_number <- max(df_scenario6$tick) # get max run number
v_start_quaran <- df_scenario6$start_tick_of_global_quarantine[df_scenario6$tick==max_run_number] #put the start_tick_of_global_quarantine
v_start_quaran <- v_start_quaran[v_start_quaran!="never"] # remove never if it is in there (probably not)
gl_mean_start_quaran_tick <- mean( as.numeric(as.character(v_start_quaran)) ) # change the vector so we can calculate with it and take the mean

v_end_quaran <- df_scenario6$end_tick_of_global_quarantine[df_scenario6$tick==max_run_number] #put the end_tick_of_global_quarantine
v_end_quaran <- v_end_quaran[v_end_quaran!="never"] # remove never if it is in there (probably not)
gl_mean_end_quaran_tick <- mean( as.numeric(as.character(v_end_quaran)) ) # change the vector so we can calculate with it and take the mean


#=============================================================
#========= DATAMANIPULATION FOR VALIDATION CHAPTER ===========
#=============================================================
#df_scenario6$tick <- df_scenario6$tick - gl_mean_start_quaran_tick
bool_for_oxford_comparison <- FALSE

#=============================================================
#========================== !PLOTS! ==========================
#=============================================================
if (bool_for_oxford_comparison) {
  gl_pdf_width = 6
  gl_pdf_height = 6
} else {
  gl_pdf_width = 9
  gl_pdf_height = 6
}

plotAllPlots <- function() {

  # Create a new folder for the plots and csv's and set the output_dir as variable
  date_time = gsub(":", ".", gsub(" ", "_" , substr(Sys.time(),1,nchar(Sys.time()))))
  output_dir = paste("output_plots_", date_time, sep="")
  dir.create(output_dir)
  if (one_plot) {
    pdf(paste(output_dir, "/s6plots_", date_time, "_", str_remove(dataFileName[1], ".csv"), ".pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height)
  }
  
  dlfPlotIncludedRunsReport(df_included_runs_report, output_dir)
  
 
  if (!randomTesting) {
    source("S6_infected_compliance_tests.r")
    plotS6InfectedComplianceTests(df_scenario6, output_dir, one_plot)
    source("S6_quarantiners.r")
    plotS6Quarantiners(df_scenario6, output_dir, one_plot)
    source("S6_contacts_per_day.r")
    plotS6ContactsPerDay(df_scenario6, output_dir, one_plot)
    source("S6_cumulative_infections.r")
    plotS6CumulativeInfections(df_scenario6, output_dir, one_plot)
    #source("S6_detailed_app_tests.r")
    #plotS6DetailedAppTests(df_scenario6, output_dir, one_plot)
    source("S6_stacked_bar_ratio_infector_infectee.r")
    plotS6StackedBarRatioInfectorInfectee(df_scenario6, output_dir, one_plot)
    source("S6_stacked_bar_ratio_contacts_per_contacted.r")
    plotS6StackedBarRatioContactsPerContacted(df_scenario6, output_dir, one_plot)
    source("S6_hospital_admissions.r")
    plotS6HospitalAdmissions(df_scenario6, output_dir, one_plot)
    source("S6_infection_ratio_per_gathering_point_over_time.r")
    plotS6InfectionRatioGP(df_scenario6, output_dir, one_plot)
    source("S6_infectors_and_infectees.r")
    plotS6InfectorsAndInfectees(df_scenario6, output_dir, one_plot)
    source("S6_contacts_at_gathering_point.r")
    plotS6ContactsAtGatheringPoint(df_scenario6, output_dir, one_plot)
  }
  else {
    source("S6R_cumulative_infections.r")
    plotS6RCumulativeInfections(df_scenario6, output_dir, one_plot)
    source("S6R_contacts_at_gathering_point.r")
    plotS6RContactsAtGatheringPoint(df_scenario6, output_dir, one_plot)
    source("S6R_infection_ratio_per_gathering_point_over_time.r")
    plotS6RInfectionRatioGP(df_scenario6, output_dir, one_plot)
  }
  
  if (one_plot) {
    dev.off()
  }
}

#========================== Plot settings ==========================
# Specify whether to have all plots in one pdf (TRUE) or in separate files (FALSE)
one_plot <- TRUE
 
if (!randomTesting) {
  gl_plot_variable_name <- "App users ratio"
} else {
  gl_plot_variable_name <- "Random daily tests"
}

# Adjust names, theme and the scales of the plots
if (bool_for_oxford_comparison) {
  multiplier = 2/3
  gl_plot_theme  <-  theme_bw() + theme(legend.position="bottom",
                                        axis.text = element_text(size = rel(1.3)),
                                        axis.title = element_text(size = rel(1.3)),
                                        legend.text = element_text(size = rel(1.1)),
                                        legend.title = element_text(size = rel(1.1 )),
                                        title = element_text(size = rel(1.3 * multiplier)) )
  
} else {
  multiplier = 1
  gl_plot_theme  <-  theme_bw() + theme(legend.position="bottom",
                                        axis.text = element_text(size = rel(1.3 * multiplier)),
                                        axis.title = element_text(size = rel(1.3 * multiplier)),
                                        legend.text = element_text(size = rel(1.2 * multiplier)),
                                        legend.title = element_text(size = rel(1.2 * multiplier)),
                                        title = element_text(size = rel(1.3 * multiplier)) )
  
}

gl_plot_guides <- guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1)))

gl_plot_guides_side <- guides(colour = guide_legend(override.aes = list(size=5, alpha=1)))
gl_plot_theme_side  <-  theme_bw() + theme(legend.position="right")

if (bool_for_oxford_comparison) { #This thing
  gl_x_lim_days <- c(-45, 140)
  gl_mean_end_quaran_tick <- gl_mean_end_quaran_tick - gl_mean_start_quaran_tick
  gl_mean_end_quaran_day  <- gl_mean_end_quaran_tick / 4
  gl_mean_start_quaran_tick <- 0
  gl_mean_start_quaran_day <- 0
} else {
  gl_x_lim_days <- c(0, 375)
  gl_mean_start_quaran_day <- gl_mean_start_quaran_tick / 4
  gl_mean_end_quaran_day   <- gl_mean_end_quaran_tick / 4
}

df_compliance_tests_random <- read.csv("plot_data_s6_infected_compliance_tests.csv", header = TRUE)
df_for_testing_line <- df_compliance_tests_random %>% select(tick, ratio_population_randomly_tested_daily, tests_performed)
df_for_testing_line <- df_for_testing_line %>% filter(ratio_population_randomly_tested_daily == 0.0119)

df_for_testing_1perc <- 1126 * (1/100);
df_for_testing_2perc <- 1126 * (2/100);
df_for_testing_5perc <- 1126 * (5/100);

gl_col_youth   = '#cedecb'
gl_col_student = '#de9236'
gl_col_worker  = '#16727e'
gl_col_retired = '#391c1c'

gl_col_1 = '#000000' # black
gl_col_2 = '#CC0000' # dark red
gl_col_3 = '#3399FF' # blue
gl_col_4 = '#55FF55' # light green
  
# Specify the independent variable, the variable to separate the data on
plotAllPlots()
#plotAllPlots(ratio_population_randomly_tested_daily)






















#Simple dataset extraction
df_simple <- df_scenario6 %>% 
  group_by(tick, ratio_of_app_users) %>% 
  summarise(infected = mean(infected, na.rm = TRUE),
            infected_this_tick = mean(infected_this_tick, na.rm = TRUE),
            dead_people = mean(dead_people, na.rm = TRUE),
            tests_performed = mean(tests_performed, na.rm = TRUE),
            count_youngs = mean(count_youngs),
            count_students = mean(count_students),
            count_workers = mean(count_workers),
            count_retireds = mean(count_retireds))

write.csv(df_simple, file="data_ASSOCC_tracking_tracing_averaged_50_runs.csv")
