#install.packages("sjmisc")

library(tidyverse)
library(ggplot2)
library(sjmisc)

#first empty working memory 
rm(list=ls()) 

setwd("D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context")
getwd()

### MANUAL INPUT: Optionally specify filepath (i.e. where the behaviorspace csv is situated) ###
#NOTE: if csv files are placed in the workdirec, then leave filesPath unchanged
filesPath <- "" 

#=================== MANUAL INPUT: specify filenames ====================
dataFileName <- c("report-[C= false -H= 100 -R= 1 -A= 6].csv",
                  "report-[C= false -H= 150 -R= 1 -A= 6].csv",
                  "report-[C= false -H= 200 -R= 1 -A= 6].csv",
                  "report-[C= false -H= 250 -R= 1 -A= 6].csv",
                  "report-[C= false -H= 300 -R= 1 -A= 6].csv",
                  "report-[C= false -H= 350 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 100 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 150 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 200 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 250 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 300 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 350 -R= 1 -A= 6].csv")
                  
filesNames   <- dataFileName

#=============================================================
#========================= LOAD DATA =========================
#=============================================================

p_files_path = filesPath
p_files_names = filesNames

print("- GO Function")
print("----------------------------------")
#read in datafiles using filesNames and filesPath variables
for (i in 1:length(p_files_names)) {
  file_name = p_files_names[i]
  file_path_name = paste(p_files_path, p_files_names[i], sep="")

  go = FALSE
  df_initial = read.csv(file_path_name, skip = 2)
  for (i in 1:nrow(df_initial)) {
    
    if (str_contains(df_initial[i,1], "GO ") && !go) {
      print(paste(file_name, df_initial[i,1], sep = ":    "))
      go = TRUE
    }
  }
}

#read in datafiles using filesNames and filesPath variables
print("- SELECT ACTIVITY Function")
print("----------------------------------")
for (i in 1:length(p_files_names)) {
  file_name = p_files_names[i]
  file_path_name = paste(p_files_path, p_files_names[i], sep="")
  
  select_activity = FALSE
  df_initial = read.csv(file_path_name, skip = 2)
  for (i in 1:nrow(df_initial)) {
    
    if (str_contains(df_initial[i,1], "SELECT-ACTIVITY ") && !str_contains(df_initial[i,1], "CONTEXTUAL") && !select_activity) {
      print(paste(file_name, df_initial[i,1], sep = ":    "))
      select_activity = TRUE
    }
  }
}
