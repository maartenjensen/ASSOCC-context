#first empty working memory 
rm(list=ls()) 


setwd("D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context")
getwd()


#Make sure the R script with functions is placed in the working directory!
#source("S6_1_dataframe_functions.r")

### MANUAL INPUT: Optionally specify filepath (i.e. where the behaviorspace csv is situated) ###
#NOTE: if csv files are placed in the workdirec, then leave filesPath unchanged
filesPath <- "" 

#=================== MANUAL INPUT: specify filenames ====================
#dataFileName <- c("output-s6-app-usage-87220b5.csv")
dataFileName <- c("covid-sim ContextExperiment-table.csv")

filesNames   <- dataFileName

#=============================================================
#========================= LOAD DATA =========================
#=============================================================

p_files_path = filesPath
p_files_names = filesNames
  
#read in datafiles using filesNames and filesPath variables
for (i in 1:length(p_files_names)) {
  print(paste("read csv from:", p_files_path, p_files_names[i], sep=""))
  #bind data from dataframe into new dataframe
  if (exists('t_df') && is.data.frame(get('t_df'))) {
    temp_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",",head=TRUE, stringsAsFactors = TRUE)
    temp_df$X.run.number. <- temp_df$X.run.number + max_run_number
    t_df <- rbind(t_df, temp_df)
  } else {
    t_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",",head=TRUE, stringsAsFactors = TRUE)
  }
  max_run_number <- max(t_df$X.run.number.)
}
df = t_df

