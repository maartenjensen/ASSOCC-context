#install.packages("sjmisc")

library(tidyverse)
library(ggplot2)
library(sjmisc)
library(readr)

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

dataFileName <- c("report-[C= false -H= 350 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 350 -R= 1 -A= 6].csv")

filesNames   <- dataFileName

#=============================================================
#========================= LOAD DATA =========================
#=============================================================

p_files_path = filesPath
p_files_names = filesNames

#---------- CREATE DATAFRAME ------------#
df_results = data.frame(settings=NA, function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]

print("- GO Function")
print("----------------------------------")
#read in datafiles using filesNames and filesPath variables
for (i in 1:length(p_files_names)) {
  file_name = p_files_names[i]
  file_path_name = paste(p_files_path, p_files_names[i], sep="")

  df_initial = read.csv(file_path_name, skip = 2)
  for (i in 1:nrow(df_initial)) {
    
    if (str_contains(df_initial[i,1], "GO ") || str_contains(df_initial[i,1], "SELECT-ACTIVITY ") || 
        str_contains(df_initial[i,1], "CONTEXT") || str_contains(df_initial[i,1], "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR")) {
      print(paste(file_name, df_initial[i,1], sep = ":    "))
      result_v <- v_convert_result_to_float(str_to_v_without_white_spaces(df_initial[i,1]))
      df_results <- rbind(df_results, c(file_name, result_v[1], as.double(result_v[2]), result_v[3], result_v[4], result_v[5]))
    }
    if (str_contains(df_initial[i,1], "Sorted by Inclusive Time")) {
      break
    }
  }
}
#---------- MAKING THE DATAFRAME NICE ---------#
colnames(df_results) <- c("settings", "function_name", "calls", "incl_t_ms", "excl_t_ms", "excl_calls")
df_results$calls = as.double(df_results$calls)
df_results$incl_t_ms = as.double(df_results$incl_t_ms)
df_results$excl_t_ms = as.double(df_results$excl_t_ms)
df_results$excl_calls = as.double(df_results$excl_calls)
print(df_results)

df_results <- df_results[order(df_results$settings, df_results$function_name),]

str_to_v_without_white_spaces <- function(p_str) {
  t_vector <- c()
  t_str = ""
  
  str_split <- strsplit(p_str, "")[[1]]
  for(char in str_split) {
    if (char == ' ') {
      if (t_str != "") {
        t_vector <- c(t_vector, t_str)
        t_str = ""
      }
    }
    else {
      t_str = paste(t_str, char, sep = "")
    }
  }
  t_vector <- c(t_vector, t_str)
  return(t_vector)
}

plot_ggplot_deliberation_type <- function(data_to_plot, p_title, p_limits) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement,
               group = DelibType,
               fill = DelibType), fill=NA) +
    geom_line(aes(col=DelibType)) +
    xlab("Ticks") +
    ylab("Used by n agents") +
    labs(title=p_title) +
    guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1))) +
    gl_plot_theme + p_limits + scale_color_manual(values=c('#000000', '#E69F00', '#f16a15', '#8d8d8d', '#345da9'))
}
