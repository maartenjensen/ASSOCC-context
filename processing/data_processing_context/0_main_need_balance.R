#--- LIBRARIES ---
# Install the libraries
#install.packages()

# Open the libraries
library(tidyverse)
library(ggplot2)
library(sjmisc)
library(readr)

#-------------------------------
#---     INITIALIZATION      ---
#-------------------------------

#--- WORKSPACE AND DIRECTORY ---
rm(list=ls()) 

#-   GENERAL PARAMETERS   -
filepath_workspace <- "D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context"

filenames_profiler <- c("report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= false].csv",
                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= true].csv",
                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= false].csv",
                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= true].csv")

filenames_realism  <- c("covid-sim ContextNeedBalancing-first-test.csv")

one_plot = TRUE

#-   CHANGE DIRECTORY   -
setwd(filepath_workspace)
getwd()

#--------------------------------------
#---    PROFILER - SCALABILITY      ---
#--------------------------------------
source("1_profiler_need_balance.R")
df_profiler = profilerLoadData(filepath_workspace, filenames_profiler)

df_profiler_csn = profilerLoadSpecificData(df_profiler, "CSN")
df_profiler_cssn = profilerLoadSpecificData(df_profiler, "CSSN")
df_profiler_cso = profilerLoadSpecificData(df_profiler, "CSO")
df_profiler_csso = profilerLoadSpecificData(df_profiler, "CSSO")
df_profiler_csowh = profilerLoadSpecificData(df_profiler, "CSSOWH")
df_profiler_csft = profilerLoadSpecificData(df_profiler, "CSFT")
df_profiler_cssft = profilerLoadSpecificData(df_profiler, "CSSFT")

profilerPlotNeedBalance(df_profiler, "", one_plot)

#--------------------------------------
#--- REALISM - BEHAVIOUR/INFECTIONS ---
#--------------------------------------
