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
                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= false].csv",
                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= true].csv")
# #"report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= true].csv" is omitted.

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

df_profiler_overview = profilerLoadSpecificData(df_profiler, c("GO", "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR", "CONTEXT-DELIBERATION-SELECT-ACTIVITY"))

df_profiler_csn = profilerLoadSpecificData(df_profiler, "CSN")
df_profiler_cssn = profilerLoadSpecificData(df_profiler, "CSSN")
df_profiler_cso = profilerLoadSpecificData(df_profiler, "CSO")
df_profiler_csso = profilerLoadSpecificData(df_profiler, "CSSO")
df_profiler_csowh = profilerLoadSpecificData(df_profiler, "CSSOWH")
df_profiler_csft = profilerLoadSpecificData(df_profiler, "CSFT")
df_profiler_cssft = profilerLoadSpecificData(df_profiler, "CSSFT")

# A plotting function to plot the comparison in speed. I can always just plot this, 
# but for this case its not so relevant. What I can actually do is plot the specific
# calls of functions, such as the leisure functions.
profilerPlotNeedBalance(df_profiler, "", one_plot)

#--------------------------------------
#--- REALISM - BEHAVIOUR/INFECTIONS ---
#--------------------------------------

# I'll do this one later.

#--------------------------------------
#-  SUMMARY SCALABILITY AND REALISM   -
#--------------------------------------

profilerSummarize(df_profiler, df_profiler_overview)
{
  
}

#--------------------------------------
#-       GOALS FOR THIS PROGRAM       -
#--------------------------------------
# It needs to be easy to adjust things like which data to get out, but I guess this can be done in plotting
# I want to focus on the realism plots, but I should still look at the profiler data to see if I really got everything out of it.
