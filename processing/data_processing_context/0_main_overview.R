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
  libraries_loaded = TRUE
}

#-------------------------------
#---     INITIALIZATION      ---
#-------------------------------

#--- WORKSPACE AND DIRECTORY ---
rm(list=ls()) 
libraries_loaded = TRUE

#-   GENERAL PARAMETERS   -
filepath_workspace <- "D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context"

# filenames_profiler <- c("report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= false].csv",
#                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= false].csv",
#                        "report-[C= true -H= 350 -R= 1 -A= 6 -N= true -PR= true].csv")
# #"report-[C= true -H= 350 -R= 1 -A= 6 -N= false -PR= true].csv" is omitted.

filenames_profiler <- c("report-[C= 0 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 0 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 0 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 1 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 1 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 1 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 2 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 2 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 2 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 3 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 3 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 3 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 4 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 4 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 4 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 5 -H= 350 -R= 1 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 5 -H= 350 -R= 2 -A= 6 -L= false -DCC= false -SRFQ= false].csv",
                        "report-[C= 5 -H= 350 -R= 3 -A= 6 -L= false -DCC= false -SRFQ= false].csv")
                        #"report-[C= 0 -H= 350 -R= 1 -A= 6 -L= true].csv",
                        #"report-[C= 1 -H= 350 -R= 1 -A= 6 -L= false].csv",
                        #"report-[C= 1 -H= 350 -R= 1 -A= 6 -L= true].csv")

filenames_realism  <- c("covid-sim-small-full-test-scalability.csv") # covid-sim ContextNeedBalancing-first-test.csv")

one_plot = TRUE

#-   CHANGE DIRECTORY   -
setwd(filepath_workspace)
getwd()

#--------------------------------------
#---    PROFILER - SCALABILITY      ---
#--------------------------------------
source("1_profiler_overview.R")
df_profiler = profilerLoadData(filepath_workspace, filenames_profiler)

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
