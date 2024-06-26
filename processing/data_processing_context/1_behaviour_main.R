#--- WORKSPACE AND DIRECTORY ---
if (exists("libraries_loaded") && getwd() != "C:/Users/maart/OneDrive/Documenten") 
{ libraries_loaded <- TRUE } 

setwd(paste("D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context", directory_files, sep="/"))
getwd()

source("../1_behaviour_support.R")
source("../1_behaviour_plots_1_deliberation_type.R")
source("../1_behaviour_plots_2_infections.R")
source("../1_behaviour_plots_3_quarantiners.R")
source("../1_behaviour_plots_4_needs.R")
source("../1_behaviour_plots_5_location_types.R")
source("../1_behaviour_plots_6_activities.R")

behaviourLoadLibraries()

#-------------------------------
#---     Setup variables     ---
#-------------------------------

# Directory files
#directory_files <- "2024-06-21-activities-normative"
directory_files <- "2024_06_24_full_exp_single_runs"

# Plot type
plot_type <- "none" # Generate no pdf's, just generate it in the viewer
#plot_type <- "one" # One plot per pdf
plot_type <- "all" # All plots in one pdf

# If this is true, then deliberation_type plots are not generated
limit_plots <- FALSE 

# True: plot 1.1 rigid-habits-, 1.2 rigid-habits-infected
specific_experiment_presets <- c("2.2 DCSD-2-obligation-constraint", "5.1 DCSD-5-optimisation") 

# Plots general size
gl_pdf_width = 9
gl_pdf_height = 6


#-------------------------------
#---     Fixed variables     ---
#-------------------------------

filesPath <- "" 
filesNames   <- c(paste(directory_files, "csv", sep = "."))

#=============================================================
#==================== PREPARE DATAFRAME ======================
#=============================================================

df_initial = behaviourLoadDataframe(filesPath, filesNames)
df_final <- behaviourRenameDataframe(df_initial)

# Filter on one random seed, since this is a single run
random_seed = 0
df_final_filtered <- df_final[df_final$random_seed == random_seed, ]

# Add a column for the amount of people that are alive, todo: just take the column of people (alive)
df_final_filtered$people_alive <- (df_final_filtered$youngs_at_start + df_final_filtered$students_at_start +
                                   df_final_filtered$workers_at_start + df_final_filtered$retireds_at_start) - df_final_filtered$dead_people

#=============================================================
#============= PLOT FOR LOOP FOR EVERYTHING ==================
#=============================================================
if (plot_type == "all") { pdf(paste("plot_", directory_files, "_behaviour_all_plots.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) }


#-----------------    CREATE THE FOR LOOP    -----------------
experiment_preset = unique(df_final_filtered$ce_context_experiment_presets)[1]   # Check the depth values and make dependent on the dataframe's depth levels

if (length(specific_experiment_presets) > 0)
{
  experiment_presets = specific_experiment_presets
} else {
  experiment_presets = unique(df_final_filtered$ce_context_experiment_presets)
}

for (experiment_preset in experiment_presets)
{
  # Start the plot creating loop
  print(paste("Plotting: Starting to print plots for ", experiment_preset))

  subset_df <- df_final_filtered[df_final_filtered$ce_context_experiment_presets == experiment_preset, ]
  
  depth_value = unique(subset_df$ce_context_depth)[1] # This is the same for the whole column since its data of ONE preset
  gl_limits_x_max <- subset_df$stop_before_tick[1]
  
  plot_base_name = paste("plot_", directory_files, "_behaviour_", experiment_preset, sep="")
  
  #=============================================================
  #=================== Plotting functions  =====================
  #=============================================================
  
  #-----------------    THE LINE PLOT: TIME    -----------------
  
  if (!limit_plots)
  {
    behaviourPlot1DeliberationType()
  }
  
  behaviourPlot2Infections()
  
  behaviourPlot3Quarantiners()
  
  behaviourPlot4Needs()
  
  behaviourPlot5LocationTypes()
  
  behaviourPlot6Activities()
  
}

if (plot_type == "all") { dev.off() }


#=============================================================
# -- Depricated -- 
# This little piece of code was there to check whether n at all locations summed was equal to the amount of people alive

# I have the df_location_types dataframe
# df_location_type contains the columns tick, ce_context_depth, at_essential_shops, at_homes, at_non_essential_shops, at_private_leisure, at_public_leisure, at_schools, at_universities, at_workplaces, at_treatment
# For each row I want to sum the columns at_essential_shops until at_treatment

#for (i in 1:nrow(df_location_types)) {
  
  #row = df_location_types[i,]
  #sum_row = sum(row$at_essential_shops, row$at_homes, row$at_non_essential_shops, row$at_private_leisure, row$at_public_leisure, row$at_schools, row$at_universities, row$at_workplaces, row$at_treatment)
  #print(sum_row)
#}

