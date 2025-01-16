# The single sign version | returns an entire vector. The double sign version || returns the result of the OR operator on the first element of each vector.
#--- WORKSPACE AND DIRECTORY ---
libraries_need_to_be_loaded <- FALSE
if (!exists("libraries_loaded") || libraries_loaded == FALSE || getwd() == "C:/Users/maart/OneDrive/Documenten") 
{ libraries_need_to_be_loaded <- TRUE } 

# Directory files
directory_files <- "2024_07_01_full_exp_single_runs"
#directory_files <- "2024_07_15_full_single_runs"
directory_files <- "2024_07_18_realism"
directory_files <- "2024_07_11_full_three_runs"
directory_files <- "2024_07_30_comparison"
directory_files <- "2024_07_31_comparison"
directory_files <- "2024_08_01_comparison"
#directory_files <- "2024_08_08_lockdown"
#directory_files <- "2024_08_09_comparison"
directory_files <- "2024_08_12_comparison_full"
directory_files <- "2024_08_14_comparison_extreme"
directory_files <- "2024_12_19_realism"
directory_files <- "2025_01_13_criteria_multi"
directory_files <- "2025_01_14_criteria_multi"

criteria_get_mean_off_runs <- FALSE # FALSE: Use the single random seed run

setwd(paste("D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context", directory_files, sep="/"))
getwd()

source("../1_behaviour_support.R")
source("../1_behaviour_plots_1_deliberation_type.R")
source("../1_behaviour_plots_2_infections.R")
source("../1_behaviour_plots_3_quarantiners.R")
source("../1_behaviour_plots_4_needs.R")
source("../1_behaviour_plots_5_location_types.R")
source("../1_behaviour_plots_6_activities.R")
source("../1_behaviour_plots_7_mean_of_activities.R")
source("../1_behaviour_plots_8_measure_criteria.R")

libraries_loaded <- behaviourLoadLibraries(libraries_need_to_be_loaded)

#-------------------------------
#---     Setup variables     ---
#-------------------------------

# Plot type
plot_type <- "none" # Generate no pdf's, just generate it in the viewer
plot_type <- "one" # One plot per pdf
#plot_type <- "all" # All plots in one pdf

# Create functions that have to be called
plot_specifics_only <- TRUE # if there are specific plots for the setting, only plot specific plots

plot_full_functions <- c("behaviourPlot1DeliberationType", "behaviourPlot1DeliberationTypeBar",
                         "behaviourPlot2Infections", "behaviourPlot2InfectionsBelieveInfected",
                         "behaviourPlot3Quarantiners",
                         "behaviourPlot4Needs",
                         "behaviourPlot5LocationTypes",
                         "behaviourPlot6Activities", "behaviourPlot6ActivitiesSimplified4RestAndWorkHome")
# There are more!

random_seed = 0 # This was set to 2

# To determine later! Probably I need to change the plots of 1 until 2, that show the activities to have a combined rest and work at home! But let's see whether it changes later on.
plot_specifics_h <- hash()
plot_specifics_h[["1.1 rigid-habits-no-infected"]] <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", 
                                                        "behaviourPlot2InfectionsBelieveInfected")
plot_specifics_h[["1.2 rigid-habits-infected"]]    <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", 
                                                        "behaviourPlot2InfectionsBelieveInfected")
plot_specifics_h[["1.3 DCSD-1"]]                   <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                        "behaviourPlot6ActivitiesSimplified4Leisure",
                                                        "behaviourPlot4Needs", "behaviourPlot4NeedsLeisureAndShopping", "behaviourPlot4NeedsLeisureAndShoppingIndividual")

plot_specifics_h[["1.4 DCSD-1-leisure-habits"]]    <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                        "behaviourPlot6ActivitiesSimplified4Leisure",
                                                        "behaviourPlot6ActivitiesWorkStudy", "behaviourPlot3QuarantinersAgeGroup") # For comparison in CH2

plot_specifics_h[["2.1 DCSD-2"]]                   <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                        "behaviourPlot6ActivitiesWorkStudy", "behaviourPlot3QuarantinersAgeGroup")
plot_specifics_h[["2.2 DCSD-2-obligation-constraint"]]  <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected", "behaviourPlot3QuarantinersAgeGroup",
                                                        "behaviourPlot6ActivitiesWorkStudy", "behaviourPlot3QuarantinersAgeGroup")


plot_specifics_h[["3.1 DCSD-3-rigid-norms"]]       <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                        "behaviourPlot3Quarantiners")
plot_specifics_h[["3.2 DCSD-3-rigid-norms-lockdown"]] <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                           "behaviourPlot3Quarantiners")
plot_specifics_h[["3.3 DCSD-3"]]                      <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                           "behaviourPlot3Quarantiners", "behaviourPlot6ActivitiesSimplified4Leisure")
plot_specifics_h[["3.4 DCSD-3-lockdown"]]             <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                           "behaviourPlot3Quarantiners", "behaviourPlot6ActivitiesSimplified4Leisure")

plot_specifics_h[["4.1 DCSD-4"]]                   <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHomeDay", "behaviourPlot2InfectionsBelieveInfected",
                                                        "behaviourPlot3Quarantiners", "behaviourPlot1DeliberationType", "behaviourPlot1DeliberationType",
                                                        "behaviourPlot1DeliberationTypeConformity")
#plot_specifics_h <- hash()
# plot_specifics_h[["5.1 DCSD-5-optimisation"]]      <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHomeDay", "behaviourPlot2InfectionsBelieveInfected",
#                                                         "behaviourPlot3Quarantiners", "behaviourPlot6ActivitiesSimplified4Leisure", 
#                                                         "behaviourPlot4Needs", "behaviourPlot4NeedsLeisureAndShopping", "behaviourPlot6ActivitiesWorkStudyHome",
#                                                         "behaviourPlot7SocialDistancing")
# plot_specifics_h[["5.2 DCSD-5-optimisation-lockdown"]] <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHomeDay", "behaviourPlot2InfectionsBelieveInfected",
#                                                             "behaviourPlot3Quarantiners", "behaviourPlot6ActivitiesSimplified4Leisure", "behaviourPlot6ActivitiesWorkStudyHome",
#                                                             "behaviourPlot7SocialDistancing")
# 
# plot_specifics_h[["0.1 Original ASSOCC"]]             <- c("behaviourPlot6Activities", "behaviourPlot6ActivitiesSimplified4RestAndWorkHomeDay", "behaviourPlot2InfectionsBelieveInfected",
#                                                            "behaviourPlot3Quarantiners", "behaviourPlot6ActivitiesSimplified4Leisure", "behaviourPlot6ActivitiesWorkStudyHome",
#                                                            "behaviourPlot7SocialDistancing")
# plot_specifics_h[["0.2 Original ASSOCC-lockdown"]]    <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHomeDay", "behaviourPlot2InfectionsBelieveInfected",
#                                                            "behaviourPlot3Quarantiners", "behaviourPlot3QuarantinersAgeGroup", "behaviourPlot6ActivitiesSimplified4Leisure", "behaviourPlot6ActivitiesWorkStudyHome",
#                                                            "behaviourPlot7SocialDistancing")
#plot_specifics_h <- hash()
plot_specifics_h[["5.0 DCSD-5-optimisation-no-infections"]] <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome",
                                                                 "behaviourPlot6ActivitiesSimplified5", "behaviourPlot6ActivitiesSimplified4Leisure2Weeks")
plot_specifics_h[["5.1 DCSD-5-optimisation"]]      <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                        "behaviourPlot3Quarantiners", "behaviourPlot6ActivitiesSimplified4Leisure", 
                                                        "behaviourPlot4Needs", "behaviourPlot4NeedsLeisureAndShopping", "behaviourPlot6ActivitiesWorkStudyHome",
                                                        "behaviourPlot7SocialDistancing", "behaviourPlot6ActivitiesSimplified5")
plot_specifics_h[["5.2 DCSD-5-optimisation-lockdown"]] <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                            "behaviourPlot3Quarantiners", "behaviourPlot6ActivitiesSimplified4Leisure", "behaviourPlot6ActivitiesWorkStudyHome",
                                                            "behaviourPlot7SocialDistancing", "behaviourPlot6ActivitiesSimplified5",
                                                            "behaviourPlot4NeedsLeisureAndShopping", "behaviourPlot4NeedsLeisureAndShoppingIndividual")
plot_specifics_h[["0.0 Original ASSOCC-no-infections"]] <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome",
                                                             "behaviourPlot6ActivitiesSimplified5", "behaviourPlot6ActivitiesSimplified4Leisure2Weeks")
plot_specifics_h[["0.1 Original ASSOCC"]]             <- c("behaviourPlot6Activities", "behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                           "behaviourPlot3Quarantiners", "behaviourPlot6ActivitiesSimplified4Leisure", "behaviourPlot6ActivitiesWorkStudyHome",
                                                           "behaviourPlot7SocialDistancing", "behaviourPlot6ActivitiesSimplified5")
plot_specifics_h[["0.2 Original ASSOCC-lockdown"]]    <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot2InfectionsBelieveInfected",
                                                           "behaviourPlot3Quarantiners", "behaviourPlot3QuarantinersAgeGroup", "behaviourPlot6ActivitiesSimplified4Leisure", "behaviourPlot6ActivitiesWorkStudyHome",
                                                           "behaviourPlot7SocialDistancing", "behaviourPlot6ActivitiesSimplified5",
                                                           "behaviourPlot4NeedsLeisureAndShopping", "behaviourPlot4NeedsLeisureAndShoppingIndividual")

plot_specifics_h <- hash()
plot_specifics_h[["1.1 rigid-habits-no-infected"]] <- c("behaviourPlot8Criteria")
plot_specifics_h[["1.2 rigid-habits-infected"]]    <- c("behaviourPlot8Criteria")
plot_specifics_h[["1.3 DCSD-1"]]                   <- c("behaviourPlot8Criteria")
plot_specifics_h[["1.4 DCSD-1-leisure-habits"]]    <- c("behaviourPlot8Criteria")

plot_specifics_h[["2.1 DCSD-2"]]                   <- c("behaviourPlot8Criteria")
plot_specifics_h[["2.2 DCSD-2-obligation-constraint"]]  <- c("behaviourPlot8Criteria")

plot_specifics_h[["3.1 DCSD-3-rigid-norms"]]       <- c("behaviourPlot8Criteria")
plot_specifics_h[["3.2 DCSD-3-rigid-norms-lockdown"]] <- c("behaviourPlot8Criteria")
plot_specifics_h[["3.3 DCSD-3"]]                      <- c("behaviourPlot8Criteria")
plot_specifics_h[["3.4 DCSD-3-lockdown"]]             <- c("behaviourPlot8Criteria")

plot_specifics_h[["4.1 DCSD-4"]]                   <- c("behaviourPlot8Criteria")

plot_specifics_h[["5.0 DCSD-5-optimisation-no-infections"]] <- c("behaviourPlot8Criteria")
plot_specifics_h[["5.1 DCSD-5-optimisation"]]               <- c("behaviourPlot8Criteria")
plot_specifics_h[["5.2 DCSD-5-optimisation-lockdown"]]      <- c("behaviourPlot8Criteria", "behaviourPlot2InfectionsBelieveInfected")

plot_specifics_h[["0.0 Original ASSOCC-no-infections"]] <- c("behaviourPlot8Criteria")
plot_specifics_h[["0.1 Original ASSOCC"]]               <- c("behaviourPlot8Criteria")
plot_specifics_h[["0.2 Original ASSOCC-lockdown"]]      <- c("behaviourPlot8Criteria")

#plot_specifics_h[["5.0 DCSD-5-optimisation-no-infections"]] <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot6ActivitiesSimplified5")
#plot_specifics_h[["0.0 Original ASSOCC-no-infections"]] <- c("behaviourPlot6ActivitiesSimplified4RestAndWorkHome", "behaviourPlot6ActivitiesSimplified5")

if (TRUE == FALSE) { print("Test") }
# [1] 1.1 rigid-habits-no-infected     1.2 rigid-habits-infected        1.3 DCSD-1                       1.4 DCSD-1-leisure-habits        2.1 DCSD-2                      
# [6] 2.2 DCSD-2-obligation-constraint 3.1 DCSD-3-rigid-norms           3.2 DCSD-3-rigid-norms-lockdown  3.3 DCSD-3                       3.4 DCSD-3-lockdown             
#[11] 4.1 DCSD-4                       
# 5.0 DCSD-5-optimisation-no-infections        5.1 DCSD-5-optimisation          5.2 DCSD-5-optimisation-lockdown
# 0.0 Original ASSOCC-no-infections            0.1 Original ASSOCC              0.2 Original ASSOCC-lockdown  

# True: plot 1.1 rigid-habits-, 1.2 rigid-habits-infected
specific_experiment_presets <- c("1.3 DCSD-1") 
specific_experiment_presets <- NULL

# Plots general size
gl_pdf_width = 9
gl_pdf_height = 6

gl_plot_theme  <- theme_bw()
gl_plot_guides <- guides(colour = guide_legend(nrow=2, byrow=TRUE, override.aes = list(size=5, alpha=1)))

#-------------------------------
#---     Fixed variables     ---
#-------------------------------

filesPath <- "" 
filesNames   <- c(paste(directory_files, "csv", sep = "."))

#=============================================================
#==================== PREPARE DATAFRAME ======================
#=============================================================

df_initial = behaviourLoadDataframe(filesPath, filesNames)
df_renamed <- behaviourRenameDataframe(df_initial)
df_final <- behaviourAddNormalizedColumns(df_renamed)

behaviourPlot7InfectionsComparison()

# Filter on one random seed, since the plots are from a single run
df_final_filtered <- df_final[df_final$random_seed == random_seed, ]
# df_final_filtered <- df_final_filtered[df_final_filtered$ce_risk_avoidance_threshold_for_sd == 0.80, ]  # 0.75 0.76 0.77 0.78 0.79 0.80 0.85 0.90

# Print the data for the comparison section
behaviourPlot7MeanOfActivities()
behaviourPlot8CriteriaResultsInitialise()

#=============================================================
#============= PLOT FOR LOOP FOR EVERYTHING ==================
#=============================================================
if (plot_type == "all") { pdf(paste("plot_", directory_files, "_behaviour_all_plots.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) }


#-----------------    CREATE THE FOR LOOP    -----------------
experiment_preset = unique(df_final_filtered$ce_context_experiment_presets)[1]   # Check the depth values and make dependent on the dataframe's depth levels

if (!is.null(specific_experiment_presets))
{
  experiment_presets = specific_experiment_presets
} else {
  experiment_presets = sort(unique(df_final_filtered$ce_context_experiment_presets))
}

for (experiment_preset in experiment_presets)
{
  # Start the plot creating loop

  subset_df <- df_final_filtered[df_final_filtered$ce_context_experiment_presets == experiment_preset, ]
  
  depth_value = unique(subset_df$ce_context_depth)[1] # This is the same for the whole column since its data of ONE preset
  gl_limits_x_max <- subset_df$stop_before_tick[1]
  
  plot_base_name = paste("plot_", directory_files, "_behaviour_", experiment_preset, sep="")
  
  #=============================================================
  #=================== Plotting functions  =====================
  #=============================================================
  
  #-----------------    THE LINE PLOT: TIME    -----------------
  
  # https://stackoverflow.com/questions/33702924/r-call-a-function-from-function-name-that-is-stored-in-a-variable
  #
  # a<-function(){1+1}                                                                                                  
  # var<-"a"
  # > get(var)()
  # [1] 2
  # Plan for tomorrow
  # Make separate functions (that don't need parameters ;) ), then decide per preset which functions are usefull, just give a list for each preset
  # (perhaps make a dictionary), then after that call them with get() (if specific plotting is TRUE!)
  # get("behaviourPlot1DeliberationType")()
  
  if (plot_specifics_only) {
    
    if (!is.null(plot_specifics_h[[experiment_preset]])) {
      print(paste("== Plotting for ", experiment_preset, " ==", sep=""))
      for (plot_specific_f_name in plot_specifics_h[[experiment_preset]]) {
        get(plot_specific_f_name)(plot_specific_f_name)
      }
    }
  }
  else {
    print(paste("== Plotting for ", experiment_preset, " ==", sep=""))
    for (plot_f_name in plot_full_functions) {
      get(plot_f_name)(plot_f_name)
    }
  }
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

