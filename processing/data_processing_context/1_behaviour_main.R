#--- LIBRARIES ---
# Install the libraries
#install.packages()
#install.packages("zoo")

# Open the libraries
if (!exists("libraries_loaded") || getwd() == "C:/Users/maart/OneDrive/Documenten")
{
  library(tidyverse)
  library(ggplot2)
  library(sjmisc)
  library(readr)
  library(zoo)
  
  #first empty working memory 
  rm(list=ls())
  libraries_loaded = TRUE
} else {
  #first empty working memory 
  rm(list=ls()) 
  libraries_loaded = TRUE
}

if (length(dev.list()!=0)) {dev.off()} # Close all open pdf's

#-------------------------------
#---     INITIALIZATION      ---
#-------------------------------

#---   Settings   ---
directory_r <- "D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context"

# This R file is for the main behaviour: only no lockdown or yes lockdown allowed.
directory_files <- "2024_05_13_full_experiment"
#directory_files <- "2024_03_13_full_yes_lockdown"
directory_files <- "2024_05_15_small_experiment"
directory_files <- "2024_05_15_presentation_experiment_2"
directory_files <- "2024_06_20_activities_check"
directory_files <- "2024-06-21-activities-habitual"
#directory_files <- "2024-06-21-activities-normative"

# One of: "none", "one", "all"
plot_type <- "none" # Generate no pdf's, just generate it in the viewer
#plot_type <- "one" # One plot per pdf
plot_type <- "all" # All plots in one pdf
plot_only_specific <- TRUE # At the moment this one is not used
limit_plots <- TRUE # If this is true, then deliberation_type plots are not generated

only_specific_presets <- FALSE # True: plot 1.1 rigid-habits-, 1.2 rigid-habits-infected

dataFileNames <- c(paste(directory_files, "csv", sep = "."))

#--- WORKSPACE AND DIRECTORY ---
setwd(paste(directory_r, directory_files, sep="/"))
getwd()


### MANUAL INPUT: Optionally specify filepath (i.e. where the behaviorspace csv is situated) ###
#NOTE: if csv files are placed in the workdirec, then leave filesPath unchanged
filesPath <- "" 

#=================== MANUAL INPUT: specify filenames ====================
filesNames   <- dataFileNames

#=============================================================
#========================= LOAD DATA =========================
#=============================================================

p_files_path = filesPath
p_files_names = filesNames

#read in datafiles using filesNames and filesPath variables
for (i in 1:length(p_files_names)) {
  print(paste("read csv from:", p_files_path, p_files_names[i], sep=""))
  #bind data from dataframe into new dataframe
  if (exists('t_df') && is.data.frame(get('t_df'))) { # Create additional rows, skips first 6 lines
    temp_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",", head=TRUE, stringsAsFactors = TRUE)
    temp_df$X.run.number. <- temp_df$X.run.number + max_run_number
    t_df <- rbind(t_df, temp_df)
  } else { # Create the first row
    t_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",", head=TRUE, stringsAsFactors = TRUE)
  }
  max_run_number <- max(t_df$X.run.number.)
}
df_initial = t_df

#=============================================================
#====================== RENAME COLUMNS =======================
#=============================================================
#change variable names
for (i in 1:length(df_initial)){
  
  col_name = names(df_initial)[i];
  # BASIC ADJUSTMENTS
  new_name = str_remove_all(col_name, "X.");
  new_name = str_replace_all(new_name, "\\.\\.\\.\\.", "_")
  new_name = str_replace_all(new_name, "\\.\\.\\.", "_")
  new_name = str_replace_all(new_name, "\\.\\.", "_")
  new_name = str_replace_all(new_name, "\\.", "_")
  if (substr(new_name, nchar(new_name), nchar(new_name)) == "_" ) {
    new_name = substr(new_name, 1, nchar(new_name)-1);
  }
  # ADVANCED ADJUSTMENTS
  new_name = str_remove(new_name, "age_group_to_age_group_")
  colnames(df_initial)[i] = new_name;
  print(paste(i ,". ", col_name, " >>> ", new_name, sep=""));
}

df_renamed = df_initial
old_variable_names <- names(t_df)
#- Custom column names
# Rename colnames for population status
colnames(df_renamed)[match("step", colnames(df_renamed))] = "tick";
colnames(df_renamed)[match("count_people_with_infection_status_healthy", colnames(df_renamed))] = "uninfected";
colnames(df_renamed)[match("count_people_with_infection_status_immune", colnames(df_renamed))] = "immune";
colnames(df_renamed)[match("count_people_with_is_believing_to_be_immune", colnames(df_renamed))] = "believe_immune";
colnames(df_renamed)[match("count_people_with_infection_status_healthy_or_infection_status_immune", colnames(df_renamed))] = "healthy";

# Rename colnames for the needs
colnames(df_renamed)[match("mean_belonging_satisfaction_level_of_people", colnames(df_renamed))] = "belonging";
colnames(df_renamed)[match("mean_risk_avoidance_satisfaction_level_of_people", colnames(df_renamed))] = "risk_avoidance";
colnames(df_renamed)[match("mean_autonomy_satisfaction_level_of_people", colnames(df_renamed))] = "autonomy";
colnames(df_renamed)[match("mean_luxury_satisfaction_level_of_people_with_not_is_child", colnames(df_renamed))] = "luxury";
colnames(df_renamed)[match("mean_health_satisfaction_level_of_people", colnames(df_renamed))] = "health";
colnames(df_renamed)[match("mean_sleep_satisfaction_level_of_people", colnames(df_renamed))] = "sleep";
colnames(df_renamed)[match("mean_compliance_satisfaction_level_of_people", colnames(df_renamed))] = "compliance";
colnames(df_renamed)[match("mean_financial_stability_satisfaction_level_of_people_with_not_is_child", colnames(df_renamed))] = "financial_stability";
colnames(df_renamed)[match("mean_food_safety_satisfaction_level_of_people", colnames(df_renamed))] = "food_safety";
colnames(df_renamed)[match("mean_leisure_satisfaction_level_of_people", colnames(df_renamed))] = "leisure";
colnames(df_renamed)[match("mean_financial_survival_satisfaction_level_of_people_with_not_is_child", colnames(df_renamed))] = "financial_survival";
colnames(df_renamed)[match("mean_conformity_satisfaction_level_of_people", colnames(df_renamed))] = "conformity";

# Rename colnames for the activities
colnames(df_renamed)[match("count_people_with_current_motivation_rest", colnames(df_renamed))] = "rest_at_home";
colnames(df_renamed)[match("count_people_with_is_working_at_home", colnames(df_renamed))] = "work_at_home";
colnames(df_renamed)[match("count_people_with_is_working_at_work", colnames(df_renamed))] = "work_at_work";
colnames(df_renamed)[match("count_children_with_is_at_school", colnames(df_renamed))] = "study_at_school";
colnames(df_renamed)[match("count_students_with_is_at_university", colnames(df_renamed))] = "study_at_university";
colnames(df_renamed)[match("count_people_with_is_at_private_leisure_place", colnames(df_renamed))] = "at_private_leisure"; # is also leisure_at_private
colnames(df_renamed)[match("count_people_with_is_at_public_leisure_place", colnames(df_renamed))] = "at_public_leisure"; # is also leisure_at_public
colnames(df_renamed)[match("count_people_with_current_motivation_essential_shopping", colnames(df_renamed))] = "shop_groceries";
colnames(df_renamed)[match("count_people_with_current_motivation_shopping", colnames(df_renamed))] = "shop_luxury";
colnames(df_renamed)[match("count_people_with_current_motivation_treatment_motive", colnames(df_renamed))] = "at_treatment";

# Rename colnames for the location types
colnames(df_renamed)[match("count_people_at_essential_shops", colnames(df_renamed))] = "at_essential_shops";
colnames(df_renamed)[match("count_people_with_is_at_home", colnames(df_renamed))] = "at_homes";
colnames(df_renamed)[match("count_people_at_non_essential_shops", colnames(df_renamed))] = "at_non_essential_shops";
colnames(df_renamed)[match("count_people_with_is_at_school", colnames(df_renamed))] = "at_schools";
colnames(df_renamed)[match("count_people_with_is_at_university", colnames(df_renamed))] = "at_universities";
colnames(df_renamed)[match("count_people_with_is_at_work", colnames(df_renamed))] = "at_workplaces";

# Rename colnames for the deliberation types
colnames(df_renamed)[match("count_people_with_delib_count_minimal_context_1", colnames(df_renamed))] = "Minimal context";
colnames(df_renamed)[match("count_people_with_delib_count_determine_most_salient_need_1", colnames(df_renamed))] = "Most salient need";
colnames(df_renamed)[match("count_people_with_delib_count_compare_need_levels_1", colnames(df_renamed))] = "Compare need levels";
colnames(df_renamed)[match("count_people_with_delib_count_normative_consideration_1", colnames(df_renamed))] = "Normative deliberation";
colnames(df_renamed)[match("count_people_with_delib_count_conformity_network_action_1", colnames(df_renamed))] = "Conformity deliberation";
colnames(df_renamed)[match("count_people_with_delib_count_full_need_1", colnames(df_renamed))] = "Full need";

colnames(df_renamed)[match("count_people_with_epistemic_infection_status_infected", colnames(df_renamed))] = "believe_infected";

df_names_compare <- data.frame("new" = names(df_renamed), "old" = old_variable_names)
print("Renamed the dateframe, please check the df_names_compare dataframe for correct column translation")

df_final = df_renamed

#=============================================================
#================= SELECT SUBSET OF DATA =====================
#=============================================================

# Just select one random seed. Since we are interested in just the single run results.
random_seed = 0
df_final_filtered <- df_final[df_final$random_seed == random_seed, ]

# Add a column for the amount of people that are alive, todo: just take the column of people (alive)
df_final_filtered$people_alive <- (df_final_filtered$youngs_at_start + df_final_filtered$students_at_start +
                           df_final_filtered$workers_at_start + df_final_filtered$retireds_at_start) - df_final_filtered$dead_people

#=============================================================
#============= PLOT FOR LOOP FOR EVERYTHING ==================
#=============================================================

gl_pdf_width = 9
gl_pdf_height = 6

if (plot_type == "all") { pdf(paste("plot_", directory_files, "_behaviour_all_plots.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) }

# Now I want to plot the different types of deliberation
# I want to plot the following columns: Minimal context, Most salient need, Compare need levels, Normative deliberation, Conformity deliberation, Full need
# Potentially remove Compare need levels
# For deliberation types:
# Should the agent numbers be a percentage?? Well to be honest I think so, but I can do this in Netlogo! That's way easier!

#-----------------    CREATE THE FOR LOOP    -----------------
experiment_preset = unique(df_final_filtered$ce_context_experiment_presets)[1]   # Check the depth values and make dependent on the dataframe's depth levels

if (only_specific_presets)
{
  experiment_presets = c("1.1 rigid-habits-no-infected", "1.2 rigid-habits-infected")
} else {
  experiment_presets = unique(df_final_filtered$ce_context_experiment_presets)
}

for (experiment_preset in experiment_presets)
{
  # Remove the first line??
  
  print(paste("Plotting: Starting to print plots for ", experiment_preset))
  # This retrieve all the rows for a single run with the experimental presets
  subset_df <- df_final_filtered[df_final_filtered$ce_context_experiment_presets == experiment_preset, ]
  
  depth_value = unique(subset_df$ce_context_depth)[1] # This should be the same for all the rows because its the same settings
  
  gl_limits_x_max <- subset_df$stop_before_tick[1]
  
  plot_base_name = paste("plot_", directory_files, "_behaviour_", experiment_preset, sep="")
  
  #=============================================================
  #================= PLOT DELIBERATION TYPE  ===================
  #=============================================================
  
  #-----------------    THE LINE PLOT: TIME    -----------------
  
  if (!limit_plots)
  {
  # Gather (tidyr) and select (dyplr), maybe its not a good idea to mix these?
  df_deliberation_type <- select(subset_df, tick, ce_context_depth, people_alive, `Minimal context`, `Most salient need`, `Compare need levels`, `Normative deliberation`, `Conformity deliberation`, `Full need`)
  # Now I want to for each of the columns `Minimal context` until `Full need` divide it by the people_alive column
  df_deliberation_type <- df_deliberation_type %>% mutate(`Minimal context` = (`Minimal context` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Most salient need` = (`Most salient need` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Compare need levels` = (`Compare need levels` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Normative deliberation` = (`Normative deliberation` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Conformity deliberation` = (`Conformity deliberation` / people_alive) * 100)
  df_deliberation_type <- df_deliberation_type %>% mutate(`Full need` = (`Full need` / people_alive) * 100)
  
  df_deliberation_type <- gather(df_deliberation_type, `Deliberation Type`, measurement, `Minimal context`:`Full need`)
  
  # Can remove: p <- ggplot(seg_acc_deliberation_type, aes(tick, measurement)) + geom_boxplot(aes(fill=Status), alpha=0.5)
  
  # col  = for the outline
  # fill = for filling the line (which then makes the whole line black because if col is not specified the outline will be the thing seen)
  p <- ggplot(df_deliberation_type, aes(x = tick, y = measurement, col=`Deliberation Type`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('Minimal context'='Minimal context','Compare need levels'='Compare need levels',
             'Most salient need'='Most salient need','Normative deliberation'='Normative deliberation',
             'Conformity deliberation'='Conformity deliberation','Full need'='Full need'),
    values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'),
    breaks=c('Minimal context','Most salient need','Compare need levels','Normative deliberation','Conformity deliberation','Full need'))
  p <- p + xlab("Ticks") + ylab("% used by agents")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=2, byrow=TRUE))

  if (plot_type == "one") { pdf(paste(plot_base_name, "_deliberation_type_overall.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - Overall", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_deliberation_type_at_beginning.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(0, 53), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - At Beginning", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_deliberation_type_at_peak_infections.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(84, 138), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (", experiment_preset,") - At Peak Infections", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  
  #----------------  THE BAR PLOT: PROPORTIONS  ----------------
  
  # Now I want to plot the proportions of the different types of deliberation

  # I want to sum the columns `Minimal context` until `Full need` in the df_deliberation_type dataframe
  df_deliberation_type_sum <- df_deliberation_type %>% group_by(`Deliberation Type`) %>% summarise_all(sum)
  df_deliberation_type_sum$`Deliberation Type` <- factor(df_deliberation_type_sum$`Deliberation Type`, levels = c(df_deliberation_type_sum$`Deliberation Type`))
  #levels = c(3, 5, 6, 1, 2, 4))
  
  # Now i want an additional column for df_deliberation_type_sum that is the sum of the single measurement column
  df_deliberation_type_sum$measurement_sum <- rep(sum(df_deliberation_type_sum$measurement), nrow(df_deliberation_type_sum))
  df_deliberation_type_sum$deliberation_type_proportions <- df_deliberation_type_sum$measurement / df_deliberation_type_sum$measurement_sum * 100
  
  # Now i want to plot df_deliberation_type_sum in a bar plot
  p <- ggplot(df_deliberation_type_sum, aes(x = `Deliberation Type`, y = deliberation_type_proportions, fill = `Deliberation Type`)) +
       geom_bar(stat="identity") + theme_bw() + ylab("Overall % used by agents") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.5)) +
       scale_fill_manual(
        labels=c('Minimal context'='Minimal context','Compare need levels'='Compare need levels',
                 'Most salient need'='Most salient need','Normative deliberation'='Normative deliberation',
                 'Conformity deliberation'='Conformity deliberation','Full need'='Full need'),
        values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'),
        breaks=c('Minimal context','Most salient need','Compare need levels','Normative deliberation','Conformity deliberation','Full need'))
  if (plot_type == "one") { pdf(paste(plot_base_name, "_deliberation_type_bar_plot.pdf", sep=""), width=6, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  # It should still be properly ordered, but that can come later.
  }
  #=============================================================
  #==================== PLOT INFECTIONS  =======================
  #=============================================================
  
  gl_plot_theme  <- theme_bw()
  gl_plot_guides <- guides(colour = guide_legend(nrow=2, byrow=TRUE, override.aes = list(size=5, alpha=1)))

  df_population_status <- select(subset_df, tick, ce_context_depth, uninfected, infected, believe_infected, dead_people, healthy) #, immune, believe_immune, 
  df_population_status <- gather(df_population_status, `Population Status`, measurement, uninfected:healthy)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('uninfected'='Uninfected', 'infected'='Infected',
             'believe_infected' ='Believe Infected', 'dead_people' ='Dead People', 
             'healthy' ='Healthy'),
    values=c('#afd16f', '#b00300', '#ff7c73', '#000000', '#3c9e34'),
    breaks=c('uninfected', 'infected','believe_infected','dead_people','healthy'))
  p <- p + labs(x = "Ticks", y = "Status of n agents", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Population Status (", experiment_preset,") - Overall", sep=""))
  if (plot_type == "one") { pdf(paste(plot_base_name, "_population_status_overall.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  #p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=2, byrow=TRUE))
  
  #==================== Minimal infection curve ===================
  df_population_status <- select(subset_df, tick, ce_context_depth, infected, believe_infected, healthy) # uninfected, dead_people, immune, believe_immune, 
  df_population_status <- gather(df_population_status, `Population Status`, measurement, infected:healthy)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('infected'='Infected',
             'believe_infected' ='Believe Infected', 
             'healthy' ='Healthy'),
    values=c('#b00300', '#ff7c73', '#3c9e34'),
    breaks=c('infected','believe_infected','healthy'))
  p <- p + labs(x = "Ticks", y = "Status of n agents", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Population Status (", experiment_preset,")", sep=""))
  if (plot_type == "one") { pdf(paste(plot_base_name, "_population_status_simplified.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  #=============================================================
  #====================== QUARANTINERS  ========================
  #=============================================================
  
  df_population_status <- select(subset_df, tick, ce_context_depth, count_officially_quarantiners, count_people_with_is_officially_asked_to_quarantine_and_not_is_in_quarantine) # uninfected, dead_people, immune, believe_immune, 
  df_population_status <- gather(df_population_status, `Population Status`, measurement, count_officially_quarantiners, count_people_with_is_officially_asked_to_quarantine_and_not_is_in_quarantine)
  
  p <- ggplot(df_population_status, aes(x = tick, y = measurement, col=`Population Status`)) + geom_line()
  p <- p + scale_colour_manual(
    labels=c('count_officially_quarantiners'='Officially asked to quarantine',
             'count_people_with_is_officially_asked_to_quarantine_and_not_is_in_quarantine' ='Breaking quarantine'),
    values=c('#2269ee', '#b00300'),
    breaks=c('count_officially_quarantiners','count_people_with_is_officially_asked_to_quarantine_and_not_is_in_quarantine'))
  p <- p + labs(x = "Time (Ticks)", y = "Status of n agents", col = "Status")
  p <- p + theme_bw() + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Quarantining (", experiment_preset,")", sep=""))
  if (plot_type == "one") { pdf(paste(plot_base_name, "_quarantining.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  
  #=============================================================
  #======================= PLOT NEEDS  =========================
  #=============================================================
  
  df_needs <- select(subset_df, tick, ce_context_depth, autonomy, belonging, compliance, conformity, financial_stability,
                     financial_survival, food_safety, health, leisure, luxury, risk_avoidance, sleep)
  df_needs <- gather(df_needs, `Need Type`, measurement, autonomy:sleep)
  
  p <- ggplot(df_needs, aes(x = tick, y = measurement, col=`Need Type`))
  p <- p + scale_colour_manual(
    labels=c('autonomy'='AUT', 'belonging'='BEL', 'compliance'='COM', 'conformity'='CON',
             'financial_stability'='FST', 'financial_survival'='FSU', 'food_safety'='FOO',
             'health'='HEA', 'leisure'='LEI', 'luxury'='LUX', 'risk_avoidance'='RIS', 'sleep'='SLE'),
    values=c('#f16a15','#000000','#9d6e48','#43a0a0',
             '#E69F00','#881556','#1A4B09',
             '#d73229','#f2ccd5','#80e389','#345da9','#8d8d8d'),
    breaks=c('autonomy', 'belonging', 'compliance', 'conformity',
             'financial_stability', 'financial_survival', 'food_safety',
             'health', 'leisure', 'luxury', 'risk_avoidance', 'sleep'))
  p <- p + xlab("Ticks") + ylab("Need Level") + labs(col = "Need")
  p <- p + theme_bw() + theme(text = element_text(size=16))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1)) + labs(title=paste("Need Levels (", experiment_preset,") - Overall", sep=""))  
  p_smooth <- p + geom_smooth()
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_needs_overall.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_needs_overall_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  #=============================================================
  #==================== LOCATION TYPES  ========================
  #=============================================================
  
  df_location_types <- select(subset_df, tick, ce_context_depth, at_essential_shops, at_homes, at_non_essential_shops,
                              at_private_leisure, at_public_leisure, at_schools, at_universities, at_workplaces, at_treatment)
  df_location_types <- gather(df_location_types, `Location Type`, measurement, at_essential_shops:at_treatment)
  
  p <- ggplot(df_location_types, aes(x = tick, y = measurement, col=`Location Type`))
  p <- p + scale_colour_manual(
    labels=c('at_essential_shops'='Essential Shops', 'at_homes'='Homes', 'at_non_essential_shops'='Non-essential Shops',
             'at_private_leisure'='Private Leisure', 'at_public_leisure'='Public Leisure', 'at_schools'='Schools',
             'at_universities'='Universities', 'at_workplaces'='Workplaces', 'at_treatment'='Treatment'),
    values=c('#881556','#80e389','#f2ccd5',
             '#f16a15','#d73229','#9d6e48', 
             '#E69F00','#345da9','#8d8d8d'),
    breaks=c('at_essential_shops', 'at_homes', 'at_non_essential_shops',
             'at_private_leisure', 'at_public_leisure', 'at_schools',
             'at_universities', 'at_workplaces', 'at_treatment'))
  p <- p + xlab("Ticks") + ylab("Agents at Location Type") + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Location Types (", experiment_preset,") - Overall", sep=""))  
  p_smooth <- p + geom_smooth()
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_location_types.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_location_types_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  #------------------------------------------------------------------
  #==================== Agent Activities All =================
  
  # "rest_at_home", "work_at_home", "work_at_work", "study_at_school", "study_at_university", "at_private_leisure", 
  # "at_public_leisure", "shop_groceries", "shop_luxury", "at_treatment"
  
  # Roll mean/moving average
  df_activities <- select(subset_df, tick, ce_context_depth, people_alive, shop_groceries, rest_at_home, shop_luxury,
                          at_private_leisure, at_public_leisure, study_at_school,
                          study_at_university, work_at_work, work_at_home, at_treatment)
  
  # for each column shop_groceries:at_treatment I want to mutate each column and multiply by 100
  for (column_i in 4:13)
  { df_activities[, column_i] <- (df_activities[, column_i] / df_activities$people_alive) * 100 }
  
  k_rollmean = 27#27 # 19/21 is a fine number
  decrease = 0 #floor(k_rollmean/2)
  v_tick <- (1+decrease):(gl_limits_x_max-decrease)
  
  df_activities_mean <- data.frame(v_tick, rep(df_activities$ce_context_depth[1], times=length(v_tick)))
  for (column_i in 4:13)
  { df_activities_mean <- cbind(df_activities_mean, rollapplyr(df_activities[, column_i], k_rollmean, mean, partial = TRUE)) } #rollmean(df_activities[, column_i], k_rollmean)) }
  
  colnames(df_activities_mean) <- c("tick", "ce_context_depth", "shop_groceries", "rest_at_home", "shop_luxury",
                                   "at_private_leisure", "at_public_leisure", "study_at_school",
                                   "study_at_university", "work_at_work", "work_at_home", "at_treatment")
  
  df_activities_gathered <- gather(df_activities, `Location Type`, measurement, shop_groceries:at_treatment)
  df_activities_mean_gathered <- gather(df_activities_mean, `Location Type`, measurement, shop_groceries:at_treatment)
  
  p <- ggplot(df_activities_gathered, aes(x = tick, y = measurement, col=`Location Type`))
  p <- p + scale_colour_manual(
    labels=c('shop_groceries'='Shop groceries', 'rest_at_home'='Rest at home', 'shop_luxury'='Shop luxury',
             'at_private_leisure'='Leisure at Pr', 'at_public_leisure'='Leisure at Pu', 'study_at_school'='Study at school',
             'study_at_university'='Study at uni', 'work_at_work'='Work at work', 'work_at_home'='Work at home', 'at_treatment'='Treatment'),
    values=c('#881556','#80e389','#f2ccd5',
             '#f16a15','#d73229','#9d6e48', 
             '#E69F00','#345da9','#000000','#8d8d8d'),
    breaks=c('shop_groceries', 'rest_at_home', 'shop_luxury',
             'at_private_leisure', 'at_public_leisure', 'study_at_school',
             'study_at_university', 'work_at_work', 'work_at_home', 'at_treatment'))
  p <- p + xlab("Ticks") + ylab("% Activities Chosen") + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Location Types (", experiment_preset,") - Overall", sep=""))  
  p_smooth <- p + geom_smooth() # se = True (confidence interval), span = .2 span = 0.75 (default = 0.75), method = 'lm' (for a linear line)
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_activities_rollmean.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_activities_rollmean_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  
  
  #------------------------------------------------------------------
  #==================== Agent Activities Simplified =================
  
  #----- Prepare the data frame ------
  
  df_activities_mean <- df_activities %>% mutate(rest_at_home = rest_at_home + work_at_home)
  df_activities_mean <- df_activities_mean %>% mutate(obligation = study_at_school + study_at_university + work_at_work)
  df_activities_mean <- df_activities_mean %>% mutate(shop_grocery = shop_groceries)
  df_activities_mean <- df_activities_mean %>% mutate(shop_luxury = shop_luxury)
  df_activities_mean <- df_activities_mean %>% mutate(leisure = at_private_leisure + at_public_leisure)
  
  df_activities_mean <- select(df_activities_mean, tick, ce_context_depth, rest_at_home, obligation, shop_grocery, shop_luxury, leisure)
  
  #----- Gather data for the plot -----
  df_activities_mean_gathered <- gather(df_activities_mean, `Location Type`, measurement, rest_at_home:leisure)
  
  p <- ggplot(df_activities_mean_gathered, aes(x = tick, y = measurement, col=`Location Type`))
  p <- p + scale_colour_manual(
    labels=c('rest_at_home'='Rest at Home', 'obligation'='Work or Study', 'shop_grocery'='Shop grocery', 'shop_luxury'='Shop luxury', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#9d6e48','#f16a15'),
    breaks=c('rest_at_home', 'obligation', 'shop_grocery', 'shop_luxury', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Activities (", experiment_preset,") - Overall", sep=""))  
  p <- p + xlab("Time (Ticks)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .7)
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_activities.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_activities_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  #==================== Agent Activities Simplified - SMOOTH THE LINES - DAY =================

  df_activities_day <- df_activities_mean %>% mutate(day = (tick - (tick %% 4)) / 4)
  # mean for each day
  df_activities_day <- df_activities_day %>% group_by(day) %>% summarise_all(mean)
  # remove column tick
  df_activities_day <- select(df_activities_day, -tick)
  
  df_activities_day_gathered <- gather(df_activities_day, `Location Type`, measurement, rest_at_home:leisure)
  
  p <- ggplot(df_activities_day_gathered, aes(x = day, y = measurement, col=`Location Type`))
  p <- p + scale_colour_manual(
    labels=c('rest_at_home'='Rest at Home', 'obligation'='Work or Study', 'shop_grocery'='Shop grocery', 'shop_luxury'='Shop luxury', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#9d6e48','#f16a15'),
    breaks=c('rest_at_home', 'obligation', 'shop_grocery', 'shop_luxury', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max/4), ylim = c(0, 100)) + labs(title=paste("Activities (", experiment_preset,") - Overall", sep=""))  
  p <- p + xlab("Time (Days)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .75)
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_activities_day.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_activities_day_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  #==================== Agent Activities Simplified - SMOOTH THE LINES - WEEK - USELESS DEPRICATED =================
  if (TRUE==FALSE) {
  df_activities_week <- df_activities_mean %>% mutate(week = (tick - (tick %% 28)) / 28)
  # mean for each week
  df_activities_week <- df_activities_week %>% group_by(week) %>% summarise_all(mean)
  # remove column tick
  df_activities_week <- select(df_activities_week, -tick)
  
  df_activities_week_gathered <- gather(df_activities_week, `Location Type`, measurement, at_home:leisure)
  
  p <- ggplot(df_activities_week_gathered, aes(x = week, y = measurement, col=`Location Type`))
  p <- p + scale_colour_manual(
    labels=c('at_home'='Rest at Home', 'obligation'='Work or Study', 'shopping'='Shopping', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#f16a15'),
    breaks=c('at_home', 'obligation', 'shopping', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max/28), ylim = c(0, 1020)) + labs(title=paste("Activities (", experiment_preset,") - Overall", sep=""))  
  p <- p + xlab("Time (Weeks)") + ylab("Agents performing activity") + labs(col="")
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste(plot_base_name, "_activities_week.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  }
}

if (plot_type == "all") { dev.off() }

# I have the df_location_types dataframe
# df_location_type contains the columns tick, ce_context_depth, at_essential_shops, at_homes, at_non_essential_shops, at_private_leisure, at_public_leisure, at_schools, at_universities, at_workplaces, at_treatment
# For each row I want to sum the columns at_essential_shops until at_treatment

#for (i in 1:nrow(df_location_types)) {
  
  #row = df_location_types[i,]
  #sum_row = sum(row$at_essential_shops, row$at_homes, row$at_non_essential_shops, row$at_private_leisure, row$at_public_leisure, row$at_schools, row$at_universities, row$at_workplaces, row$at_treatment)
  #print(sum_row)
#}

