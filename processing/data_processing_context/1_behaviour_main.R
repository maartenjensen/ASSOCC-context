#--- LIBRARIES ---
# Install the libraries
#install.packages()

# Open the libraries
if (!exists("libraries_loaded") || getwd() == "C:/Users/maart/OneDrive/Documenten")
{
  library(tidyverse)
  library(ggplot2)
  library(sjmisc)
  library(readr)
  
  #first empty working memory 
  rm(list=ls())
  libraries_loaded = TRUE
} else {
  #first empty working memory 
  rm(list=ls()) 
  libraries_loaded = TRUE
}

#-------------------------------
#---     INITIALIZATION      ---
#-------------------------------

#---   Settings   ---
directory_r <- "D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context"

# This R file is for the main behaviour: only no lockdown or yes lockdown allowed.
directory_files <- "2024_03_13_full_no_lockdown"
#directory_files <- "2024_03_13_full_yes_lockdown"


dataFileNames <- c(paste(directory_files, "csv", sep = "."))

if (directory_files == "2024_03_13_full_yes_lockdown") {
  
  gl_limits_x_max <- 480
} else {
  gl_limits_x_max <- 240
}




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
# Custom column names
colnames(df_renamed)[match("step", colnames(df_renamed))] = "tick";
colnames(df_renamed)[match("count_people_with_infection_status_healthy", colnames(df_renamed))] = "uninfected";
colnames(df_renamed)[match("count_people_with_infection_status_immune", colnames(df_renamed))] = "immune";
colnames(df_renamed)[match("count_people_with_is_believing_to_be_immune", colnames(df_renamed))] = "believe_immune";
colnames(df_renamed)[match("count_people_with_infection_status_healthy_or_infection_status_immune", colnames(df_renamed))] = "healthy";

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

colnames(df_renamed)[match("count_people_at_essential_shops", colnames(df_renamed))] = "at_essential_shops";
colnames(df_renamed)[match("count_people_with_is_at_home", colnames(df_renamed))] = "at_homes";
colnames(df_renamed)[match("count_people_at_non_essential_shops", colnames(df_renamed))] = "at_non_essential_shops";
colnames(df_renamed)[match("count_people_with_is_at_private_leisure_place", colnames(df_renamed))] = "at_private_leisure";
colnames(df_renamed)[match("count_people_with_is_at_public_leisure_place", colnames(df_renamed))] = "at_public_leisure";
colnames(df_renamed)[match("count_people_with_is_at_school", colnames(df_renamed))] = "at_schools";
colnames(df_renamed)[match("count_people_with_is_at_university", colnames(df_renamed))] = "at_universities";
colnames(df_renamed)[match("count_people_with_is_at_work", colnames(df_renamed))] = "at_workplaces";
colnames(df_renamed)[match("count_people_with_current_motivation_treatment_motive", colnames(df_renamed))] = "at_treatment";

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

random_seed = 1
df_final_filtered <- df_final[df_final$random_seed == random_seed, ]

# Add a column for the amount of people that are alive, todo: just take the column of people (alive)
df_final_filtered$people_alive <- (df_final_filtered$youngs_at_start + df_final_filtered$students_at_start +
                           df_final_filtered$workers_at_start + df_final_filtered$retireds_at_start) - df_final_filtered$dead_people

#=============================================================
#============= PLOT FOR LOOP FOR EVERYTHING ==================
#=============================================================

# One of: "none", "one", "all"
plot_type <- "none"
plot_type <- "one" 
plot_type <- "all"

gl_pdf_width = 9
gl_pdf_height = 6

if (plot_type == "all") { pdf(paste("plot_", directory_files, "_behaviour_all_plots.pdf", sep=""), width=gl_pdf_width, height=gl_pdf_height) }

# Now I want to plot the different types of deliberation
# I want to plot the following columns: Minimal context, Most salient need, Compare need levels, Normative deliberation, Conformity deliberation, Full need
# Potentially remove Compare need levels
# For deliberation types:
# Should the agent numbers be a percentage?? Well to be honest I think so, but I can do this in Netlogo! That's way easier!

depth_value = unique(df_final$ce_context_depth)[1]   # Check the depth values and make dependent on the dataframe's depth levels
for (depth_value in unique(df_final$ce_context_depth))
{
  subset_df <- df_final_filtered[df_final_filtered$ce_context_depth == depth_value, ]
  
  #=============================================================
  #================= PLOT DELIBERATION TYPE  ===================
  #=============================================================
  
  #-----------------    THE LINE PLOT: TIME    -----------------
  
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

  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_deliberation_type_overall.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (CD:", depth_value,") - Overall", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_deliberation_type_at_beginning.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(0, 53), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (CD:", depth_value,") - At Beginning", sep=""))
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_deliberation_type_at_peak_infections.pdf", sep=""), width=9, height=5) }
  p <- p + coord_cartesian(xlim = c(84, 138), ylim = c(0, 100)) + labs(title=paste("Deliberation Type per Agent (CD:", depth_value,") - At Peak Infections", sep=""))
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
  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_deliberation_type_bar_plot.pdf", sep=""), width=6, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  # It should still be properly ordered, but that can come later.
  
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
    values=c('#afd16f', '#ff1100', '#ff7c73', '#000000', '#3c9e34'),
    breaks=c('uninfected', 'infected','believe_infected','dead_people','healthy'))
  p <- p + xlab("Ticks") + ylab("Status of n agents")
  p <- p + theme_bw()
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Population Status (CD:", depth_value,") - Overall", sep=""))
  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_population_status_overall.pdf", sep=""), width=9, height=5) }
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
  p <- p + xlab("Ticks") + ylab("Need Level")
  p <- p + theme_bw()
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1)) + labs(title=paste("Need Levels (CD:", depth_value,") - Overall", sep=""))  
  p_smooth <- p + geom_smooth()
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_needs_overall.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_needs_overall_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  #=============================================================
  #==================== LOCATION TYPES  ========================
  #=============================================================
  # Perhaps actions at is a better one, or complete action with location. E.g. Shop at ES, Shop at NES, Rest at home, etc....
  
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
  p <- p + xlab("Ticks") + ylab("Agents at Location Type")
  p <- p + theme_bw()
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 1020)) + labs(title=paste("Location Types (CD:", depth_value,") - Overall", sep=""))  
  p_smooth <- p + geom_smooth()
  p <- p + geom_line()
  
  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_location_types.pdf", sep=""), width=9, height=5) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { pdf(paste("plot_", directory_files, "_behaviour_cd_", depth_value, "_location_types_smooth.pdf", sep=""), width=9, height=5) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
}

if (plot_type == "all") { dev.off() }

