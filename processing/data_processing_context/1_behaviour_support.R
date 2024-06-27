#=====
behaviourLoadLibraries <- function(p_libraries_need_to_be_loaded) {
  
  print("Initialise program")
  
  #install.packages("envnames","hash")
  
  # Load libraries
  if (p_libraries_need_to_be_loaded)
  {
    print("Loading libraries ...")
    library(tidyverse)
    library(ggplot2)
    library(sjmisc)
    library(readr)
    library(zoo)
    library(envnames)
    library(hash)
  }
  
  rm(list=ls())
  
  if (length(dev.list()!=0)) {dev.off()} # Close all open pdf's
  
  print("Initialised program")
  return(TRUE)
}

behaviourLoadDataframe <- function(p_files_path, p_files_names) {
  
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
  
  return(t_df)
}

behaviourRenameDataframe <- function(df_to_rename) {

  #change variable names
  for (i in 1:length(df_to_rename)){
    
    col_name = names(df_to_rename)[i];
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
    colnames(df_to_rename)[i] = new_name;
    print(paste(i ,". ", col_name, " >>> ", new_name, sep=""));
  }
  
  df_renamed = df_to_rename
  old_variable_names <- names(df_to_rename)
  #- Custom column names
  # Rename colnames for population status
  colnames(df_renamed)[match("step", colnames(df_renamed))] = "tick";
  colnames(df_renamed)[match("count_people", colnames(df_renamed))] = "people_alive";
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
  
  return(df_final)
}

behaviourAddNormalizedColumns <- function(df_to_normalize) {
  
  df_to_normalize <- df_to_normalize %>% mutate(`Minimal context perc` = (`Minimal context` / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(`Most salient need perc` = (`Most salient need` / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(`Compare need levels perc` = (`Compare need levels` / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(`Normative deliberation perc` = (`Normative deliberation` / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(`Conformity deliberation perc` = (`Conformity deliberation` / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(`Full need perc` = (`Full need` / people_alive) * 100)
  
  df_to_normalize <- df_to_normalize %>% mutate(shop_groceries_perc = (shop_groceries / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(rest_at_home_perc = (rest_at_home / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(shop_luxury_perc = (shop_luxury / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(at_private_leisure_perc = (at_private_leisure / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(at_public_leisure_perc = (at_public_leisure / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(study_at_school_perc = (study_at_school / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(study_at_university_perc = (study_at_university / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(work_at_work_perc = (work_at_work / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(work_at_home_perc = (work_at_home / people_alive) * 100)
  df_to_normalize <- df_to_normalize %>% mutate(at_treatment_perc = (at_treatment / people_alive) * 100)
  
  return(df_to_normalize)
}