library(tidyverse)
library(ggplot2)

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
  if (exists('t_df') && is.data.frame(get('t_df'))) { # Create additional rows, skips first 6 lines
    temp_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",",head=TRUE, stringsAsFactors = TRUE)
    temp_df$X.run.number. <- temp_df$X.run.number + max_run_number
    t_df <- rbind(t_df, temp_df)
  } else { # Create the first row
    t_df <- read.csv(paste(p_files_path, p_files_names[i], sep=""), skip = 6, sep = ",",head=TRUE, stringsAsFactors = TRUE)
  }
  max_run_number <- max(t_df$X.run.number.)
}
df_initial = t_df

#=============================================================
#======================= PREPARE DATA ========================
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

df_names_compare <- data.frame("new" = names(df_renamed), "old" = old_variable_names)
print("Renamed the dateframe, please check the df_names_compare dataframe for correct column translation")

df_final = df_renamed

#=============================================================
#========================= PLOT DATA =========================
#=============================================================


#------------------------- PLOT DATA -------------------------

df_people_at_locations <- df_final %>% 
  group_by(tick, context_sensitive_deliberation) %>% 
  summarise(essential_shops = mean(count_people_at_essential_shops, na.rm = TRUE),
            homes = mean(count_people_with_is_at_home, na.rm = TRUE),
            non_essential_shops = mean(count_people_at_non_essential_shops, na.rm = TRUE),
            private_leisure = mean(count_people_with_is_at_private_leisure_place, na.rm = TRUE),
            public_leisure = mean(count_people_with_is_at_public_leisure_place, na.rm = TRUE),
            schools = mean(count_people_with_is_at_school, na.rm = TRUE),
            universities = mean(count_people_with_is_at_university, na.rm = TRUE),
            workplaces = mean(count_people_with_is_at_work, na.rm = TRUE))
colnames(df_people_at_locations)

seg_acc_people_at_locations <- gather(df_people_at_locations, Location_type, measurement, essential_shops:workplaces)

plot_ggplot(filter(seg_acc_people_at_locations, context_sensitive_deliberation=="false"), "false")
plot_ggplot(filter(seg_acc_people_at_locations, context_sensitive_deliberation=="true"), "true")

seg_acc_people_at_locations_limited <- gather(df_people_at_locations, Location_type, measurement, c(essential_shops, non_essential_shops, private_leisure, public_leisure))

plot_ggplot(filter(seg_acc_people_at_locations_limited, context_sensitive_deliberation=="false"), "false")
plot_ggplot(filter(seg_acc_people_at_locations_limited, context_sensitive_deliberation=="true"), "true")



df_people_at_locations_mean <- df_final %>% 
  group_by(context_sensitive_deliberation) %>% 
  summarise(essential_shops = mean(count_people_at_essential_shops, na.rm = TRUE),
            homes = mean(count_people_with_is_at_home, na.rm = TRUE),
            non_essential_shops = mean(count_people_at_non_essential_shops, na.rm = TRUE),
            private_leisure = mean(count_people_with_is_at_private_leisure_place, na.rm = TRUE),
            public_leisure = mean(count_people_with_is_at_public_leisure_place, na.rm = TRUE),
            schools = mean(count_people_with_is_at_school, na.rm = TRUE),
            universities = mean(count_people_with_is_at_university, na.rm = TRUE),
            workplaces = mean(count_people_with_is_at_work, na.rm = TRUE))
colnames(df_people_at_locations_mean)


df_workers_at_locations_mean <- df_final %>% 
  group_by(context_sensitive_deliberation) %>% 
  summarise(essential_shops = mean(count_workers_with_is_non_essential_shop_of_current_activity, na.rm = TRUE),
            homes = mean(count_workers_with_is_at_home, na.rm = TRUE),
            non_essential_shops = mean(count_workers_with_is_essential_shop_of_current_activity, na.rm = TRUE),
            private_leisure = mean(count_workers_with_is_at_private_leisure_place, na.rm = TRUE),
            public_leisure = mean(count_workers_with_is_at_public_leisure_place, na.rm = TRUE),
            schools = mean(count_workers_with_is_at_school, na.rm = TRUE),
            universities = mean(count_workers_with_is_at_university, na.rm = TRUE),
            workplaces = mean(count_workers_with_is_at_work, na.rm = TRUE))
colnames(df_workers_at_locations_mean)



#------------------------- PLOTTING FUNCTIONS -------------------------

multiplier = 1
gl_plot_theme  <-  theme_bw() + theme(legend.position="bottom",
                                      axis.text = element_text(size = rel(1.3 * multiplier)),
                                      axis.title = element_text(size = rel(1.3 * multiplier)),
                                      legend.text = element_text(size = rel(1 * multiplier)),
                                      legend.title = element_text(size = rel(1 * multiplier)),
                                      title = element_text(size = rel(1.3 * multiplier)) )

gl_plot_guides <- guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1)))

plot_ggplot <- function(data_to_plot, uses_context_reasoning) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement,
               group = Location_type,
               fill = Location_type), fill=NA) +
    geom_line(aes(col=Location_type)) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) +
    xlab("Ticks") +
    ylab("Agents per tick") +
    labs(title=paste("Agents at location type - Context ASSOCC:", uses_context_reasoning),
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)") +
    gl_plot_guides + gl_plot_theme 
}
