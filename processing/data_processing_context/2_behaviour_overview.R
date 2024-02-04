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

#--- WORKSPACE AND DIRECTORY ---
setwd("D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context")
getwd()


#Make sure the R script with functions is placed in the working directory!
#source("S6_1_dataframe_functions.r")

### MANUAL INPUT: Optionally specify filepath (i.e. where the behaviorspace csv is situated) ###
#NOTE: if csv files are placed in the workdirec, then leave filesPath unchanged
filesPath <- "" 

#=================== MANUAL INPUT: specify filenames ====================
dataFileNames <- c("covid-sim-behaviour-space-3-runs-full-context.csv")
filesNames   <- dataFileNames

one_plot <- TRUE

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

#=============================================================
#================ PLOT DELIBERATION TYPE =====================
#=============================================================

# Now I want to plot the different types of deliberation
# I want to plot the following columns: Minimal context, Most salient need, Compare need levels, Normative deliberation, Conformity deliberation, Full need

depth_value = 5
subset_df <- df_final_filtered[df_final_filtered$ce_context_depth == depth_value, ]


# Gather (tidyr) and select (dyplr), maybe its not a good idea to mix these?
subset_df <- select(subset_df, tick, ce_context_depth, `Minimal context`, `Most salient need`, `Compare need levels`, `Normative deliberation`, `Conformity deliberation`, `Full need`)
seg_acc_deliberation_type <- gather(subset_df, DelibType, measurement, `Minimal context`:`Full need`)

p <- ggplot(seg_acc_deliberation_type, aes(tick, measurement)) + geom_boxplot(aes(fill=Status), alpha=0.5)

# col  = for the outline
# fill = for filling the line (which then makes the whole line black because if col is not specified the outline will be the thing seen)
p <- ggplot(seg_acc_deliberation_type, aes(x = tick, y = measurement, col=DelibType)) + geom_line()
p <- p + scale_colour_manual(
  labels=c('Minimal context'='Minimal context','Compare need levels'='Compare need levels',
           'Most salient need'='Most salient need','Normative deliberation'='Normative deliberation',
           'Conformity deliberation'='Conformity deliberation','Full need'='Full need'),
  values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'),
  breaks=c('Minimal context','Most salient need','Compare need levels','Normative deliberation','Conformity deliberation','Full need'))
p <- p + theme_bw()
show(p)

p <- ggplot(seg_acc_deliberation_type, aes(x = tick, y = measurement,group = DelibType, fill = DelibType), fill=NA) + geom_line(aes(col=DelibType))

seg_acc_deliberation_type %>%
  ggplot(aes(x = tick, 
             y = measurement,
             group = DelibType,
             fill = DelibType), fill=NA) +
  geom_line(aes(col=DelibType)) +
  xlab("Ticks") +
  ylab("Used by n agents") +
  labs(title="Deliberation types") +
  #guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1))) +
  theme_bw() + scale_color_manual(values=c('#33ddff', '#48bf3f', '#8c8c8c', '#E69F00', '#9911ab', '#000000'))


+ scale_fill_manual(
  labels=c('Low'='Lowest','Med'='Medium','High'='Highest'),
  values=c('red','blue','green'),
  breaks=c('Low','Med','High'))


seg_acc_deliberation_type %>%
  ggplot(aes(x = tick, 
             y = value,
             group = need_type,
             fill = need_type), fill=NA) +
  geom_line(aes(col=need_type)) +
  xlab("Ticks") +
  ylab("Used by n agents") +
  labs(title="Deliberation types") +
  guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1))) +
  theme_bw() + scale_color_manual(values=c('#000000', '#E69F00', '#f16a15', '#8d8d8d', '#345da9'))



plot_data %>%
  ggplot(aes(x = tick, 
             y = value,
             group = DelibType,
             fill = DelibType), fill=NA) +
  geom_line(aes(col=DelibType)) +
  xlab("Ticks") +
  ylab("Used by n agents") +
  labs(title="Deliberation types") +
  guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1))) +
  gl_plot_theme + scale_color_manual(values=c('#000000', '#E69F00', '#f16a15', '#8d8d8d', '#345da9'))



gl_plot_theme <- theme_bw()
#gl_plot_theme  <-  theme_bw() + theme(legend.position="bottom",
#                                      axis.text = element_text(size = rel(1.3 * multiplier)),
#                                      axis.title = element_text(size = rel(1.3 * multiplier)),
#                                      legend.text = element_text(size = rel(1 * multiplier)),
#                                      legend.title = element_text(size = rel(1 * multiplier)),
#                                      title = element_text(size = rel(1.3 * multiplier)) )

# Filter the dataframe for ce_context_depth values from 0 to 5
for (depth in 0:5) {
  # Create a subset dataframe for the current ce_context_depth
  subset_df <- df_final[df_final$ce_context_depth == depth, ]
  
  # Gather the columns for plotting
  plot_data <- subset_df %>%
    select(ce_context_depth, `Minimal context`, `Most salient need`, `Compare need levels`, `Normative deliberation`, `Conformity deliberation`, `Full need`) %>%
    gather(key = "need_type", value = "value", -ce_context_depth)
  
  plot_data %>%
    ggplot(aes(x = tick, 
               y = value,
               group = DelibType,
               fill = DelibType), fill=NA) +
    geom_line(aes(col=DelibType)) +
    xlab("Ticks") +
    ylab("Used by n agents") +
    labs(title="Deliberation types") +
    guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1))) +
    gl_plot_theme + scale_color_manual(values=c('#000000', '#E69F00', '#f16a15', '#8d8d8d', '#345da9'))
}



# Create a subset dataframe for the current ce_context_depth
subset_df <- df_final[df_final$ce_context_depth == depth, ]

# Gather the columns for plotting
plot_data <- subset_df %>%
  select(ce_context_depth, `Minimal context`, `Most salient need`, `Compare need levels`, `Normative deliberation`, `Conformity deliberation`, `Full need`) %>%
  gather(key = "need_type", value = "value", -ce_context_depth)

plot_data %>%
  ggplot(aes(x = tick, 
             y = value,
             group = DelibType,
             fill = DelibType), fill=NA) +
  geom_line(aes(col=DelibType)) +
  xlab("Ticks") +
  ylab("Used by n agents") +
  labs(title="Deliberation types") +
  guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1))) +
  gl_plot_theme + scale_color_manual(values=c('#000000', '#E69F00', '#f16a15', '#8d8d8d', '#345da9'))




df_action_space = df_final[df_final$action_space==6, ]

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

df_deliberation_type <- df_action_space %>% 
  group_by(tick, context_sensitive_deliberation) %>% 
  summarise(Typical = mean(Typical, na.rm = TRUE),
            Most_salient = mean(One_need, na.rm = TRUE),
            MS_Conf = mean(ON_Conformity, na.rm = TRUE),
            MS_Multi = mean(ON_Multi_act, na.rm = TRUE),
            All_needs = mean(All_needs, na.rm = TRUE))

seg_acc_deliberation_type <- gather(df_deliberation_type, DelibType, measurement, Typical:All_needs)

limits = coord_cartesian(xlim = c(0, 479), ylim = c(0, 1000))

if (!one_plot) { pdf("plot_deliberation_type.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot_deliberation_type(filter(seg_acc_deliberation_type, context_sensitive_deliberation=="true"), "Deliberation Type for Context ASSOCC", limits)
if (!one_plot) { dev.off() }


gather


# Function to create line plots for each need type
create_line_plots <- function(data, depth) {
  plot_data <- data %>%
    filter(ce_context_depth == depth) %>%
    select(ce_context_depth, `Minimal context`, `Most salient need`, `Compare need levels`, Normative, Conformity, `Full need`) %>%
    gather(key = "need_type", value = "value", -ce_context_depth)
  
  ggplot(plot_data, aes(x = factor(ce_context_depth), y = value, color = need_type, group = need_type)) +
    geom_line() +
    labs(title = paste("Line Plot for ce_context_depth =", depth),
         x = "ce_context_depth",
         y = "Value",
         color = "Need Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create line plots for each ce_context_depth
for (depth in 0:5) {
  plot <- create_line_plots(df_final, depth)
  print(plot)
}



# Filter the dataframe for ce_context_depth values from 0 to 5
for (depth in 0:5) {
  # Create a subset dataframe for the current ce_context_depth
  subset_df <- df_final[df_final$ce_context_depth == depth, ]
  
  # Gather the columns for plotting
  plot_data <- subset_df %>%
    select(ce_context_depth, `Minimal context`, `Most salient need`, `Compare need levels`, Normative, Conformity, `Full need`) %>%
    gather(key = "need_type", value = "value", -ce_context_depth)
  
  # Plotting
  ggplot(plot_data, aes(x = factor(ce_context_depth), y = value, fill = need_type)) +
    geom_boxplot() +
    labs(title = paste("Plot for ce_context_depth =", depth),
         x = "ce_context_depth",
         y = "Value",
         fill = "Need Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


#======================================================================
#------------------------ END OF DATAFRAME PREPARATIONS ---------------
#------------------------ START OF PLOTTING  -------------



#======================================================================
#------------------------- PLOTTING FUNCTIONS -------------------------

multiplier = 1
gl_plot_theme  <-  theme_bw() + theme(legend.position="bottom",
                                      axis.text = element_text(size = rel(1.3 * multiplier)),
                                      axis.title = element_text(size = rel(1.3 * multiplier)),
                                      legend.text = element_text(size = rel(1 * multiplier)),
                                      legend.title = element_text(size = rel(1 * multiplier)),
                                      title = element_text(size = rel(1.3 * multiplier)) )

gl_plot_guides <- guides(colour = guide_legend(nrow=2, byrow=TRUE, override.aes = list(size=5, alpha=1)))

plot_ggplot <- function(data_to_plot, p_title, p_limits) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement,
               group = Location_type,
               fill = Location_type), fill=NA) +
    geom_line(aes(col=Location_type)) +
    guides(colour = guide_legend(override.aes = list(size=5, alpha=1))) +
    xlab("Ticks") +
    ylab("Agents per tick") +
    labs(title=p_title) +
    gl_plot_guides + gl_plot_theme + p_limits +
    scale_color_manual(values=c('#000000', '#E69F00', '#d73229', '#1A4B09', '#1133FF'))
}

# Printing as PDF's
gl_pdf_width = 7
gl_pdf_height = 5

#=============================================================
#========================= PLOT DATA =========================
#=============================================================

df_final_backup <- df_final

df_final <- df_final_backup
df_final <- df_final[df_final$enable_salient_food_luxury_forced_obligation=="false", ]


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
            workplaces = mean(count_people_with_is_at_work, na.rm = TRUE),
            treatment = mean(count_people_with_current_motivation_treatment_motive, na.rm = TRUE))
colnames(df_people_at_locations)

x_limits = c(0,479) #c(28,83)


#---------- A SPLIT -----------




#----------- All the locations -------------
limits = coord_cartesian(xlim = x_limits, ylim = c(0, 1000))
seg_acc_people_at_locations <- gather(df_people_at_locations, Location_type, measurement, essential_shops:workplaces)
#plot_ggplot(filter(seg_acc_people_at_locations, context_sensitive_deliberation=="false"), "Agents per location type - Original ASSOCC", limits)
#plot_ggplot(filter(seg_acc_people_at_locations, context_sensitive_deliberation=="true"), "Agents per location type - Context ASSOCC", limits)

#----------- Location type 1 -------------
limits_1 = coord_cartesian(xlim = x_limits, ylim = c(0, 1000))
seg_acc_people_at_locations_limited_1 <- gather(df_people_at_locations, Location_type, measurement, c(homes, schools, universities, workplaces))
plot_ggplot(filter(seg_acc_people_at_locations_limited_1, context_sensitive_deliberation=="false"), "Agents per location type 1 - Original ASSOCC", limits_1)
plot_ggplot(filter(seg_acc_people_at_locations_limited_1, context_sensitive_deliberation=="true"), "Agents per location type 1 - Context ASSOCC", limits_1)

#----------- Location type 2 -------------
limits_2 = coord_cartesian(xlim = x_limits, ylim = c(0, 500))
seg_acc_people_at_locations_limited_2 <- gather(df_people_at_locations, Location_type, measurement, c(essential_shops, non_essential_shops, private_leisure, public_leisure, treatment))
plot_ggplot(filter(seg_acc_people_at_locations_limited_2, context_sensitive_deliberation=="false"), "Agents per location type 2 - Original ASSOCC", limits_2)
plot_ggplot(filter(seg_acc_people_at_locations_limited_2, context_sensitive_deliberation=="true"), "Agents per location type 2 - Context ASSOCC", limits_2)

if (one_plot) { pdf("plot_context_assocc_infections.pdf", width=gl_pdf_width, height=gl_pdf_height) }

if (!one_plot) { pdf("plot_agents_per_location_original_1.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot(filter(seg_acc_people_at_locations_limited_1, context_sensitive_deliberation=="false"), "Agents per location type 1 - Original ASSOCC", limits_1)
if (!one_plot) { dev.off() }
if (!one_plot) { pdf("plot_agents_per_location_context_1.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot(filter(seg_acc_people_at_locations_limited_1, context_sensitive_deliberation=="true"), "Agents per location type 1 - Context ASSOCC", limits_1)
if (!one_plot) { dev.off() }
if (!one_plot) { pdf("plot_agents_per_location_original_2.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot(filter(seg_acc_people_at_locations_limited_2, context_sensitive_deliberation=="false"), "Agents per location type 2 - Original ASSOCC", limits_2)
if (!one_plot) { dev.off() }
if (!one_plot) { pdf("plot_agents_per_location_context_2.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot(filter(seg_acc_people_at_locations_limited_2, context_sensitive_deliberation=="true"), "Agents per location type 2 - Context ASSOCC", limits_2)
if (!one_plot) { dev.off() }

agent_n = df_final$youngs_at_start[1] + df_final$students_at_start[1] + df_final$workers_at_start[1] + df_final$retireds_at_start[1]

df_people_at_locations_mean <- df_final %>% 
  group_by(context_sensitive_deliberation) %>% 
  summarise(agents = agent_n,
            essential_shops = mean(count_people_at_essential_shops, na.rm = TRUE),
            homes = mean(count_people_with_is_at_home, na.rm = TRUE),
            non_essential_shops = mean(count_people_at_non_essential_shops, na.rm = TRUE),
            private_leisure = mean(count_people_with_is_at_private_leisure_place, na.rm = TRUE),
            public_leisure = mean(count_people_with_is_at_public_leisure_place, na.rm = TRUE),
            schools = mean(count_people_with_is_at_school, na.rm = TRUE),
            universities = mean(count_people_with_is_at_university, na.rm = TRUE),
            workplaces = mean(count_people_with_is_at_work, na.rm = TRUE),
            treatment = mean(count_people_with_current_motivation_treatment_motive, na.rm = TRUE))

df_children_at_locations_mean <- df_final %>% 
  group_by(context_sensitive_deliberation) %>% 
  summarise(agents = mean(youngs_at_start, na.rm = TRUE),
            essential_shops = mean(count_children_with_is_non_essential_shop_of_current_activity, na.rm = TRUE),
            homes = mean(count_children_with_is_at_home, na.rm = TRUE),
            non_essential_shops = mean(count_children_with_is_essential_shop_of_current_activity, na.rm = TRUE),
            private_leisure = mean(count_children_with_is_at_private_leisure_place, na.rm = TRUE),
            public_leisure = mean(count_children_with_is_at_public_leisure_place, na.rm = TRUE),
            schools = mean(count_children_with_is_at_school, na.rm = TRUE),
            universities = mean(count_children_with_is_at_university, na.rm = TRUE),
            workplaces = mean(count_children_with_is_at_work, na.rm = TRUE),
            treatment = mean(count_children_with_current_motivation_treatment_motive, na.rm = TRUE))

df_students_at_locations_mean <- df_final %>% 
  group_by(context_sensitive_deliberation) %>% 
  summarise(agents = mean(students_at_start, na.rm = TRUE),
            essential_shops = mean(count_students_with_is_non_essential_shop_of_current_activity, na.rm = TRUE),
            homes = mean(count_students_with_is_at_home, na.rm = TRUE),
            non_essential_shops = mean(count_students_with_is_essential_shop_of_current_activity, na.rm = TRUE),
            private_leisure = mean(count_students_with_is_at_private_leisure_place, na.rm = TRUE),
            public_leisure = mean(count_students_with_is_at_public_leisure_place, na.rm = TRUE),
            schools = mean(count_students_with_is_at_school, na.rm = TRUE),
            universities = mean(count_students_with_is_at_university, na.rm = TRUE),
            workplaces = mean(count_students_with_is_at_work, na.rm = TRUE),
            treatment = mean(count_students_with_current_motivation_treatment_motive, na.rm = TRUE))

df_workers_at_locations_mean <- df_final %>% 
  group_by(context_sensitive_deliberation) %>% 
  summarise(agents = mean(workers_at_start, na.rm = TRUE),
            essential_shops = mean(count_workers_with_is_non_essential_shop_of_current_activity, na.rm = TRUE),
            homes = mean(count_workers_with_is_at_home, na.rm = TRUE),
            non_essential_shops = mean(count_workers_with_is_essential_shop_of_current_activity, na.rm = TRUE),
            private_leisure = mean(count_workers_with_is_at_private_leisure_place, na.rm = TRUE),
            public_leisure = mean(count_workers_with_is_at_public_leisure_place, na.rm = TRUE),
            schools = mean(count_workers_with_is_at_school, na.rm = TRUE),
            universities = mean(count_workers_with_is_at_university, na.rm = TRUE),
            workplaces = mean(count_workers_with_is_at_work, na.rm = TRUE),
            treatment = mean(count_workers_with_current_motivation_treatment_motive, na.rm = TRUE))

df_retireds_at_locations_mean <- df_final %>% 
  group_by(context_sensitive_deliberation) %>% 
  summarise(agents = mean(retireds_at_start, na.rm = TRUE),
            essential_shops = mean(count_retireds_with_is_non_essential_shop_of_current_activity, na.rm = TRUE),
            homes = mean(count_retireds_with_is_at_home, na.rm = TRUE),
            non_essential_shops = mean(count_retireds_with_is_essential_shop_of_current_activity, na.rm = TRUE),
            private_leisure = mean(count_retireds_with_is_at_private_leisure_place, na.rm = TRUE),
            public_leisure = mean(count_retireds_with_is_at_public_leisure_place, na.rm = TRUE),
            schools = mean(count_retireds_with_is_at_school, na.rm = TRUE),
            universities = mean(count_retireds_with_is_at_university, na.rm = TRUE),
            workplaces = mean(count_retireds_with_is_at_work, na.rm = TRUE),
            treatment = mean(count_retireds_with_current_motivation_treatment_motive, na.rm = TRUE))


print(df_people_at_locations_mean)
print(df_children_at_locations_mean)
print(df_students_at_locations_mean)
print(df_workers_at_locations_mean)
print(df_retireds_at_locations_mean)

df_total = df_people_at_locations_mean
df_total = rbind(df_total, df_children_at_locations_mean)
df_total = rbind(df_total, df_students_at_locations_mean)
df_total = rbind(df_total, df_workers_at_locations_mean)
df_total = rbind(df_total, df_retireds_at_locations_mean)

print(df_total)

#=============================================================
#======================== PLOT NEEDS =========================
#=============================================================

plot_ggplot_needs <- function(data_to_plot, p_title, p_limits) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement,
               group = Level,
               fill = Level), fill=NA) +
    geom_line(aes(col=Level)) +
    xlab("Ticks") +
    ylab("Need level") +
    labs(title=p_title) +
    guides(colour = guide_legend(nrow=2, byrow=TRUE, override.aes = list(size=8, alpha=1))) +
    gl_plot_theme + p_limits +
    scale_linetype_manual(values=c("solid", "dashed", "twodash", "dotted", "twodash", "dotted", "twodash", "dotted", "twodash", "dotted", "twodash", "dotted")) +
    scale_color_manual(values=c('#f16a15','#000000','#9d6e48','#43a0a0','#E69F00','#881556','#1A4B09','#d73229','#f2ccd5','#80e389','#345da9','#8d8d8d'))
}

#scale_linetype_manual(values=c("twodash", "dotted"))+
#scale_color_manual(values=c('#999999','#E69F00'))+

df_needs <- df_final %>% 
  group_by(tick, context_sensitive_deliberation) %>% 
  summarise(AUT = mean(autonomy, na.rm = TRUE),
            BEL = mean(belonging, na.rm = TRUE),
            COM = mean(compliance, na.rm = TRUE),
            CON = mean(conformity, na.rm = TRUE),
            FST = mean(financial_stability, na.rm = TRUE),
            FSU = mean(financial_survival, na.rm = TRUE),
            FOO = mean(food_safety, na.rm = TRUE),
            HEA = mean(health, na.rm = TRUE),
            LEI = mean(leisure, na.rm = TRUE),
            LUX = mean(luxury, na.rm = TRUE),
            RIS = mean(risk_avoidance, na.rm = TRUE),
            SLE = mean(sleep, na.rm = TRUE))
            
colnames(df_needs)


seg_acc_need_level <- gather(df_needs, Level, measurement, AUT:SLE)

x_limits = c(0,479) #c(28,83)

limits = coord_cartesian(xlim = x_limits, ylim = c(0.2, 1))

#plot_ggplot_needs(filter(seg_acc_need_level, context_sensitive_deliberation=="false"), "Need level - Original ASSOCC", limits)
#plot_ggplot_needs(filter(seg_acc_need_level, context_sensitive_deliberation=="true"), "Need level - Context ASSOCC", limits)

if (!one_plot) {pdf("plot_needs_original.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot_needs(filter(seg_acc_need_level, context_sensitive_deliberation=="false"), "Need level - Original ASSOCC", limits)
if (!one_plot) { dev.off() }

if (!one_plot) {pdf("plot_needs_assocc.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot_needs(filter(seg_acc_need_level, context_sensitive_deliberation=="true"), "Need level - Context ASSOCC", limits)
if (!one_plot) { dev.off() }


#=============================================================
#==================== PLOT INFECTIONS  =======================
#=============================================================

plot_ggplot_tick <- function(data_to_plot, p_title = "None", p_y_lab = "None",
                             p_mean_start_quaran_tick = 0, p_mean_end_quaran_tick = 0) {
  
  data_to_plot %>%
    ggplot(aes(x = tick, 
               y = measurement)) +
    geom_line(aes(col=as.factor(context_sensitive_deliberation))) +
    #scale_colour_brewer(palette = "viridis", name=gl_plot_variable_name) +
    scale_colour_viridis_d(name="Context") +
    labs(title=p_title,
         caption="Agent-based Social Simulation of Corona Crisis (ASSOCC)",
         x="Days", y=p_y_lab) +
    gl_plot_guides + gl_plot_theme + coord_cartesian(xlim = c(0, 479)) 
}

df_data <- df_final %>% 
  group_by(tick, context_sensitive_deliberation) %>% 
  summarise(infected = mean(infected, na.rm = TRUE),
            believe_infected = mean(believe_infected, na.rm = TRUE),
            tests_performed = mean(tests_performed, na.rm = TRUE),
            ratio_quarantiners_complying = mean(ratio_quarantiners_currently_complying_to_quarantine, na.rm = TRUE),
            dead_people = mean(dead_people)) #infected_this_tick = mean(infected_this_tick),

plots_data_infected <- gather(df_data, variable, measurement, infected)

if (!one_plot) { pdf("plot_agents_infected.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot_tick(plots_data_infected, "Agents infected", "Number of infected")
if (!one_plot) { dev.off() }

# Epistemic infected
plots_data_epistemic_infected <- gather(df_data, variable, measurement, believe_infected)

if (!one_plot) { pdf("plot_agents_infected_believe.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot_tick(plots_data_epistemic_infected, "Agents believing to be infected",
                       "Number of agents believing they are infected")
if (!one_plot) { dev.off() }

# Total deaths
plots_data_deaths <- gather(df_data, variable, measurement, dead_people)

if (!one_plot) { pdf("plot_agents_died.pdf", width=gl_pdf_width, height=gl_pdf_height) }
plot_ggplot_tick(plots_data_deaths, "Agents that died", "Cummulative number of deaths")
if (!one_plot) { dev.off() }

if (one_plot) { dev.off() }