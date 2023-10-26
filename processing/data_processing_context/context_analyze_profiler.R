#install.packages("sjmisc")

library(tidyverse)
library(ggplot2)
library(sjmisc)
library(readr)

#first empty working memory 
rm(list=ls()) 

setwd("D:/SimulationToolkits/ASSOCC-context/processing/data_processing_context")
getwd()

### MANUAL INPUT: Optionally specify filepath (i.e. where the behaviorspace csv is situated) ###
#NOTE: if csv files are placed in the workdirec, then leave filesPath unchanged
filesPath <- "" 

#=================== MANUAL INPUT: specify filenames ====================
dataFileName <- c("report-[C= false -H= 350 -R= 1 -A= 6].csv",
                  "report-[C= false -H= 350 -R= 2 -A= 6].csv",
                  "report-[C= false -H= 350 -R= 3 -A= 6].csv",
                  "report-[C= false -H= 350 -R= 4 -A= 6].csv",
                  "report-[C= false -H= 350 -R= 5 -A= 6].csv",
                  "report-[C= false -H= 700 -R= 1 -A= 6].csv",
                  "report-[C= false -H= 700 -R= 2 -A= 6].csv",
                  "report-[C= false -H= 700 -R= 3 -A= 6].csv",
                  "report-[C= false -H= 700 -R= 4 -A= 6].csv",
                  "report-[C= false -H= 700 -R= 5 -A= 6].csv",
                  "report-[C= true -H= 350 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 350 -R= 2 -A= 6].csv",
                  "report-[C= true -H= 350 -R= 3 -A= 6].csv",
                  "report-[C= true -H= 350 -R= 4 -A= 6].csv",
                  "report-[C= true -H= 350 -R= 5 -A= 6].csv",
                  "report-[C= true -H= 700 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 700 -R= 2 -A= 6].csv",
                  "report-[C= true -H= 700 -R= 3 -A= 6].csv",
                  "report-[C= true -H= 700 -R= 4 -A= 6].csv",
                  "report-[C= true -H= 700 -R= 5 -A= 6].csv")

dataFileName <- c("report-[C= false -H= 350 -R= 1 -A= 6].csv",
                  "report-[C= true -H= 350 -R= 1 -A= 6].csv")

dataFileName <- c("report-[C= false -H= 350 -R= 1 -A= 6 -F= false].csv",
                  "report-[C= false -H= 350 -R= 1 -A= 6 -F= true].csv",
                  "report-[C= true -H= 350 -R= 1 -A= 6 -F= false].csv",
                  "report-[C= true -H= 350 -R= 1 -A= 6 -F= true].csv")

filesNames   <- dataFileName

str_to_v_without_white_spaces <- function(p_str) {
  t_vector <- c()
  t_str = ""
  
  str_split <- strsplit(p_str, "")[[1]]
  for(char in str_split) {
    if (char == ' ') {
      if (t_str != "") {
        t_vector <- c(t_vector, t_str)
        t_str = ""
      }
    }
    else {
      t_str = paste(t_str, char, sep = "")
    }
  }
  t_vector <- c(t_vector, t_str)
  return(t_vector)
}

setting_str_to_v <- function(p_str) {
  t_settings_vector <- c()
  t_index = 1
  str_split <- strsplit(p_str, "")[[1]]
  # Check C= False or True
  while (length(t_settings_vector) == 0) {
    if (str_split[t_index] == "C") {
      t_index = t_index + 3
      if (str_split[t_index] == "f") { t_settings_vector <- c(t_settings_vector, FALSE); t_index = t_index + 1 }
      else if (str_split[t_index] == "t") { t_settings_vector <- c(t_settings_vector, TRUE) }
      else { stop(paste(p_str, " is not a valid string", sep = "")) }
    }
    t_index = t_index + 1
  }
  # Check H= 175, 350, 1000, etc.
  while (length(t_settings_vector) == 1) {
    if (str_split[t_index] == "H") {
      t_index = t_index + 3
      t_households = str_split[t_index]
      while (length(t_settings_vector) == 1) {
        t_index = t_index + 1 # before, since the first number is already added when t_households is initialized
        if (str_split[t_index] != " ") {
          t_households = paste(t_households, str_split[t_index], sep="")
        }
        else if (str_split[t_index] == " ") {
          t_settings_vector <- c(t_settings_vector, t_households)
        }
      }
    }
    t_index = t_index + 1
  }
  # Check R= 1 or 2 or something
  while (length(t_settings_vector) == 2) {
    if (str_split[t_index] == "R") {
      t_index = t_index + 3
      t_random_seed = str_split[t_index]
      while (length(t_settings_vector) == 2) {
        t_index = t_index + 1 # before, since the first number is already added when t_random_seed is initialized
        if (str_split[t_index] != " ") {
          t_random_seed = paste(t_random_seed, str_split[t_index], sep="")
        }
        else if (str_split[t_index] == " ") {
          t_settings_vector <- c(t_settings_vector, t_random_seed)
        }
      }
    }
    t_index = t_index + 1
  }
  # Check A= Action space
  while (length(t_settings_vector) == 3) {
    if (str_split[t_index] == "A") {
      t_index = t_index + 3
      t_settings_vector <- c(t_settings_vector, str_split[t_index])
    }
    t_index = t_index + 1
  }

  return(t_settings_vector)
}
# Solution for tomorrow, a for loop through the characters and find C, then 3 steps further, then find H, then further until space, do the same for R and A, etc.

#=============================================================
#========================= LOAD DATA =========================
#=============================================================

p_files_path = filesPath
p_files_names = filesNames

#---------- CREATE DATAFRAME ------------#
df_results = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]

print("- GO Function")
print("----------------------------------")
#read in datafiles using filesNames and filesPath variables
for (i in 1:length(p_files_names)) {
  file_name = p_files_names[i]
  file_path_name = paste(p_files_path, p_files_names[i], sep="")

  df_initial = read.csv(file_path_name, skip = 2)
  for (i in 1:nrow(df_initial)) {

    if (str_contains(df_initial[i,1], "GO ") || str_contains(df_initial[i,1], "SELECT-ACTIVITY ") || 
        str_contains(df_initial[i,1], "CONTEXT") || str_contains(df_initial[i,1], "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR") ||
        str_contains(df_initial[i,1], "CSN") || str_contains(df_initial[i,1], "CSSN") ||
        str_contains(df_initial[i,1], "CSO") || str_contains(df_initial[i,1], "CSSO") ||
        str_contains(df_initial[i,1], "CSFT") || str_contains(df_initial[i,1], "CSSFT")) {
      
      result_v <- str_to_v_without_white_spaces(df_initial[i,1])
      settings_v <- setting_str_to_v(file_name)
      df_results <- rbind(df_results, c(settings_v[1], settings_v[2], settings_v[3], settings_v[4],
                                        result_v[1], as.double(result_v[2]), result_v[3], result_v[4], result_v[5]))
    }
    if (str_contains(df_initial[i,1], "Sorted by Inclusive Time")) {
      break
    }
  }
}

#---------- MAKING THE DATAFRAME NICE ---------#
colnames(df_results) <- c("context", "households", "random_seed", "action_space", "function_name", "calls", "incl_t_ms", "excl_t_ms", "excl_calls")
df_results$households = as.integer(df_results$households)
df_results$random_seed = as.integer(df_results$random_seed)
df_results$action_space = as.integer(df_results$action_space)
df_results$calls = as.double(df_results$calls)
df_results$incl_t_ms = as.double(df_results$incl_t_ms)
df_results$excl_t_ms = as.double(df_results$excl_t_ms)
df_results$excl_calls = as.double(df_results$excl_calls)
print(df_results)

df_results <- df_results[order(df_results$context, df_results$function_name), ]

df_results_csn = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]
for (i in 1:nrow(df_results)) {
  
  if (str_contains(df_results$function_name[i], "CSN")) {
    df_results_csn <- rbind(df_results_csn, df_results[i, ])
  }
}

df_results_cssn = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]
for (i in 1:nrow(df_results)) {
  
  if (str_contains(df_results$function_name[i], "CSSN")) {
    df_results_cssn <- rbind(df_results_cssn, df_results[i, ])
  }
}

df_results_cso = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]
for (i in 1:nrow(df_results)) {
  
  if (str_contains(df_results$function_name[i], "CSO")) {
    df_results_cso <- rbind(df_results_cso, df_results[i, ])
  }
}

df_results_csso = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]
for (i in 1:nrow(df_results)) {
  
  if (str_contains(df_results$function_name[i], "CSSO")) {
    df_results_csso <- rbind(df_results_csso, df_results[i, ])
  }
}

df_results_csft = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]
for (i in 1:nrow(df_results)) {
  
  if (str_contains(df_results$function_name[i], "CSFT")) {
    df_results_csft <- rbind(df_results_csft, df_results[i, ])
  }
}

df_results_cssft = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]
for (i in 1:nrow(df_results)) {
  
  if (str_contains(df_results$function_name[i], "CSSFT")) {
    df_results_cssft <- rbind(df_results_cssft, df_results[i, ])
  }
}

df_results_overview = df_results[c(5,4,17,63), ]

#=============================================================
#================== PLOTTING FUNCTIONS  ======================
#=============================================================
# For this code see also the file: context_analyze_table_data.R
multiplier = 1
gl_plot_theme  <-  theme_bw() + theme(legend.position="bottom",
                                      axis.text = element_text(size = rel(1.3 * multiplier)),
                                      axis.title = element_text(size = rel(1.3 * multiplier)),
                                      legend.text = element_text(size = rel(1 * multiplier)),
                                      legend.title = element_text(size = rel(1 * multiplier)),
                                      title = element_text(size = rel(1.3 * multiplier)) )

#gl_plot_guides <- guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1)))

# Printing as PDF's
gl_pdf_width = 7
gl_pdf_height = 5

plot_ggplot_execution_time <- function(data_to_plot, p_title, p_limits) {
  
  data_to_plot %>%
    ggplot(aes(x = households, 
               y = incl_t_ms,
               group = context,
               fill = context), fill=NA) +
    geom_line(aes(col=context)) +
    xlab("Households") +
    ylab("Execution time ms") +
    labs(title=p_title) +
    guides(colour = guide_legend(nrow=1, byrow=TRUE, override.aes = list(size=5, alpha=1))) +
    gl_plot_theme + scale_color_manual(values=c('#000000', '#E69F00', '#f16a15', '#8d8d8d', '#345da9')) + p_limits
}

#=============================================================
#================= PREPARE DATA AND PLOT =====================
#=============================================================

#pdf("plot_context_assocc_profiler.pdf", width=gl_pdf_width, height=gl_pdf_height)

df_results_go <- df_results[df_results$function_name=="GO", ]
df_grouped_go <- df_results_go[ , c('context', 'households', 'random_seed', 'incl_t_ms')] %>% group_by(context, households)  %>% 
  summarise(incl_t_ms = mean(incl_t_ms, na.rm = TRUE))
plot_ggplot_execution_time(df_grouped_go, "Execution time ms - GO (n=5)", coord_cartesian(xlim = c(350, 700), ylim = c(0, 950000)))

df_results_select_activity <- df_results[(df_results$function_name=="CONTEXT-SELECT-ACTIVITY" | df_results$function_name=="SELECT-ACTIVITY"), ]
df_grouped_select_activity <-  df_results_select_activity[ , c('context', 'households', 'random_seed', 'incl_t_ms')] %>% group_by(context, households) %>% 
  summarise(incl_t_ms = mean(incl_t_ms, na.rm = TRUE))
plot_ggplot_execution_time(df_grouped_select_activity, "Execution time ms - Select Activity (n=5)", coord_cartesian(xlim = c(350, 700), ylim = c(0, 500000)))

#dev.off()

df_check <- df_results[df_results$random_seed==1 & df_results$households==350, ]

print(df_results_csn)
print(df_results_cssn)
print(df_results_cso)
print(df_results_csso)
print(df_results_csft)
print(df_results_cssft)

# Updates for later
#- The column should also include the forced obligation 
#- It should print the important results always at the end, i.e. GO, 'CONTEXT'-SELECT-ACTIVITY, MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR