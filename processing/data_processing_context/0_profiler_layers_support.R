profilerLoadData <- function(p_filepath_workspace, p_filenames_profiler) {
  
  # If there is a warning with 'names' attribute [10] must be the same length as the vector [9]
  # The bug is in profilerGetContextDepth(), where the preset should be added
  # p_filepath_workspace = paste(directory_r, directory_files, sep="/")
  # p_filenames_profiler = filenames_profiler
  
  df_results = data.frame(preset=NA, households=NA, action_space=NA, random_seed=NA, context = NA,  
                          function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]

  print("- GO Function")
  print("----------------------------------")
  
  # Read in datafiles using filesNames and filesPath variables
  for (i in 1:length(p_filenames_profiler)) {
    
    file_name = p_filenames_profiler[i]
    file_path_name = paste(p_filepath_workspace, p_filenames_profiler[i], sep="/")

    df_initial = read.csv(file_path_name, skip = 2)
    for (j in 1:nrow(df_initial)) {

      if (str_contains(df_initial[j,1], "GO ") || str_contains(df_initial[j,1], "SELECT-ACTIVITY ") ||
          str_contains(df_initial[j,1], "CONTEXT") || str_contains(df_initial[j,1], "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR") ||
          str_contains(df_initial[j,1], "CSN") || str_contains(df_initial[j,1], "CSSN") ||
          str_contains(df_initial[j,1], "CSO") || str_contains(df_initial[j,1], "CSSO") ||
          str_contains(df_initial[j,1], "CSFT") || str_contains(df_initial[j,1], "CSSFT")) {

        result_v = profilerStrToVWithoutWhiteSpaces(df_initial[j,1])
        settings_v = profilerSettingStrToV(file_name)
        df_new_line = c(settings_v[1], settings_v[2], settings_v[3], settings_v[4], profilerGetContextDepth(settings_v[1]),
                         result_v[1], as.double(result_v[2]), result_v[3], result_v[4], result_v[5])
        
        if (length(df_new_line) != 10) {
          print(df_new_line)
        }
        df_results = rbind(df_results, df_new_line)
      }
      if (str_contains(df_initial[j,1], "Sorted by Inclusive Time")) {
        break
      }
    }
  }
  
  # Making the dataframe nice
  colnames(df_results) <- c("preset", "households", "action_space", "random_seed", "context",
                            "function_name", "calls", "incl_t_ms", "excl_t_ms", "excl_calls")
  df_results$households = as.integer(df_results$households)
  df_results$action_space = as.integer(df_results$action_space)
  df_results$random_seed = as.integer(df_results$random_seed)
  df_results$context = as.integer(df_results$context)
  df_results$calls = as.double(df_results$calls)
  df_results$incl_t_ms = as.double(df_results$incl_t_ms)
  df_results$excl_t_ms = as.double(df_results$excl_t_ms)
  df_results$excl_calls = as.double(df_results$excl_calls)
  
  df_results <- df_results[order(df_results$preset, df_results$function_name), ]
  
  return(df_results)
}

profilerLoadSpecificData <- function(p_df_profiler, p_string) {
  
  df_profiler_specific = data.frame(preset=NA, households=NA, action_space=NA, random_seed=NA, context = NA,  
                                    function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]
  
  # Check if it is a single string
  if (profilerIsSingleString(p_string))
  {
    for (i in 1:nrow(p_df_profiler)) {
      
      if (str_contains(p_df_profiler$function_name[i], p_string)) {
        df_profiler_specific <- rbind(df_profiler_specific, p_df_profiler[i, ])
      }
    }
    return(df_profiler_specific)
  }
  else
  {
    for (i in 1:nrow(p_df_profiler)) {
      for (j in 1:length(p_string))
      {
        if (str_contains(p_df_profiler$function_name[i], p_string[j])) {
          df_profiler_specific <- rbind(df_profiler_specific, p_df_profiler[i, ])
        }
      }
    }
    return(df_profiler_specific)
  }
  return("It didn't work")
}

profilerSummarize <- function(df_profiler, df_profiler_overview) {
  
  print("----------------------------")
  print("Profiler summary")
  print("----------------------------")
  print(df_profiler_overview)
}

retrieve_filenames_profiler <- function(p_preset = c("0.1 Original ASSOCC"), p_households = c("350"), p_action_space = c("6"), p_random_seed = c("0")) {
  
  p_filenames_profiler <- c()
  for (preset in p_preset)
  {
    for (households in p_households)
    {
      for (action_space in p_action_space)
      {
        for (random_seed in p_random_seed)
        {
           p_filenames_profiler <- c(p_filenames_profiler, paste("report-[-P= ", preset, " -H= ", households, 
                                                                    " -A= ", action_space, " -R= ", random_seed, "].csv", sep=""))
        }
      }
    }
  }
  return(p_filenames_profiler)
}

#-----------------------
#--- EXTRA FUNCTIONS ---
#-----------------------
profilerIsSingleString <- function(input) {
  is.character(input) & length(input) == 1
}

profilerStrToVWithoutWhiteSpaces <- function(p_str) {
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

profilerSettingStrToV <- function(p_str) {

  t_settings_vector <- c()
  t_index = 1
  # Split the whole line into separate characters
  str_split <- strsplit(p_str, "")[[1]] 
  
  # Check P= Preset
  while (length(t_settings_vector) == 0) {
    if (str_split[t_index] == "P") {
      t_index = t_index + 3
      t_preset = str_split[t_index]
      while (length(t_settings_vector) == 0) {
        t_index = t_index + 1 # before, since the first character is already added when t_preset is initialized
        if (str_split[t_index+1] != "-" || str_split[t_index+2] != "H") {
          t_preset = paste(t_preset, str_split[t_index], sep="")
        }
        else if (str_split[t_index] == " " && str_split[t_index+1] == "-" && str_split[t_index+2] == "H") {
          t_settings_vector <- c(t_settings_vector, t_preset)
        }
      }
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
  
  # Check A= Action space
  while (length(t_settings_vector) == 2) {
    if (str_split[t_index] == "A") {
      t_index = t_index + 3
      t_settings_vector <- c(t_settings_vector, str_split[t_index])
    }
    t_index = t_index + 1
  }
  
  # Check R= 1 or 2 or something
  while (length(t_settings_vector) == 3) {
    if (str_split[t_index] == "R") {
      t_index = t_index + 3
      t_random_seed = str_split[t_index]
      while (length(t_settings_vector) == 3) {
        t_index = t_index + 1 # before, since the first number is already added when t_random_seed is initialized
        if (str_split[t_index] != "]") {
          t_random_seed = paste(t_random_seed, str_split[t_index], sep="")
        }
        else if (str_split[t_index] == "]") {
          t_settings_vector <- c(t_settings_vector, t_random_seed)
        }
      }
    }
    t_index = t_index + 1
  }
  
  return(t_settings_vector)
}

profilerGetContextDepth <- function(p_preset) {
  
  if (p_preset == "1.1 rigid-habits-no-infected") {
    return(1)
  }
  else if (p_preset == "1.2 rigid-habits-infected") {
    return(1)
  }
  else if (p_preset == "1.3 DCSD-1") {
    return(1)
  }
  else if (p_preset == "1.4 DCSD-1-leisure-habits") {
    return(1)
  }
  else if (p_preset == "2.1 DCSD-2") {
    return(2)
  }
  else if (p_preset == "2.2 DCSD-2-obligation-constraint") {
    return(2)
  }
  else if (p_preset == "3.1 DCSD-3-rigid-norms") {
    return(3)
  }
  else if (p_preset == "3.2 DCSD-3-rigid-norms-lockdown") {
    return(3)
  }
  else if (p_preset == "3.3 DCSD-3") {
    return(3)
  }
  else if (p_preset == "3.4 DCSD-3-lockdown") {
    return(3)
  }
  else if (p_preset == "4.1 DCSD-4") {
    return(4)
  }
  else if (p_preset == "5.0 DCSD-5-optimisation-no-infections") {
    return(5)
  }
  else if (p_preset == "5.1 DCSD-5-optimisation") {
    return(5)
  }
  else if (p_preset == "5.2 DCSD-5-optimisation-lockdown") {
    return(5)
  }
  else if (p_preset == "0.0 Original ASSOCC-no-infections") {
    return(0)
  }
  else if (p_preset == "0.1 Original ASSOCC") {
    return(0)
  }
  else if (p_preset == "0.2 Original ASSOCC-lockdown") {
    return(0)
  }
}

profilerPrepareOverviewDataframe <- function(df_p_overview) {
  
  # Remove the "CONTEXT-DELIBERATION-SELECT-ACTIVITY"
  df_p_overview <- df_p_overview[!grepl("CONTEXT-DELIBERATION-SELECT-ACTIVITY", df_p_overview$function_name), ]
  # Rename
  df_p_overview$function_name[df_p_overview$function_name == "SELECT-ACTIVITY"] <- "CONTEXT-SELECT-ACTIVITY"
  # Rename the "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR" to "FULL ASSOCC DELIBERATION"
  df_p_overview$function_name[df_p_overview$function_name == "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR"] <- "FULL ASSOCC DELIBERATION"
  
  # There are some double entries in the dataframe, so I need to remove them
  df_p_overview <- df_p_overview[!duplicated(df_p_overview), ]
  
  # For df_p_overview I want to divide incl_t_ms by the number of calls
  df_p_overview$incl_t_ms_per_call <- df_p_overview$incl_t_ms / df_p_overview$calls
  
  # Add a column for the agent numberss
  df_p_overview$agents <- c(1004, 2008, 4016, 6016, 8024, 10028)[match(df_p_overview$households, c(350, 700, 1400, 2100, 2800, 3500))]
  
  return(df_p_overview)
}

#--------------------------------------------------------------------------------------------------------
# Calculate the mean and sd of the data || # --- NOTE!! --- # if there is only ONE experiment: SD = NA ||
#--------------------------------------------------------------------------------------------------------

profilerGetOverviewMean <- function(df_p_overview) {
  df_p_overview_mean <- df_p_overview %>% 
    group_by(preset, function_name, agents) %>% 
    summarise(calls_mean = mean(calls), calls_sd = sd(calls),
              incl_t_ms_mean = mean(incl_t_ms), incl_t_ms_sd = sd(incl_t_ms),
              excl_t_ms_mean = mean(excl_t_ms), excl_t_ms_sd = sd(excl_t_ms),
              excl_calls_mean = mean(excl_calls), excl_calls_sd = sd(excl_calls))
  
  return(df_p_overview_mean)
}