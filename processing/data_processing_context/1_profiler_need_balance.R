profilerLoadData <- function(p_filepath_workspace, p_filenames_profiler) {
  
  df_results = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, global_lockdown=NA,
                          function_name=NA, calls=NA, incl_t_ms=NA, excl_t_ms=NA, excl_calls=NA)[numeric(0), ]
  
  print("- GO Function")
  print("----------------------------------")
  
  # Read in datafiles using filesNames and filesPath variables
  for (i in 1:length(p_filenames_profiler)) {
    file_name = p_filenames_profiler[i]
    file_path_name = paste(p_filepath_workspace, p_filenames_profiler[i], sep="/")

    df_initial = read.csv(file_path_name, skip = 2)
    for (i in 1:nrow(df_initial)) {

      if (str_contains(df_initial[i,1], "GO ") || str_contains(df_initial[i,1], "SELECT-ACTIVITY ") ||
          str_contains(df_initial[i,1], "CONTEXT") || str_contains(df_initial[i,1], "MY-PREFERRED-AVAILABLE-ACTIVITY-DESCRIPTOR") ||
          str_contains(df_initial[i,1], "CSN") || str_contains(df_initial[i,1], "CSSN") ||
          str_contains(df_initial[i,1], "CSO") || str_contains(df_initial[i,1], "CSSO") ||
          str_contains(df_initial[i,1], "CSFT") || str_contains(df_initial[i,1], "CSSFT")) {

        result_v = profilerStrToVWithoutWhiteSpaces(df_initial[i,1])
        settings_v = profilerSettingStrToV(file_name)
        df_new_line = c(settings_v[1], settings_v[2], settings_v[3], settings_v[4], settings_v[5],
                         result_v[1], as.double(result_v[2]), result_v[3], result_v[4], result_v[5])
        df_results = rbind(df_results, df_new_line)
      }
      if (str_contains(df_initial[i,1], "Sorted by Inclusive Time")) {
        break
      }
    }
  }
  
  # Making the Dataframe nice
  colnames(df_results) <- c("context", "households", "random_seed", "action_space", "global_lockdown", "function_name", "calls", "incl_t_ms", "excl_t_ms", "excl_calls")
  df_results$households = as.integer(df_results$households)
  df_results$random_seed = as.integer(df_results$random_seed)
  df_results$action_space = as.integer(df_results$action_space)
  df_results$calls = as.double(df_results$calls)
  df_results$incl_t_ms = as.double(df_results$incl_t_ms)
  df_results$excl_t_ms = as.double(df_results$excl_t_ms)
  df_results$excl_calls = as.double(df_results$excl_calls)
  
  df_results <- df_results[order(df_results$context, df_results$function_name), ]
  
  return(df_results)
}

profilerLoadSpecificData <- function(p_df_profiler, p_string) {
  
  df_profiler_specific = data.frame(context=NA, households=NA, random_seed=NA, action_space=NA, global_lockdown=NA,
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

profilerPlotNeedBalance <- function(df_profiler, output_dir, one_plot) {
  
  
  print("Profiler plotting function")
  
  
}

profilerSummarize <- function(df_profiler, df_profiler_overview) {
  
  print("Profiler summary")
  print(df_profiler_overview)
  # It depends on what I want to know, what is going to be in this function.
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
  str_split <- strsplit(p_str, "")[[1]]
  # Check C= False or True
  while (length(t_settings_vector) == 0) {
    if (str_split[t_index] == "C") {
      t_index = t_index + 3
      t_settings_vector <- c(t_settings_vector, str_split[t_index])
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
  # Check L= Global Lockdown
  while (length(t_settings_vector) == 4) {
    if (str_split[t_index] == "L") {
      t_index = t_index + 3
      if (str_split[t_index] == "f") { t_settings_vector <- c(t_settings_vector, FALSE); t_index = t_index + 1 }
      else if (str_split[t_index] == "t") { t_settings_vector <- c(t_settings_vector, TRUE) }
      t_settings_vector <- c(t_settings_vector, str_split[t_index])
    }
    t_index = t_index + 1
  }
  # Check N= Need balancing
  #while (length(t_settings_vector) == 4) {
  #  if (str_split[t_index] == "N") {
  #    t_index = t_index + 3
  #    t_settings_vector <- c(t_settings_vector, str_split[t_index])
  #  }
  #  t_index = t_index + 1
  #}
  # Check PR= Private Leisure Risk
  #while (length(t_settings_vector) == 5) {
  #  if (str_split[t_index] == "P") {
  #    t_index = t_index + 4
  #    t_settings_vector <- c(t_settings_vector, str_split[t_index])
  #  }
  #  t_index = t_index + 1
  #}
  
  return(t_settings_vector)
}
# Solution for tomorrow, a for loop through the characters and find C, then 3 steps further, then find H, then further until space, do the same for R and A, etc.
