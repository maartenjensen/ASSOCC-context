behaviourPlot6Activities <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #------------------------------------------------------------------
  #==================== Agent Activities All =================
  
  # "rest_at_home", "work_at_home", "work_at_work", "study_at_school", "study_at_university", "at_private_leisure", 
  # "at_public_leisure", "shop_groceries", "shop_luxury", "at_treatment"
  
  # Roll mean/moving average
  df_activities <- select(subset_df, tick, ce_context_depth, people_alive, shop_groceries_perc, rest_at_home_perc, shop_luxury_perc,
                          at_private_leisure_perc, at_public_leisure_perc, study_at_school_perc,
                          study_at_university_perc, work_at_work_perc, work_at_home_perc, at_treatment_perc)
  
  df_activities$rest_at_home[1] <- df_activities$people_alive[1]

  
  #k_rollmean = 27#27 # 19/21 is a fine number
  #decrease = 0 #floor(k_rollmean/2)
  #v_tick <- (1+decrease):(gl_limits_x_max-decrease)
  
  #df_activities_mean <- data.frame(v_tick, rep(df_activities$ce_context_depth[1], times=length(v_tick)))
  #for (column_i in 4:13)
  #{ df_activities_mean <- cbind(df_activities_mean, rollapplyr(df_activities[, column_i], k_rollmean, mean, partial = TRUE)) } #rollmean(df_activities[, column_i], k_rollmean)) }
  
  #colnames(df_activities_mean) <- c("tick", "ce_context_depth", "shop_groceries", "rest_at_home", "shop_luxury",
  #                                 "at_private_leisure", "at_public_leisure", "study_at_school",
  #                                 "study_at_university", "work_at_work", "work_at_home", "at_treatment")
  
  df_activities_gathered <- gather(df_activities, `Activity`, measurement, shop_groceries_perc:at_treatment_perc)
  #df_activities_mean_gathered <- gather(df_activities_mean, `Activity`, measurement, shop_groceries:at_treatment)
  
  p <- ggplot(df_activities_gathered, aes(x = tick, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('shop_groceries_perc'='Shop groceries', 'rest_at_home_perc'='Rest at home', 'shop_luxury_perc'='Shop luxury',
             'at_private_leisure_perc'='Leisure at Pr', 'at_public_leisure_perc'='Leisure at Pu', 'study_at_school_perc'='Study at school',
             'study_at_university_perc'='Study at uni', 'work_at_work_perc'='Work at work', 'work_at_home_perc'='Work at home', 'at_treatment_perc'='Treatment'),
    values=c('#881556','#80e389','#f2ccd5',
             '#f16a15','#d73229','#9d6e48', 
             '#E69F00','#345da9','#000000','#8d8d8d'),
    breaks=c('shop_groceries_perc', 'rest_at_home_perc', 'shop_luxury_perc',
             'at_private_leisure_perc', 'at_public_leisure_perc', 'study_at_school_perc',
             'study_at_university_perc', 'work_at_work_perc', 'work_at_home_perc', 'at_treatment_perc'))
  p <- p + xlab("Ticks") + ylab("% Activities Chosen") + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Activities (", experiment_preset,") - Overall", sep=""))  
  p_smooth <- p + geom_smooth() # se = True (confidence interval), span = .2 span = 0.75 (default = 0.75), method = 'lm' (for a linear line)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_smooth", sep="")) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}

behaviourPrepareActivitiesSimplified4 <- function() {
  
  df_activities <- select(subset_df, tick, ce_context_depth, people_alive, shop_groceries_perc, rest_at_home_perc, shop_luxury_perc,
                          at_private_leisure_perc, at_public_leisure_perc, study_at_school_perc,
                          study_at_university_perc, work_at_work_perc, work_at_home_perc, at_treatment_perc)
  
  df_activities$rest_at_home_perc[1] <- 100 # Everyone is at home, but since the motivation is not taken into account, all cells in the first row indicate 0
  
  #----- Prepare the data frame ------
  df_activities <- df_activities %>% mutate(rest_at_home = rest_at_home_perc)
  df_activities <- df_activities %>% mutate(obligation = study_at_school_perc + study_at_university_perc + work_at_work_perc + work_at_home_perc)
  df_activities <- df_activities %>% mutate(shopping = shop_groceries_perc + shop_luxury_perc)
  df_activities <- df_activities %>% mutate(leisure = at_private_leisure_perc + at_public_leisure_perc)
  
  df_activities <- select(df_activities, tick, ce_context_depth, rest_at_home, obligation, shopping, leisure)
  
  return(df_activities)
}

# Perhaps this function could be split into specifically creating the smoothing and the day average (FUTURE WORK)
behaviourPlot6ActivitiesSimplified4 <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #------------------------------------------------------------------
  #=================== Agent Activities Simplified 4 ================
  df_activities <- behaviourPrepareActivitiesSimplified4()
  
  #----- Gather data for the plot -----
  df_activities_mean_gathered <- gather(df_activities, `Activity`, measurement, rest_at_home:leisure)
  
  p <- ggplot(df_activities_mean_gathered, aes(x = tick, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('rest_at_home'='Rest at Home', 'obligation'='Work or Study', 'shopping'='Shopping', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#f16a15'),
    breaks=c('rest_at_home', 'obligation', 'shopping', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) 
  p <- p + xlab("Time (Ticks)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .7) + labs(title=paste("Activities (", experiment_preset,") - Simplified Smooth", sep=""))  
  p <- p + geom_line() + labs(title=paste("Activities (", experiment_preset,") - Simplified", sep=""))  
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_4", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_4_smooth", sep="")) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  #=========================
  # Activities Day: +3 is added because the first day is not taken into account, the original formula was: (tick - (tick %% 4)) / 4
  df_activities_day <- df_activities %>% mutate(day = (tick + 3 - ((tick + 3) %% 4)) / 4)
  # mean for each day
  df_activities_day <- df_activities_day %>% group_by(day) %>% summarise_all(mean)
  # remove column tick
  df_activities_day <- select(df_activities_day, -tick)
  
  df_activities_day_gathered <- gather(df_activities_day, `Activity`, measurement, rest_at_home:leisure)
  
  p <- ggplot(df_activities_day_gathered, aes(x = day, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('rest_at_home'='Rest at Home', 'obligation'='Work or Study', 'shopping'='Shopping', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#f16a15'),
    breaks=c('rest_at_home', 'obligation', 'shopping', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max/4), ylim = c(0, 100)) + labs(title=paste("Activities (", experiment_preset,") - Simplified Day", sep=""))  
  p <- p + xlab("Time (Days)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .75)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_day", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
}

behaviourPlot6ActivitiesSimplified4Leisure <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #------------------------------------------------------------------
  #=================== Agent Activities Simplified 4 ================
  df_activities <- behaviourPrepareActivitiesSimplified4()
  
  #----- Gather data for the plot -----
  df_activities_mean_gathered <- gather(df_activities, `Activity`, measurement, rest_at_home:leisure)
  
  p <- ggplot(df_activities_mean_gathered, aes(x = tick, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('rest_at_home'='Rest at Home', 'obligation'='Work or Study', 'shopping'='Shopping', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#f16a15'),
    breaks=c('rest_at_home', 'obligation', 'shopping', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) 
  p <- p + xlab("Time (Ticks)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .7) + labs(title=paste("Activities (", experiment_preset,") - Simplified Smooth", sep="")) + gghighlight::gghighlight(`Activity` == "leisure")
  p <- p + geom_line() + labs(title=paste("Activities (", experiment_preset,") - Simplified", sep=""))  + gghighlight::gghighlight(`Activity` == "leisure")
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_leisure_highlight", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_leisure_highlight_smooth", sep="")) }
  show(p_smooth) 
  if (plot_type == "one") { dev.off() }
}

behaviourPlot6ActivitiesSimplified4Leisure2Weeks <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #------------------------------------------------------------------
  #=================== Agent Activities Simplified 4 ================
  df_activities <- behaviourPrepareActivitiesSimplified4()
  
  #----- Gather data for the plot -----
  df_activities_mean_gathered <- gather(df_activities, `Activity`, measurement, leisure)
  
  p <- ggplot(df_activities_mean_gathered, aes(x = tick, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('leisure'='Leisure'),
    values=c('#f16a15'),
    breaks=c('leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, 28), ylim = c(0, 100)) 
  p <- p + xlab("Time (Ticks)") + ylab("% Activities Chosen") + labs(col="")
  p <- p + geom_line() + labs(title=paste("Activities (", experiment_preset,") - Simplified", sep=""))
  p <- p + scale_x_continuous(breaks=c(0,4,8,12,16,20,24,28))
  p <- p + annotate("text", x = 2, y=-2, label = "Mo")
  p <- p + annotate("text", x = 6, y=-2, label = "Tu")
  p <- p + annotate("text", x = 10, y=-2, label = "We")
  p <- p + annotate("text", x = 14, y=-2, label = "Th")
  p <- p + annotate("text", x = 18, y=-2, label = "Fr")
  p <- p + annotate("text", x = 22, y=-2, label = "Sa")
  p <- p + annotate("text", x = 26, y=-2, label = "Su")
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_leisure_highlight_2_week", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
}

# This code is used for the final comparison where we need to distinguish between luxury shopping and grocery shopping to explain the differences.
behaviourPlot6ActivitiesSimplified5 <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #------------------------------------------------------------------
  #==================== Agent Activities Simplified =================
  
  df_activities <- select(subset_df, tick, ce_context_depth, people_alive, shop_groceries_perc, rest_at_home_perc, shop_luxury_perc,
                          at_private_leisure_perc, at_public_leisure_perc, study_at_school_perc,
                          study_at_university_perc, work_at_work_perc, work_at_home_perc, at_treatment_perc)
  
  df_activities$rest_at_home_perc[1] <- 100 # Everyone is at home, but since the motivation is not taken into account, all cells in the first row indicate 0
  
  #----- Prepare the data frame ------
  df_activities_mean <- df_activities %>% mutate(work_rest_at_home = rest_at_home_perc + work_at_home_perc)
  df_activities_mean <- df_activities_mean %>% mutate(obligation_out = study_at_school_perc + study_at_university_perc + work_at_work_perc)
  df_activities_mean <- df_activities_mean %>% mutate(shop_grocery = shop_groceries_perc)
  df_activities_mean <- df_activities_mean %>% mutate(shop_luxury = shop_luxury_perc)
  df_activities_mean <- df_activities_mean %>% mutate(leisure = at_private_leisure_perc + at_public_leisure_perc)
  
  #----- Print the data for the activity averages -----
  print(plot_base_name)
  print(paste("RH/WH & WW/S & GROC & LUX & LEI \\" , sep=""))

  print(paste(round(mean(df_activities_mean$work_rest_at_home), digits = 2), " & ", round(mean(df_activities_mean$obligation_out), digits = 2), " & ", 
              round(mean(df_activities_mean$shop_grocery), digits = 2), " & ", round(mean(df_activities_mean$shop_luxury), digits = 2)," & ",
              round(mean(df_activities_mean$leisure), digits = 2), " \\", sep=""))

  #df_activities_mean <- select(df_activities_mean, tick, ce_context_depth, work_rest_at_home, obligation_out, shop_grocery, shop_luxury, leisure)
  df_activities_mean <- select(df_activities_mean, tick, ce_context_depth, shop_grocery, shop_luxury, leisure)
  
  #----- Gather data for the plot -----
  #df_activities_mean_gathered <- gather(df_activities_mean, `Activity`, measurement, work_rest_at_home:leisure)
  df_activities_mean_gathered <- gather(df_activities_mean, `Activity`, measurement, shop_grocery:shop_luxury)
  
  #labels=c('work_rest_at_home'='Rest or Work at Home', 'obligation_out'='Work or Study out', 'shop_grocery'='Shop grocery', 'shop_luxury'='Shop luxury', 'leisure'='Leisure'),
  #values=c('#197221','#33ddff','#881556','#9d6e48','#f16a15'),
  #breaks=c('work_rest_at_home', 'obligation_out', 'shop_grocery', 'shop_luxury', 'leisure')) + labs(col="")
  
  p <- ggplot(df_activities_mean_gathered, aes(x = tick, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('shop_grocery'='Shop grocery', 'shop_luxury'='Shop luxury'),
    values=c('#881556','#9d6e48'),
    breaks=c('shop_grocery', 'shop_luxury')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 15)) + labs(title=paste("Activities (", experiment_preset,") - Overall", sep=""))  
  p <- p + xlab("Time (Ticks)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .7)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_5", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_5_smooth", sep="")) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  #==================== Agent Activities Simplified - SMOOTH THE LINES - DAY =================
  
  df_activities_day <- df_activities_mean %>% mutate(day = (tick + 3 - ((tick + 3) %% 4)) / 4)
  # Print the averages for comparison
  
  # mean for each day
  df_activities_day <- df_activities_day %>% group_by(day) %>% summarise_all(mean)
  # remove column tick
  df_activities_day <- select(df_activities_day, -tick)
  
  df_activities_day_gathered <- gather(df_activities_day, `Activity`, measurement, shop_grocery:shop_luxury)
  
  p <- ggplot(df_activities_day_gathered, aes(x = day, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('shop_grocery'='Shop grocery', 'shop_luxury'='Shop luxury'),
    values=c('#881556','#9d6e48'),
    breaks=c('shop_grocery', 'shop_luxury')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max/4), ylim = c(0, 15)) + labs(title=paste("Activities (", experiment_preset,") - Overall", sep=""))  
  p <- p + xlab("Time (Days)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .75)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_5_day", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}

behaviourPlot6ActivitiesWorkStudyHome <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #------------------------------------------------------------------
  #==================== Agent Activities All =================
  
  # "rest_at_home", "work_at_home", "work_at_work", "study_at_school", "study_at_university", "at_private_leisure", 
  # "at_public_leisure", "shop_groceries", "shop_luxury", "at_treatment"
  
  # Roll mean/moving average
  df_activities <- select(subset_df, tick, ce_context_depth, people_alive, study_at_school_perc,
                          study_at_university_perc, work_at_work_perc, work_at_home_perc)
  
  df_activities$rest_at_home[1] <- df_activities$people_alive[1]
  
  
  df_activities <- df_activities %>% mutate(day = (tick + 3 - ((tick + 3) %% 4)) / 4)
  # mean for each day
  df_activities <- df_activities %>% group_by(day) %>% summarise_all(mean)
  # remove column tick
  df_activities <- select(df_activities, -tick)

  df_activities_gathered <- gather(df_activities, `Activity`, measurement, study_at_school_perc:work_at_home_perc)
  #df_activities_mean_gathered <- gather(df_activities_mean, `Activity`, measurement, shop_groceries:at_treatment)
  
  p <- ggplot(df_activities_gathered, aes(x = day, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('study_at_school_perc'='Study at school',
             'study_at_university_perc'='Study at uni', 'work_at_work_perc'='Work at work', 'work_at_home_perc'='Work at home'),
    values=c('#9d6e48', 
             '#E69F00','#345da9','#000000'),
    breaks=c('study_at_school_perc',
             'study_at_university_perc', 'work_at_work_perc', 'work_at_home_perc'))
  p <- p + xlab("Days") + ylab("% Activities Chosen") + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max/4), ylim = c(0, 25)) + labs(title=paste("Activities (", experiment_preset,") - Work Study", sep=""))  
  p_smooth <- p + geom_smooth() # se = True (confidence interval), span = .2 span = 0.75 (default = 0.75), method = 'lm' (for a linear line)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_work_study", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_work_study_smooth", sep="")) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  print("-- ... finished!")
}

behaviourPlot6ActivitiesWorkStudy <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #------------------------------------------------------------------
  #==================== Agent Activities All =================
  
  # "rest_at_home", "work_at_home", "work_at_work", "study_at_school", "study_at_university", "at_private_leisure", 
  # "at_public_leisure", "shop_groceries", "shop_luxury", "at_treatment"
  
  # Roll mean/moving average
  df_activities <- select(subset_df, tick, ce_context_depth, people_alive, study_at_school_perc,
                          study_at_university_perc, work_at_work_perc)
  
  df_activities$rest_at_home[1] <- df_activities$people_alive[1]
  
  
  df_activities_gathered <- gather(df_activities, `Activity`, measurement, study_at_school_perc:work_at_work_perc)
  
  p <- ggplot(df_activities_gathered, aes(x = tick, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('study_at_school_perc'='Study at school',
             'study_at_university_perc'='Study at uni', 'work_at_work_perc'='Work at work'),
    values=c('#9d6e48', 
             '#E69F00','#345da9'),
    breaks=c('study_at_school_perc', 'study_at_university_perc', 'work_at_work_perc'))
  p <- p + xlab("Ticks") + ylab("% Activities Chosen") + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 25)) + labs(title=paste("Activities (", experiment_preset,") - Work Study", sep=""))  
  p_smooth <- p + geom_smooth() # se = True (confidence interval), span = .2 span = 0.75 (default = 0.75), method = 'lm' (for a linear line)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_work_study", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_work_study_smooth", sep="")) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  # Day
  df_activities <- df_activities %>% mutate(day = (tick + 3 - ((tick + 3) %% 4)) / 4)
  # mean for each day
  df_activities <- df_activities %>% group_by(day) %>% summarise_all(mean)
  # remove column tick
  df_activities <- select(df_activities, -tick)
  
  df_activities_gathered <- gather(df_activities, `Activity`, measurement, study_at_school_perc:work_at_work_perc)
  #df_activities_mean_gathered <- gather(df_activities_mean, `Activity`, measurement, shop_groceries:at_treatment)
  
  p <- ggplot(df_activities_gathered, aes(x = day, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('study_at_school_perc'='Study at school', 'study_at_university_perc'='Study at uni', 'work_at_work_perc'='Work at work'),
    values=c('#9d6e48', '#E69F00','#345da9'),
    breaks=c('study_at_school_perc', 'study_at_university_perc', 'work_at_work_perc'))
  p <- p + xlab("Days") + ylab("% Activities Chosen") + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max/4), ylim = c(0, 25)) + labs(title=paste("Activities (", experiment_preset,") - Work Study", sep=""))  
  p_smooth <- p + geom_smooth() # se = True (confidence interval), span = .2 span = 0.75 (default = 0.75), method = 'lm' (for a linear line)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_work_study_day", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  
  print("-- ... finished!")
}

#==========================================
# 3. NORMATIVE
#==========================================
behaviourPrepareActivitiesSimplified4RestAndWorkHome <- function() {
  
  df_activities <- select(subset_df, tick, ce_context_depth, people_alive, shop_groceries_perc, rest_at_home_perc, shop_luxury_perc,
                          at_private_leisure_perc, at_public_leisure_perc, study_at_school_perc,
                          study_at_university_perc, work_at_work_perc, work_at_home_perc, at_treatment_perc)
  
  df_activities$rest_at_home_perc[1] <- 100 # Everyone is at home, but since the motivation is not taken into account, all cells in the first row indicate 0
  
  #----- Prepare the data frame ------
  df_activities <- df_activities %>% mutate(work_rest_at_home = rest_at_home_perc + work_at_home_perc)
  df_activities <- df_activities %>% mutate(obligation_out = study_at_school_perc + study_at_university_perc + work_at_work_perc)
  df_activities <- df_activities %>% mutate(shopping = shop_groceries_perc + shop_luxury_perc)
  df_activities <- df_activities %>% mutate(leisure = at_private_leisure_perc + at_public_leisure_perc)
  
  df_activities <- select(df_activities, tick, ce_context_depth, work_rest_at_home, obligation_out, shopping, leisure)
  
  return(df_activities)
}

behaviourPlot6ActivitiesSimplified4RestAndWorkHome <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #----------------------------------------------------------------------------------------
  #=================== Agent Activities Simplified 4 Work and Rest at home ================
  df_activities <- behaviourPrepareActivitiesSimplified4RestAndWorkHome()
  
  #----- Gather data for the plot -----
  df_activities_mean_gathered <- gather(df_activities, `Activity`, measurement, work_rest_at_home:leisure)
  
  p <- ggplot(df_activities_mean_gathered, aes(x = tick, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('work_rest_at_home'='Rest or Work at Home', 'obligation_out'='Work or Study out', 'shopping'='Shopping', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#f16a15'),
    breaks=c('work_rest_at_home', 'obligation_out', 'shopping', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) 
  p <- p + xlab("Time (Ticks)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .7) + labs(title=paste("Activities (", experiment_preset,") - Simplified Smooth", sep=""))  
  p <- p + geom_line() + labs(title=paste("Activities (", experiment_preset,") - Simplified", sep=""))  
  
  # Add rectangle for global lockdown
  #if (gl_limits_x_max > 300) {
    # start-tick-of-global-quarantine
  #  p <- p + geom_rect(aes(xmin = 26, xmax = (26 + 56 * 4), ymin = 0, ymax = 100), fill = "red", alpha = 0.002)
  #}
  #show(p)
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_4_rest_and_work_home", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_4_rest_and_work_home_smooth", sep="")) }
  show(p_smooth)
  if (plot_type == "one") { dev.off() }
  
  #=========================
  # Activities Day
  df_activities_day <- df_activities %>% mutate(day = (tick + 3 - ((tick + 3) %% 4)) / 4)
  # mean for each day
  df_activities_day <- df_activities_day %>% group_by(day) %>% summarise_all(mean)
  # remove column tick
  df_activities_day <- select(df_activities_day, -tick)
  
  df_activities_day_gathered <- gather(df_activities_day, `Activity`, measurement, work_rest_at_home:leisure)
  
  p <- ggplot(df_activities_day_gathered, aes(x = day, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('work_rest_at_home'='Rest or Work at Home', 'obligation_out'='Work or Study out', 'shopping'='Shopping', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#f16a15'),
    breaks=c('work_rest_at_home', 'obligation_out', 'shopping', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max/4), ylim = c(0, 100)) + labs(title=paste("Activities (", experiment_preset,") - Simplified Day", sep=""))  
  p <- p + xlab("Time (Days)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .75)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_4_day_rest_and_work_home", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
}


behaviourPlot6ActivitiesSimplified4RestAndWorkHomeDay <- function(plot_specific_f_name) {
  
  cat("-- Plot", plot_specific_f_name, "...\n")
  
  #----------------------------------------------------------------------------------------
  #=================== Agent Activities Simplified 4 Work and Rest at home ================
  df_activities <- behaviourPrepareActivitiesSimplified4RestAndWorkHome()
  
  #=========================
  # Activities Day
  df_activities_day <- df_activities %>% mutate(day = (tick + 3 - ((tick + 3) %% 4)) / 4)
  # mean for each day
  df_activities_day <- df_activities_day %>% group_by(day) %>% summarise_all(mean)
  # remove column tick
  df_activities_day <- select(df_activities_day, -tick)
  
  df_activities_day_gathered <- gather(df_activities_day, `Activity`, measurement, work_rest_at_home:leisure)
  
  p <- ggplot(df_activities_day_gathered, aes(x = day, y = measurement, col=`Activity`))
  p <- p + scale_colour_manual(
    labels=c('work_rest_at_home'='Rest or Work at Home', 'obligation_out'='Work or Study out', 'shopping'='Shopping', 'leisure'='Leisure'),
    values=c('#197221','#33ddff','#881556','#f16a15'),
    breaks=c('work_rest_at_home', 'obligation_out', 'shopping', 'leisure')) + labs(col="")
  p <- p + theme_bw()
  p <- p + theme(legend.position="bottom", text = element_text(size=16)) + guides(fill=guide_legend(nrow=1, byrow=TRUE))
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max/4), ylim = c(0, 100)) + labs(title=paste("Activities (", experiment_preset,") - Simplified Day", sep=""))  
  p <- p + xlab("Time (Days)") + ylab("% Activities Chosen") + labs(col="")
  p_smooth <- p + geom_smooth(se = TRUE, span = .75)
  p <- p + geom_line()
  
  if (plot_type == "one") { behaviourEnablePdf(paste(plot_base_name, "_activities_4_day_rest_and_work_home", sep="")) }
  show(p)
  if (plot_type == "one") { dev.off() }
}