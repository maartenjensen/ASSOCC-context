behaviourPlot6Activities <- function() {
  
  
  #------------------------------------------------------------------
  #==================== Agent Activities All =================
  
  # "rest_at_home", "work_at_home", "work_at_work", "study_at_school", "study_at_university", "at_private_leisure", 
  # "at_public_leisure", "shop_groceries", "shop_luxury", "at_treatment"
  
  # Roll mean/moving average
  df_activities <- select(subset_df, tick, ce_context_depth, people_alive, shop_groceries, rest_at_home, shop_luxury,
                          at_private_leisure, at_public_leisure, study_at_school,
                          study_at_university, work_at_work, work_at_home, at_treatment)
  
  df_activities$rest_at_home[1] <- df_activities$people_alive[1]
  
  # for each column shop_groceries:at_treatment I want to mutate each column and multiply by 100
  for (column_i in 4:13)
  { df_activities[, column_i] <- (df_activities[, column_i] / df_activities$people_alive) * 100 }
  
  #k_rollmean = 27#27 # 19/21 is a fine number
  #decrease = 0 #floor(k_rollmean/2)
  #v_tick <- (1+decrease):(gl_limits_x_max-decrease)
  
  #df_activities_mean <- data.frame(v_tick, rep(df_activities$ce_context_depth[1], times=length(v_tick)))
  #for (column_i in 4:13)
  #{ df_activities_mean <- cbind(df_activities_mean, rollapplyr(df_activities[, column_i], k_rollmean, mean, partial = TRUE)) } #rollmean(df_activities[, column_i], k_rollmean)) }
  
  #colnames(df_activities_mean) <- c("tick", "ce_context_depth", "shop_groceries", "rest_at_home", "shop_luxury",
  #                                 "at_private_leisure", "at_public_leisure", "study_at_school",
  #                                 "study_at_university", "work_at_work", "work_at_home", "at_treatment")
  
  df_activities_gathered <- gather(df_activities, `Location Type`, measurement, shop_groceries:at_treatment)
  #df_activities_mean_gathered <- gather(df_activities_mean, `Location Type`, measurement, shop_groceries:at_treatment)
  
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
  p <- p + coord_cartesian(xlim = c(0, gl_limits_x_max), ylim = c(0, 100)) + labs(title=paste("Activities (", experiment_preset,") - Overall", sep=""))  
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