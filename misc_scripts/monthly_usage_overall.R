library(plyr)

#-----------------------------------------------------------------------------#
# Create ribbon plots instead of showing individual, grey domain lines
#-----------------------------------------------------------------------------#

# Active days per month - overall by month index
overall = ddply(all_monthly, c("obsnum"), summarise,
                act_days_med = median(active_days_per_month, na.rm = T),
                sd = sd(active_days_per_month, na.rm=T),
                n = sum(!is.na(active_days_per_month)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

g_active_days_overall = 
    ggplot(overall, aes(x = obsnum, y = act_days_med, 
                    ymax = (max(df$active_days_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = act_days_med - ci95, ymax = act_days_med + ci95),
                alpha = 0.2) +
    ggtitle("Active days (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Active days (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
    #theme_bw()

#-----------------------------------------------------------------------------#
# Visits - by month index

overall = ddply(all_monthly, .(obsnum), summarise,
                visits_med = median(visits, na.rm = T),
                sd = sd(visits, na.rm=T),
                n = sum(!is.na(visits)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

over_min_logical = (overall$visits_med - overall$ci95) < -3
over_max_logical = (overall$visits_med + overall$ci95) > 
    (max(df$visits_med, na.rm = T) + 5)

over_min=over_min_logical
over_min[over_min_logical==F] = 
    (overall$visits_med - overall$ci95)[over_min_logical==F]
over_min[over_min_logical==T] = -3

over_max=over_max_logical
over_max[over_max_logical==F] = 
    (overall$visits_med + overall$ci95)[over_max_logical==F]
over_max[over_max_logical==T] = (max(df$visits_med, na.rm = T) + 5)

    
g_visits_overall = 
    ggplot(overall, aes(x = obsnum, y = visits_med, 
                        ymax = (max(df$visits_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))

#-----------------------------------------------------------------------------#
# Visits per active day - by month index

overall = ddply(all_monthly, .(obsnum), summarise,
                visits_active_day_med = median(median_visits_per_active_day, 
                                               na.rm = T),
                sd = sd(median_visits_per_active_day, na.rm=T),
                n = sum(!is.na(median_visits_per_active_day)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

over_min_logical = (overall$visits_active_day_med - overall$ci95) < -3
over_max_logical = (overall$visits_active_day_med + overall$ci95) > 
    (max(df$visits_active_day_med, na.rm = T) + 5)

over_min=over_min_logical
over_min[over_min_logical==F] = 
    (overall$visits_active_day_med - overall$ci95)[over_min_logical==F]
over_min[over_min_logical==T] = -3

over_max=over_max_logical
over_max[over_max_logical==F] = 
    (overall$visits_active_day_med + overall$ci95)[over_max_logical==F]
over_max[over_max_logical==T] = (max(df$visits_active_day_med, na.rm = T) + 5)


g_visits_active_overall = 
    ggplot(overall, aes(x = obsnum, y = visits_active_day_med, 
                        ymax = (max(df$visits_active_day_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visits per active day (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Median visits per active day (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))

#-----------------------------------------------------------------------------#
# Cases registered - by month index

overall = ddply(all_monthly, .(obsnum), summarise,
                reg_med = median(case_registered, na.rm = T),
                sd = sd(case_registered, na.rm=T),
                n = sum(!is.na(case_registered)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

over_min_logical = (overall$reg_med - overall$ci95) < -3
over_max_logical = (overall$reg_med + overall$ci95) > 
    (max(df$reg_med, na.rm = T) + 5)

over_min=over_min_logical
over_min[over_min_logical==F] = 
    (overall$reg_med - overall$ci95)[over_min_logical==F]
over_min[over_min_logical==T] = -3

over_max=over_max_logical
over_max[over_max_logical==F] = 
    (overall$reg_med + overall$ci95)[over_max_logical==F]
over_max[over_max_logical==T] = (max(df$reg_med, na.rm = T) + 5)


g_reg_med_overall = 
    ggplot(overall, aes(x = obsnum, y = reg_med, 
                        ymax = (max(df$reg_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases registered (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases registered (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))

#-----------------------------------------------------------------------------#
# Cases followed-up - by month index

overall = ddply(all_monthly, .(obsnum), summarise,
                case_fu_med = median(follow_up_unique_case, na.rm = T),
                sd = sd(follow_up_unique_case, na.rm=T),
                n = sum(!is.na(follow_up_unique_case)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

over_min_logical = (overall$case_fu_med - overall$ci95) < -3
over_max_logical = (overall$case_fu_med + overall$ci95) > 
    (max(df$case_fu_med, na.rm = T) + 5)

over_min=over_min_logical
over_min[over_min_logical==F] = 
    (overall$case_fu_med - overall$ci95)[over_min_logical==F]
over_min[over_min_logical==T] = -3

over_max=over_max_logical
over_max[over_max_logical==F] = 
    (overall$case_fu_med + overall$ci95)[over_max_logical==F]
over_max[over_max_logical==T] = (max(df$case_fu_med, na.rm = T) + 5)


g_case_fu_overall = 
    ggplot(overall, aes(x = obsnum, y = case_fu_med, 
                        ymax = (max(df$case_fu_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases followed-up (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases followed-up (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))

#-----------------------------------------------------------------------------#
# Batch entry % - by month index

overall = ddply(all_monthly, .(obsnum), summarise,
                batch_per_med = median(batch_entry_percent, na.rm = T),
                sd = sd(batch_entry_percent, na.rm=T),
                n = sum(!is.na(batch_entry_percent)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

over_min_logical = (overall$batch_per_med - overall$ci95) < -3
over_max_logical = (overall$batch_per_med + overall$ci95) > 100

over_min=over_min_logical
over_min[over_min_logical==F] = 
    (overall$batch_per_med - overall$ci95)[over_min_logical==F]
over_min[over_min_logical==T] = -3

over_max=over_max_logical
over_max[over_max_logical==F] = 
    (overall$batch_per_med + overall$ci95)[over_max_logical==F]
over_max[over_max_logical==T] = 100


g_batch_per_overall = 
    ggplot(overall, aes(x = obsnum, y = batch_per_med, ymax = 100)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Batch entry (%) of travel visits by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Batch entry (%), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))

#-----------------------------------------------------------------------------#
# Median visit duration - by month index

overall = ddply(all_monthly, .(obsnum), summarise,
                visit_dur = median(median_visit_duration, na.rm = T),
                sd = sd(median_visit_duration, na.rm=T),
                n = sum(!is.na(median_visit_duration)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

over_min_logical = (overall$visit_dur - overall$ci95) < -3
over_max_logical = (overall$visit_dur + overall$ci95) > 
    (max(df$visit_dur, na.rm = T) + 5)

over_min=over_min_logical
over_min[over_min_logical==F] = 
    (overall$visit_dur - overall$ci95)[over_min_logical==F]
over_min[over_min_logical==T] = -3

over_max=over_max_logical
over_max[over_max_logical==F] = 
    (overall$visit_dur + overall$ci95)[over_max_logical==F]
over_max[over_max_logical==T] = (max(df$visit_dur, na.rm = T) + 5)


g_visit_dur_overall = 
    ggplot(overall, aes(x = obsnum, y = visit_dur, 
                        ymax = (max(df$visit_dur, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visit duration (mins) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visit duration (mins), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))

#-----------------------------------------------------------------------------#

# Morning/afternoon % - by month index

overall = ddply(all_monthly, .(obsnum), summarise,
                morn_aft_per_med = median(morn_aft_per, na.rm = T),
                sd = sd(morn_aft_per, na.rm=T),
                n = sum(!is.na(morn_aft_per)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

over_min_logical = (overall$morn_aft_per_med - overall$ci95) < -3
over_max_logical = (overall$morn_aft_per_med + overall$ci95) > 100

over_min=over_min_logical
over_min[over_min_logical==F] = 
    (overall$morn_aft_per_med - overall$ci95)[over_min_logical==F]
over_min[over_min_logical==T] = -3

over_max=over_max_logical
over_max[over_max_logical==F] = 
    (overall$morn_aft_per_med + overall$ci95)[over_max_logical==F]
over_max[over_max_logical==T] = 100


g_morn_aft_per_overall = 
    ggplot(overall, aes(x = obsnum, y = morn_aft_per_med, ymax = 100)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Morning/afternoon visits (%) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Morning/afternoon visits (%), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))

#-----------------------------------------------------------------------------#
