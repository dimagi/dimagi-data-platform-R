#This code to is creae visuals for the ITID journal article:
#https://docs.google.com/a/dimagi.com/document/d/1AuFF40FMGfe49wWYVhvLQ0DEvGuSayAHGk4o8b67N4I/edit
#The datasets used to generate the following visuals are located here:
#fullset.csv:
#test1_data.csv:
#test2_data.csv:

#Number of active users: use fullset
overall <- fullset %>% group_by(calendar_month) %>% 
  summarise(sum_user = length(unique(user_pk)))

p_users <- ggplot(overall, aes(x=calendar_month, y=sum_user)) +
  geom_point(size = 1.5, shape = 19, alpha = 0.5, colour = "darkblue", 
             fill = "lightblue") +
  geom_line(colour = "darkblue") +
  xlab("Calendar month") +
  ylab("Number (#) of active CHWs") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=9), 
        axis.title=element_text(size=9))
  #ggtitle("Number (#) of active CHWs by month")

#Distribution of total n_active months per CHW: use fullset
n_months <- fullset %>% group_by(user_pk) %>% 
  summarise(nmonths = length(calendar_month))

g_nmonths <- ggplot(n_months, aes(x=nmonths, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  xlab("Total number (#) of active months per CHW") +
  ylab("Number (#) of CHWs") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=9), 
        axis.title=element_text(size=9)) + 
  geom_vline(xintercept = median(n_months$nmonths), linetype = "dashed")

#Univariate density plots: use fullset
#Density plots with semi-transparent fill
g_forms <- ggplot(fullset, aes(x=nforms, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,150)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "nforms"], linetype = "dashed") + 
  xlab("# forms")

g_nvisits <- ggplot(fullset, aes(x=nvisits, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "nvisits"], linetype = "dashed") + 
  xlab("# visits")

g_inter <- ggplot(fullset, aes(x=ninteractions, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,100)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "ninteractions"], linetype = "dashed") + 
  xlab("# interactions")

g_cases <- ggplot(fullset, aes(x=ncases_touched, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,70)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "ncases_touched"], linetype = "dashed") + 
  xlab("# cases")

g_cases_reg <- ggplot(fullset, aes(x=ncases_registered, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,30)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "ncases_registered"], linetype = "dashed") + 
  xlab("# cases registered")

g_cases_fu <- ggplot(fullset, aes(x=nunique_followups, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,60)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) +  
  geom_vline(xintercept = summary_stats["median", "nunique_followups"], linetype = "dashed") + 
  xlab("# cases followed-up")

g_nvisits_fu <- ggplot(fullset, aes(x=register_followup, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,80)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "register_followup"], linetype = "dashed") + 
  xlab("# follow-up visits")

g_pervisits_fu <- ggplot(fullset, aes(x=case_register_followup_rate, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,100)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "case_register_followup_rate"], linetype = "dashed") + 
  xlab("% of follow-up visits")

g_per_active <- ggplot(fullset, aes(x=active_day_percent, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,100)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "active_day_percent"], linetype = "dashed") + 
  xlab("% of active days")

g_med_vis_day <- ggplot(fullset, aes(x=median_visits_per_day, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,10), breaks=c(1,3,5,7,9)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "median_visits_per_day"], linetype = "dashed") + 
  xlab("Median # visits per active day")

g_med_vis_dur <- ggplot(fullset, aes(x=median_visit_duration, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,15)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = summary_stats["median", "median_visit_duration"], linetype = "dashed") + 
  xlab("Median visit duration (minutes)")

g_time_cc <- ggplot(fullset, aes(x=time_using_cc, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,800)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=7)) + 
  geom_vline(xintercept = summary_stats["median", "time_using_cc"], linetype = "dashed") + 
  xlab("Total duration of CommCare use (minutes)")

pdf("foo.pdf")
grid.arrange(g_forms, g_nvisits, g_inter, g_cases, g_cases_reg, g_cases_fu, 
             g_nvisits_fu, g_pervisits_fu, g_per_active, g_med_vis_day, 
             g_med_vis_dur, g_time_cc, nrow = 4, ncol=3)
dev.off()

#Intra-user correlation: use test2_data
g1 <- ggplot(test2_data, aes(x=prev_active_day_percent, y=active_day_percent)) +
  geom_point(shape=1, size = 0.3) +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.70", parse = T, x=13, y=95) + 
  xlab("% of active days (month A)") +
  ylab("% of active days (month B)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8))

g2 <- ggplot(test2_data, aes(x=prev_nforms, y=nforms)) +
  geom_point(shape=1, size = 0.3) +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.68", parse = T, x=13, y=97) + 
  xlab("# forms (month A)") +
  ylab("# forms (month B)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8))

g3 <- ggplot(test2_data, aes(x=prev_time_using_cc, y=time_using_cc)) +
  geom_point(shape=1, size = 0.3) +
  scale_x_continuous(limits=c(0,400)) +
  scale_y_continuous(limits=c(0,400)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.21", parse = T, x=55, y=395) + 
  xlab("Total duration of CommCare use (month A)") +
  ylab("Total duration of CommCare use (month B)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8))

g4 <- ggplot(test2_data, aes(x=prev_median_visit_duration, y=median_visit_duration)) +
  geom_point(shape=1, size = 0.3) +
  scale_x_continuous(limits=c(0,15)) +
  scale_y_continuous(limits=c(0,15)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.19", parse = T, x=4, y=14.5) + 
  xlab("Median visit duration (month A)") +
  ylab("Median visit duration (month B)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8))

pdf("foo.pdf")
grid.arrange(g1, g2, g3, g4, nrow = 2, ncol=2)
dev.off()

#Do CHWs have different levels of activity by calendar month or season?
g_season <- ggplot(season, aes(x=month, y=median_metric, group = metric, colour = metric)) +
  geom_line() +
  geom_point(size = 1.5, shape = 19, alpha = 0.5) +
  xlab("Month") +
  ylab("Median metric") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=9), 
        axis.title=element_text(size=9))


#------------------------------------------------------------------------#
# Not using these graphs
#------------------------------------------------------------------------#

#Intra-program correlation: use test1_data
g1 <- ggplot(test1_data, aes(x=diff_active_day_percent, y=med_active_day_percent_1a)) +
  geom_point(shape=1, size = 0.3) +
  scale_x_continuous(limits=c(-15,15)) +
  scale_y_continuous(limits=c(-15,15)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.23", parse = T, x=13, y=11) + 
  xlab("Difference in % active days (Individual CHW)") +
  ylab("Difference in % active days (Program median)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0)

g2 <- ggplot(test1_data, aes(x=diff_nforms, y=med_nforms_1a)) +
  geom_point(shape=1, size = 0.3) +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.21", parse = T, x=13, y=97) + 
  xlab("Difference in # forms (Individual CHW)") +
  ylab("Difference in # forms (Program median)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8))

g3 <- ggplot(test1_data, aes(x=diff_time_using_cc, y=med_time_using_cc_1a)) +
  geom_point(shape=1, size = 0.3) +
  scale_x_continuous(limits=c(0,400)) +
  scale_y_continuous(limits=c(0,400)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.10", parse = T, x=55, y=395) + 
  xlab("Difference in total duration CommCare use (Individual CHW)") +
  ylab("Difference in total duration CommCare use (Program median)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8))

g4 <- ggplot(test1_data, aes(x=diff_median_visit_duration, y=med_median_visit_duration_1a)) +
  geom_point(shape=1, size = 0.3) +
  scale_x_continuous(limits=c(0,15)) +
  scale_y_continuous(limits=c(0,15)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.00", parse = T, x=4, y=14.5) + 
  xlab("Difference in median visit duration (Individual CHW)") +
  ylab("Difference in median visit duration (Program median)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=8))

pdf("foo.pdf")
grid.arrange(g1, g2, g3, g4, nrow = 2, ncol=2)
dev.off()


  
