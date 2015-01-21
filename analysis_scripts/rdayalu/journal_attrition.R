training_typical <- arrange(fullset, user_pk, calendar_month)
n_months <- fullset %>% group_by(user_pk) %>% 
  summarise(nmonths = length(calendar_month))
n_months <- filter(n_months, nmonths >=4)
training_typical <- training_typical[training_typical$user_pk %in% n_months$user_pk,]
users <- unique(training_typical$user_pk)


#Create attrition table for each indicator
leadup <- data.frame(matrix(ncol = 8, nrow = 1)) 
names(leadup) <- c("user_pk", "calendar_month", "previous_three_months_active", 
                   "next_three_months_active", "month_1", "month_2", "month_3", 
                   "month_4")
leadup$calendar_month <- as.Date(leadup$calendar_month)
leadup$user_pk <- as.numeric(leadup$user_pk)
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #single_user$calendar_month <- as.Date(single_user$calendar_month)
  for (j in 1:(nrow(single_user)-3)) {
    leadup_single <- data.frame(matrix(ncol = 8, nrow = 1)) 
    names(leadup_single) <- c("user_pk", "calendar_month", "previous_three_months_active", 
                              "next_three_months_active", "month_1", "month_2", "month_3", 
                              "month_4")
    leadup_single$user_pk <- as.numeric(single_user$user_pk[1])
    leadup_single$calendar_month <- as.Date(single_user$calendar_month[3+j])
    leadup_single$previous_three_months_active <- single_user$previous_three_months_active[3+j]
    leadup_single$next_three_months_active <- single_user$next_three_months_active[3+j]
    leadup_single$month_1 <- single_user$nunique_followups[3+j]
    leadup_single$month_2 <- single_user$nunique_followups[2+j]
    leadup_single$month_3 <- single_user$nunique_followups[1+j]
    leadup_single$month_4 <- single_user$nunique_followups[j]
    leadup <- rbind(leadup, leadup_single)
  }
}
leadup <- leadup[!(is.na(leadup$calendar_month)),]
leadup <- filter(leadup, next_three_months_active == F & previous_three_months_active == T)
leadup_3 <- c(median(leadup$month_1), median(leadup$month_2), 
              median(leadup$month_3), median(leadup$month_4))


#leadup_values <- c()
leadup_values <- c(leadup_values, leadup_3)

#Create dataset for graphing
leadup_indicators <- c(rep("# visits", 4), rep("% active days", 4), rep("# forms", 4), 
                      rep("median visit duration", 4), rep("median visits per day", 4), 
                      rep("total duration using CC", 4), rep("# interactions", 4),
                      rep("# cases registered", 4), rep("# follow-up visits", 4), 
                      rep("% follow-up visits", 4), rep("# cases", 4), 
                      rep("# cases followed-up", 4))
leadup_data <- data.frame(cbind(rep(c(1:4), 12), leadup_indicators, leadup_values))
names(leadup_data) <- c("months_prior", "metric", "metric_values")
leadup_data$metric_values <- as.numeric(levels(leadup_data$metric_values))[leadup_data$metric_values]
leadup_data$months_prior <- as.factor(leadup_data$months_prior)
month_levels <- rev(levels(leadup_data$months_prior))

# percent active days and # forms
attrition_eval <- filter(leadup_data, metric == "% active days" | metric == "# forms")
g_att_eval <- ggplot(attrition_eval, aes(x = months_prior, y = metric_values, colour = metric, group = metric)) +
  geom_line() +
  geom_point(size = 1.5, shape = 19, alpha = 0.5) +
  scale_y_continuous(limits=c(0,20)) +
  scale_x_discrete(limits = month_levels) +
  xlab("# months prior to attrition event") +
  ylab("Median metric") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=9), 
        axis.title=element_text(size=9))

# Remaining 10 metrics
attrition_eval <- filter(leadup_data, metric != "% active days" & metric != "# forms")
attrition_eval$overall_max <- c(rep(max(fullset$nvisits), 4), rep(max(fullset$median_visit_duration), 4), 
                        rep(max(fullset$median_visits_per_day), 4), rep(max(fullset$time_using_cc), 4), 
                        rep(max(fullset$ninteractions), 4), rep(max(fullset$ncases_registered), 4), 
                        rep(max(fullset$register_followup), 4), rep(max(fullset$case_register_followup_rate), 4), 
                        rep(max(fullset$ncases_touched), 4), rep(max(fullset$nunique_followups), 4))
attrition_eval$normalized_median <- (attrition_eval$metric_values/attrition_eval$overall_max)*100

g_att_overall <- ggplot(attrition_eval, aes(x = months_prior, y = normalized_median, colour = metric, group = metric)) +
  geom_line() +
  geom_point(size = 1.5, shape = 19, alpha = 0.5) +
  #scale_y_continuous(limits=c(0,20)) +
  scale_x_discrete(limits = month_levels) +
  xlab("# months prior to attrition event") +
  ylab("Normalized median (% of overall metric maximum)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=9), 
        axis.title=element_text(size=9)) + 
  theme(legend.title=element_text(size=8), 
        legend.text=element_text(size=8))

g_att_zoom <- ggplot(attrition_eval, aes(x = months_prior, y = normalized_median, colour = metric, group = metric)) +
  geom_line() +
  geom_point(size = 1.5, shape = 19, alpha = 0.5) +
  scale_y_continuous(limits=c(0,18)) +
  scale_x_discrete(limits = month_levels) +
  xlab("# months prior to attrition event") +
  ylab("Normalized median (% of overall metric maximum)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=9), 
        axis.title=element_text(size=9)) + 
  theme(legend.title=element_text(size=8), 
        legend.text=element_text(size=8))

pdf("att_eval.pdf", width=8, height=4)
grid.arrange(g_att_eval)
dev.off()

pdf("att_remaining.pdf")
grid.arrange(g_att_overall, g_att_zoom, nrow = 2, ncol=1)
dev.off()