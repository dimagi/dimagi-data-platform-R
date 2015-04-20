#Visuals and calculations for Blog #4

#Get required libraries
library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate)
library(ggplot2)
library(gridExtra)

#Import full journal dataset
training_typical <- read.csv(file = "all_monthly.csv")
training_typical$calendar_month <- as.Date(training_typical$calendar_month, "%m/%d/%Y")
training_typical$month_abbr <- month(training_typical$calendar_month, label = T, abbr = T)
fullset <- training_typical 

#------------------------------------------------------------------------#
# Correlation matrix for seven indicators that we are using in Blog #4 
#------------------------------------------------------------------------#

library(corrplot)

# Make datset with only the following variable of interest
indicators_short = c("nforms", "nvisits", "ncases_touched", "active_day_percent", "time_using_cc", 
                     "ncases_registered", "nunique_followups")
corr_set <- training_typical
corr_set <- corr_set[,names(corr_set) %in% indicators_short]
corr_set <- corr_set[,c("nforms", "nvisits", "ncases_touched", "active_day_percent", "time_using_cc", 
                        "ncases_registered", "nunique_followups")]
names(corr_set) <- c("# forms", "# visits", "# cases", "% days", "cumulative mins", 
                     "# cases registered", "# cases followed-up")

#Generate numerical correlation matrix
num_corr <- cor(corr_set, use="complete.obs")
num_corr <- round(num_corr, digits = 2)

#Generate visual correlation plot
corrplot(num_corr, tl.srt=35, tl.cex = 0.8, tl.col = "black",  
         diag=F, type={"upper"}, order = "AOE")
cor_plot <- corrplot(num_corr, tl.srt=45, tl.cex = 0.6, 
                     diag=F, order = "AOE")

#------------------------------------------------------------------------#
# M & E: Seasons
#------------------------------------------------------------------------#

#Seasonal/monthly activity: use fullset
#We are going to filter India to make seasonal/holiday comparisons more applicable
test <- filter(fullset, country_final == "India")

#normalized metrics
season <- test %>% group_by(month_abbr) %>% 
  summarise(med_nvisits = median(nvisits),
            med_forms = median(nforms), 
            med_time_using_cc = median(time_using_cc),
            med_active_days = median(active_day_percent),
            med_ncases_registered = median(ncases_registered),
            med_ncases_touched = median(ncases_touched),
            med_nunique_followups = median(nunique_followups))
season <- data.frame(rep(season$month_abbr,7), 
                     c(season$med_nvisits, season$med_forms, season$med_time_using_cc,
                       season$med_active_days, season$med_ncases_registered, season$med_ncases_touched, 
                       season$med_nunique_followups))
season$indicator <- c(rep("# visits", 12), rep("# forms", 12), rep("cumulative minutes", 12), 
                      rep("% days", 12), rep("# cases registered", 12), rep("# cases", 12), 
                      rep("# cases followed-up", 12))
names(season) <- c("month", "median_metric", "metric")
season$overall_max <- c(rep(max(test$nvisits), 12), rep(max(test$nforms), 12), 
                        rep(max(test$time_using_cc), 12), rep(max(test$active_day_percent), 12), 
                        rep(max(test$ncases_registered), 12), rep(max(test$ncases_touched), 12), 
                        rep(max(test$nunique_followups), 12))
season$normalized_median <- (season$median_metric/season$overall_max)*100

#Do CHWs have different levels of activity by calendar month or season?
#normalized graph overall
g_season_normal_overall <- ggplot(season, aes(x=month, y=normalized_median, group = metric, colour = metric)) +
  geom_line() +
  #scale_y_continuous(limits=c(0,20)) +
  geom_point(size = 1.5, shape = 19, alpha = 0.5) +
  xlab("Month") +
  ylab("Normalized median (% of overall metric maximum)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=9), 
        axis.title=element_text(size=9)) + 
  theme(legend.title=element_text(size=8), 
        legend.text=element_text(size=8))


pdf("foo.pdf", width=8, height=4)
grid.arrange(g_season_normal_overall)
dev.off()

#------------------------------------------------------------------------#
# M & E: Attrition
#------------------------------------------------------------------------#

#Code from journal_attrition.R
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
leadup_indicators <- c(rep("# forms", 4), rep("# visits", 4), rep("# cases", 4), rep("% active days", 4), 
                       rep("cumulative minutes", 4), rep("# cases registered", 4), rep("# cases followed-up", 4))
leadup_data <- data.frame(cbind(rep(c(1:4), 7), leadup_indicators, leadup_values))
names(leadup_data) <- c("months_prior", "metric", "metric_values")
leadup_data$metric_values <- as.numeric(levels(leadup_data$metric_values))[leadup_data$metric_values]
leadup_data$months_prior <- as.factor(leadup_data$months_prior)
month_levels <- rev(levels(leadup_data$months_prior))


# Graph 7 normalized metrics
attrition_eval <- leadup_data
attrition_eval$overall_max <- c(rep(max(fullset$nforms), 4), rep(max(fullset$nvisits), 4), 
                                rep(max(fullset$ncases_touched), 4), rep(max(fullset$active_day_percent), 4), 
                                rep(max(fullset$time_using_cc), 4), rep(max(fullset$ncases_registered), 4), 
                                rep(max(fullset$nunique_followups), 4))
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


pdf("g_att_overall.pdf", width=8, height=4)
grid.arrange(g_att_overall)
dev.off()