all_monthly <- read.csv(file = "blog_data.csv")
library(dplyr)
library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate)
library(ggplot2)
library(scales) #to customize ggplot axis labeling
library(gridExtra) #graphing plots in columns/rows for ggplot
library(RColorBrewer) #Color palettes
source('s_dplyr.R')


#Blog set has 12,554 unique users
#Exclude any users who logged > 100 visits in any month 
all_monthly$visits_ge_100 <- all_monthly$nvisits > 100
user_ge_100 <- all_monthly %.%
  group_by(user_id) %.%
  summarise(ge_100 = sum(visits_ge_100))
user_le_100 <- filter(user_ge_100, ge_100 == 0)
#696 users have only <= 100 visits per month
training_typical <- 
  all_monthly[all_monthly$user_id %in% user_le_100$user_id, ]

#Exclude users with < 4 months on CC
month_count <- training_typical %.%
  group_by(domain, user_id) %.%
  summarise(months_on_cc = length(unique(calendar_month)))
month_count <- filter(month_count, months_on_cc >= 4)
training_typical <- 
  training_typical[training_typical$user_id %in% month_count$user_id, ]

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
#Also want differences between indicators for each user from one month to the next
#Differences in indicators will be used for tests 1a/b
training_typical <- arrange(training_typical, user_pk, calendar_month)
df <- data.table(training_typical)
setkey(df,user_pk)
#df[,diff_days:=c(NA,diff(calendar_month)),by=user_pk]
df[,diff_nvisits:=c(NA,diff(nvisits)),by=user_pk]
df[,diff_active_day_percent:=c(NA,diff(active_day_percent)),by=user_pk]
df[,diff_nforms:=c(NA,diff(nforms)),by=user_pk]
df[,diff_median_visit_duration:=c(NA,diff(median_visit_duration)),by=user_pk]
df[,diff_median_visits_per_day:=c(NA,diff(median_visits_per_day)),by=user_pk]
df[,diff_time_using_cc:=c(NA,diff(time_using_cc)),by=user_pk]
#df[,diff_ninteractions:=c(NA,diff(ninteractions)),by=user_pk]
df[,diff_ncases_registered:=c(NA,diff(ncases_registered)),by=user_pk]
df[,diff_register_followup:=c(NA,diff(register_followup)),by=user_pk]
df[,diff_case_register_followup_rate:=c(NA,diff(case_register_followup_rate)),by=user_pk]
df[,diff_ncases_touched:=c(NA,diff(ncases_touched)),by=user_pk]
df[,diff_nunique_followups:=c(NA,diff(nunique_followups)),by=user_pk]
df[,diff_audio_plays:=c(NA,diff(audio_plays)),by=user_pk]
df[,diff_network_warnings:=c(NA,diff(network_warnings)),by=user_pk]
df[,diff_num_user_pk:=c(NA,diff(num_user_pk)),by=user_pk]
df[,diff_domain_numeric:=c(NA,diff(domain_numeric)),by=user_pk]
df[,diff_sample_undefined:=c(NA,diff(sample_undefined)),by=user_pk]
df[,diff_sample_normal:=c(NA,diff(sample_normal)),by=user_pk]
df[,diff_sample_percentile:=c(NA,diff(sample_percentile)),by=user_pk]
df[,diff_sample_increase:=c(NA,diff(sample_increase)),by=user_pk]
df[,diff_sample_decrease:=c(NA,diff(sample_decrease)),by=user_pk]
training_typical <- as.data.frame(df)
#training_typical$previous_month_active <- training_typical$diff_days <= 31
users <- unique(training_typical$user_pk)

#next_month_active <- c()
#for (i in users) {
#  single_user <- training_typical[training_typical$user_pk == i,]
#  next_active <- c()
#  next_active <- append(single_user$previous_month_active[-1], F)
#  next_month_active <- append(next_month_active, next_active)
#}
#training_typical$next_month_active <- next_month_active
#If calendar_month = 10/1/14 then next_month_active = NA
#because we don't know if the user will be active in the following month
#is.na(training_typical$next_month_active) <- training_typical$calendar_month == "2014-10-01"

#------------------------------------------------------------------------#
#General plots
#------------------------------------------------------------------------#
#Number of users by calendar month
n_user <- training_typical %.% 
  group_by(calendar_month) %.% 
  summarise(n_users = length(unique(user_id)))

g <- ggplot(n_user, aes(x=calendar_month, y=n_users)) +
  geom_point(size = 3, shape = 19, alpha = 0.5, colour = "darkblue", 
             fill = "lightblue") +
  geom_line(colour = "darkblue") + 
  scale_size_area() + 
  xlab("Calendar month") +
  ylab("# unique users/month") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                 face="bold")) + 
  ggtitle("Number of users by calendar month") +
  theme(plot.title = element_text(size=14, face="bold"))

pdf("plots.pdf")
plot(g)
dev.off()

#------------------------------------------------------------------------#
#Code for Test 1
#------------------------------------------------------------------------#
# % difference in indicators for each user for consectutive months
# This isn't for truly consecutive months, so later on, 
# we will only use rows with previous_month_active == T
#This will be used for test 1b
source(file.path("analysis_scripts","rdayalu","test_1b.R", fsep = .Platform$file.sep))

#Must only include rows with previous_month_active == T. Exclude F & NA 
training_consec <- filter(training_typical, previous_month_active == T)
training_consec$concat <- paste(training_consec$user_pk, training_consec$calendar_month, 
                                sep = "_") 


#Exclude domain calendar_months with nusers < 5 for that domain
#Use this dataset only for test 1a/1b
nusers <- training_consec %>% 
  group_by(domain, calendar_month) %>% 
  summarise(nusers = length(unique(user_pk)))
nusers <- filter(nusers, nusers >= 5)
nusers$concat <- paste(nusers$domain, nusers$calendar_month, sep = "_")
training_consec <- 
  training_consec[paste(training_consec$domain, training_consec$calendar_month, sep = "_") %in% 
                     nusers$concat, ]

#Domain median ABSOLUTE change per user per calendar month, 
#excluding each user from the domain median for that user's row
#This is used for test 1a
source(file.path("analysis_scripts","rdayalu","test_1a.R", fsep = .Platform$file.sep))

#Domain median PERCENTAGE change per user per calendar month, 
#excluding each user from the domain median for that user's row
source(file.path("analysis_scripts","rdayalu","test_1b_2.R", fsep = .Platform$file.sep))

names(training_consec)
diff_indicator <- names(training_consec[41:61])
per_diff_indicator <- names(training_consec[64:84])

#Dataset for Mengji with test 1a/1b x,y values for each user for each month
#mengji <- select(training_consec, user_id, user_pk, domain, calendar_month, diff_ncases_touched, 
#                 med_ncases_touched_1a, per_diff_ncases_touched, med_ncases_touched_1b)
#names(mengji)[names(mengji) == "diff_ncases_touched"] = "ntouched_test1a_x"
#names(mengji)[names(mengji) == "med_ncases_touched_1a"] = "ntouched_test1a_y"
#names(mengji)[names(mengji) == "per_diff_ncases_touched"] = "ntouched_test1b_x"
#names(mengji)[names(mengji) == "med_ncases_touched_1b"] = "ntouched_test1b_y"

test_1a <- 
  c(cor(training_consec$med_nvisits_1a, training_consec$diff_nvisits, use = "complete.obs"),
    cor(training_consec$med_active_day_percent_1a, training_consec$diff_active_day_percent, use = "complete.obs"),
    cor(training_consec$med_nforms_1a, training_consec$diff_nforms, use = "complete.obs"),
    cor(training_consec$med_median_visit_duration_1a, training_consec$diff_median_visit_duration, use = "complete.obs"),
    cor(training_consec$med_median_visits_per_day_1a, training_consec$diff_median_visits_per_day, use = "complete.obs"),
    cor(training_consec$med_time_using_cc_1a, training_consec$diff_time_using_cc, use = "complete.obs"),
    #cor(training_consec$med_ninteractions_1a, training_consec$diff_ninteractions, use = "complete.obs"),
    cor(training_consec$med_ncases_registered_1a, training_consec$diff_ncases_registered, use = "complete.obs"),
    cor(training_consec$med_register_followup_1a, training_consec$diff_register_followup, use = "complete.obs"),
    cor(training_consec$med_case_register_followup_rate_1a, training_consec$diff_case_register_followup_rate, use = "complete.obs"),
    cor(training_consec$med_ncases_touched_1a, training_consec$diff_ncases_touched, use = "complete.obs"),
    cor(training_consec$med_nunique_followups_1a, training_consec$diff_nunique_followups, use = "complete.obs"),
    cor(training_consec$med_audio_plays_1a, training_consec$diff_audio_plays, use = "complete.obs"),
    cor(training_consec$med_network_warnings_1a, training_consec$diff_network_warnings, use = "complete.obs"),
    cor(training_consec$med_num_user_pk_1a, training_consec$diff_num_user_pk, use = "complete.obs"),
    cor(training_consec$med_domain_numeric_1a, training_consec$diff_domain_numeric, use = "complete.obs"),
    cor(training_consec$med_sample_undefined_1a, training_consec$diff_sample_undefined, use = "complete.obs"),
    cor(training_consec$med_sample_normal_1a, training_consec$diff_sample_normal, use = "complete.obs"),
    cor(training_consec$med_sample_percentile_1a, training_consec$diff_sample_percentile, use = "complete.obs"),
    cor(training_consec$med_sample_increase_1a, training_consec$diff_sample_increase, use = "complete.obs"),
    cor(training_consec$med_sample_decrease_1a, training_consec$diff_sample_decrease, use = "complete.obs"))
names(test_1a) <- indicators

test_1b <- 
  c(cor(training_consec$med_nvisits_1b, training_consec$per_diff_nvisits, use = "complete.obs"),
    cor(training_consec$med_active_day_percent_1b, training_consec$per_diff_active_day_percent, use = "complete.obs"),
    cor(training_consec$med_nforms_1b, training_consec$per_diff_nforms, use = "complete.obs"),
    cor(training_consec$med_median_visit_duration_1b, training_consec$per_diff_median_visit_duration, use = "complete.obs"),
    cor(training_consec$med_median_visits_per_day_1b, training_consec$per_diff_median_visits_per_day, use = "complete.obs"),
    cor(training_consec$med_time_using_cc_1b, training_consec$per_diff_time_using_cc, use = "complete.obs"),
    #cor(training_consec$med_ninteractions_1b, training_consec$per_diff_ninteractions, use = "complete.obs"),
    cor(training_consec$med_ncases_registered_1b, training_consec$per_diff_ncases_registered, use = "complete.obs"),
    cor(training_consec$med_register_followup_1b, training_consec$per_diff_register_followup, use = "complete.obs"),
    cor(training_consec$med_case_register_followup_rate_1b, training_consec$per_diff_case_register_followup_rate, use = "complete.obs"),
    cor(training_consec$med_ncases_touched_1b, training_consec$per_diff_ncases_touched, use = "complete.obs"),
    cor(training_consec$med_nunique_followups_1b, training_consec$per_diff_nunique_followups, use = "complete.obs"),
    cor(training_consec$med_audio_plays_1b, training_consec$per_diff_audio_plays, use = "complete.obs"),
    cor(training_consec$med_network_warnings_1b, training_consec$per_diff_network_warnings, use = "complete.obs"),
    cor(training_consec$med_num_user_pk_1b, training_consec$per_diff_num_user_pk, use = "complete.obs"),
    cor(training_consec$med_domain_numeric_1b, training_consec$per_diff_domain_numeric, use = "complete.obs"),
    cor(training_consec$med_sample_undefined_1b, training_consec$per_diff_sample_undefined, use = "complete.obs"),
    cor(training_consec$med_sample_normal_1b, training_consec$per_diff_sample_normal, use = "complete.obs"),
    cor(training_consec$med_sample_percentile_1b, training_consec$per_diff_sample_percentile, use = "complete.obs"),
    cor(training_consec$med_sample_increase_1b, training_consec$per_diff_sample_increase, use = "complete.obs"),
    cor(training_consec$med_sample_decrease_1b, training_consec$per_diff_sample_decrease, use = "complete.obs"))
names(test_1b) <- indicators


#Pairwise plots of absolute and % changes for individual FLWs by domain medians
g <- ggplot(tula_consec, aes(x=med_domain_abs_change, y=diff_nvisits)) +
  geom_point(shape=1) +
  #scale_y_continuous(limits=c(-100,100)) +
  geom_smooth(method=lm)


#------------------------------------------------------------------------#
#Code for Test 2
#------------------------------------------------------------------------#

#Previous month's indicator value
training_typical$prev_nvisits <- training_typical$nvisits - training_typical$diff_nvisits
training_typical$prev_active_day_percent <- training_typical$active_day_percent - training_typical$diff_active_day_percent
training_typical$prev_nforms<- training_typical$nforms - training_typical$diff_nforms
training_typical$prev_median_visit_duration <- training_typical$median_visit_duration - training_typical$diff_median_visit_duration
training_typical$prev_median_visits_per_day <- training_typical$median_visits_per_day - training_typical$diff_median_visits_per_day
training_typical$prev_time_using_cc <- training_typical$time_using_cc - training_typical$diff_time_using_cc
#training_typical$prev_ninteractions <- training_typical$ninteractions - training_typical$diff_ninteractions
training_typical$prev_ncases_registered <- training_typical$ncases_registered - training_typical$diff_ncases_registered
training_typical$prev_register_followup <- training_typical$register_followup - training_typical$diff_register_followup
training_typical$prev_case_register_followup_rate <- training_typical$case_register_followup_rate - training_typical$diff_case_register_followup_rate
training_typical$prev_ncases_touched <- training_typical$ncases_touched - training_typical$diff_ncases_touched
training_typical$prev_nunique_followups <- training_typical$nunique_followups - training_typical$diff_nunique_followups
training_typical$prev_audio_plays <- training_typical$audio_plays - training_typical$diff_audio_plays
training_typical$prev_network_warnings <- training_typical$network_warnings- training_typical$diff_network_warnings
training_typical$prev_num_user_pk <- training_typical$num_user_pk - training_typical$diff_num_user_pk
training_typical$prev_domain_numeric <- training_typical$domain_numeric - training_typical$diff_domain_numeric
training_typical$prev_sample_undefined <- training_typical$sample_undefined - training_typical$diff_sample_undefined
training_typical$prev_sample_normal <- training_typical$sample_normal - training_typical$diff_sample_normal
training_typical$prev_sample_percentile <- training_typical$sample_percentile - training_typical$diff_sample_percentile
training_typical$prev_sample_increase <- training_typical$sample_increase - training_typical$diff_sample_increase
training_typical$prev_sample_decrease <- training_typical$sample_decrease - training_typical$diff_sample_decrease

test2_data <- filter(training_typical, previous_month_active == T)

#Dataset for Mengji with test 2a x,y values for each user for each month
#mengji2 <- select(test2_data, user_id, user_pk, domain, calendar_month, ncases_touched, 
#                  prev_ncases_touched)
#names(mengji2)[names(mengji2) == "ncases_touched"] = "ntouched_test2a_x"
#names(mengji2)[names(mengji2) == "prev_ncases_touched"] = "ntouched_test2a_y"


test_2a <- 
  c(cor(training_typical$prev_nvisits, training_typical$nvisits, use = "complete.obs"),
    cor(training_typical$prev_active_day_percent, training_typical$active_day_percent, use = "complete.obs"),
    cor(training_typical$prev_nforms, training_typical$nforms, use = "complete.obs"),
    cor(training_typical$prev_median_visit_duration, training_typical$median_visit_duration, use = "complete.obs"),
    cor(training_typical$prev_median_visits_per_day, training_typical$median_visits_per_day, use = "complete.obs"),
    cor(training_typical$prev_time_using_cc, training_typical$time_using_cc, use = "complete.obs"),
    #cor(training_typical$prev_ninteractions, training_typical$ninteractions, use = "complete.obs"),
    cor(training_typical$prev_ncases_registered, training_typical$ncases_registered, use = "complete.obs"),
    cor(training_typical$prev_register_followup, training_typical$register_followup, use = "complete.obs"),
    cor(training_typical$prev_case_register_followup_rate, training_typical$case_register_followup_rate, use = "complete.obs"),
    cor(training_typical$prev_ncases_touched, training_typical$ncases_touched, use = "complete.obs"),
    cor(training_typical$prev_nunique_followups, training_typical$nunique_followups, use = "complete.obs"),
    cor(training_typical$prev_audio_plays, training_typical$audio_plays, use = "complete.obs"),
    cor(training_typical$prev_network_warnings, training_typical$network_warnings, use = "complete.obs"),
    cor(training_typical$prev_num_user_pk, training_typical$num_user_pk, use = "complete.obs"),
    cor(training_typical$prev_domain_numeric, training_typical$domain_numeric, use = "complete.obs"),
    cor(training_typical$prev_sample_undefined, training_typical$sample_undefined, use = "complete.obs"),
    cor(training_typical$prev_sample_normal, training_typical$sample_normal, use = "complete.obs"),
    cor(training_typical$prev_sample_percentile, training_typical$sample_percentile, use = "complete.obs"),
    cor(training_typical$prev_sample_increase, training_typical$sample_increase, use = "complete.obs"),
    cor(training_typical$prev_sample_decrease, training_typical$sample_decrease, use = "complete.obs"))
names(test_2a) <- indicators

g <- ggplot(raw_percentile, aes(x=sample_percentile, y=prev_percentile_sample)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)

#------------------------------------------------------------------------#
#Code for Test 4
#------------------------------------------------------------------------#

#Create function to append attrition list
lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

#Extract users with at least one attrition event
users <- unique((filter(training_typical, next_month_active == F))$user_pk)

#This is the test_4b code
source(file.path("analysis_scripts","rdayalu","test_4b.R", fsep = .Platform$file.sep))
names(test_4b) <- indicators

test <- data.frame(cbind(test_1a, test_1b, test_2a, test_4b))
write.csv(test, file = "blog_set_results.csv")
#------------------------------------------------------------------------#
#Other random code
#------------------------------------------------------------------------#

#Overall attrition_data: Median of each month column
months_median <- apply(attrition_data[,1:23], 2, function(x) median(x, na.rm = T))
months_median <- data.frame(months_median)
months_median$month_before_attrition <- c(1:nrow(months_median))
months_median$months_mad <- apply(attrition_data[,1:23], 2, function(x) mad(x, na.rm = T)) 

#Plot
g <- ggplot(months_median, aes(x=month_before_attrition, y=months_median, group = 1)) + 
  geom_line(colour="blue", size=1.0) + 
  geom_errorbar(aes(ymin=months_median-months_mad, ymax=months_median+months_mad), 
                width=.3, colour = "black")

#Relative attrition_data: Median of each relative month column
months_median <- apply(attrition_subset[,25:29], 2, function(x) median(x, na.rm = T))
months_median <- data.frame(months_median)
months_median$month_before_attrition <- c(1:nrow(months_median))
months_median$months_mad <- apply(attrition_subset[,25:29], 2, function(x) mad(x, na.rm = T)) 

#Plot
g <- ggplot(months_median, aes(x=month_before_attrition, y=months_median, group = 1)) + 
  geom_line(colour="blue", size=1.0) + 
  geom_errorbar(aes(ymin=months_median-months_mad, ymax=months_median+months_mad), 
                width=.3, colour = "black") +
  xlab("month_before_attrition") +
  ylab("nvisits relative to month 5 (%)")

#Subset of attrition_data: Median of each month column
months_median <- apply(attrition_subset[,1:5], 2, function(x) median(x, na.rm = T))
months_median <- data.frame(months_median)
months_median$month_before_attrition <- c(1:nrow(months_median))
months_median$months_mad <- apply(attrition_subset[,1:5], 2, function(x) mad(x, na.rm = T)) 

#Plot
g <- ggplot(months_median, aes(x=month_before_attrition, y=months_median, group = 1)) + 
  geom_line(colour="blue", size=1.0) + 
  geom_errorbar(aes(ymin=months_median-months_mad, ymax=months_median+months_mad), 
                width=.3, colour = "black")


#Test 4A: slope of line (lm) for absolute months 1-4 for each row
#attrition_subset$slope_abs <- apply(attrition_subset[,1:4], 1, function(x) 
#  lm(x~c(1:4))$coefficients[[2]])

#Rehape all_monthly from long format to wide
#This creates only one row per user with columns for each calendar month
users_long <- select(tula_typical, domain, user_id, nvisits, calendar_month)
users_wide <- reshape(users_long,
                      timevar = "calendar_month",
                      idvar = c("domain", "user_id"),
                      direction = "wide")

g <- ggplot(data=tula_typical, aes(x=calendar_month, y=nvisits, group = user_id)) + 
  geom_line(colour="grey", size=1.0)

users_long <- select(raw_percentile, domain, user_id, percentile, calendar_month)
users_wide <- reshape(users_long,
                      timevar = "calendar_month",
                      idvar = c("domain", "user_id"),
                      direction = "wide")

# Number of users by calendar_month
users_month_tula <- tula_typical %.% 
  group_by(domain, calendar_month) %.% 
  summarise(nusers = length(unique(user_id)))

g <- ggplot(data=users_month_tula, aes(x=calendar_month, y=nusers)) + 
  geom_line(colour="black", size=1.0) + 
  geom_point(colour="red", size=3, shape=21, fill="red")

users_wide <- reshape(users_month_tula,
                      timevar = "calendar_month",
                      idvar = c("domain"),
                      direction = "wide")

users_wide <- users_wide[,order(names(users_wide))]
write.csv(users_wide, file = "tula_nusers_wide.csv")
