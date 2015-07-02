#This code is to perform the hypothesis testing using in the ITID journal article for 
#crs-remind. We are especially interested to see if intra-group or intra-user consistency 
#for time_using_cc looks different for crs-remind compared to the journal dataset

#First import all_monthly from the journal article folder on rdayalu/DB

training_typical <- read.csv(file = "all_monthly.csv")
training_typical$calendar_month <- as.Date(training_typical$calendar_month, "%m/%d/%Y")
training_typical <- filter(training_typical, domain == "crs-remind")

#Indicators to evaluate
indicators <- c("nvisits", "active_day_percent", "nforms", 
                "median_visit_duration", "median_visits_per_day", 
                "time_using_cc", "ninteractions", "ncases_registered", 
                "register_followup", "case_register_followup_rate", 
                "ncases_touched", "nunique_followups", "sample_increase", 
                "sample_decrease")

#------------------------------------------------------------------------#
#Code for Test 1
#------------------------------------------------------------------------#

#Must only include rows with previous_month_active == T. Exclude F & NA 
training_consec <- filter(training_typical, previous_month_active == T)
training_consec$concat <- paste(training_consec$user_pk, training_consec$calendar_month, 
                                sep = "_") 

#Exclude domain calendar_months with nusers < 5 for that domain
#Use this dataset only for test 1a because we don't want to calculate medians 
#for <= 5 users/month
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
source(file.path("analysis_scripts","rdayalu","test_1a_journal.R", fsep = .Platform$file.sep))

test_1a <- 
  c(cor(training_consec$med_nvisits_1a, training_consec$diff_nvisits, use = "complete.obs"),
    cor(training_consec$med_active_day_percent_1a, training_consec$diff_active_day_percent, use = "complete.obs"),
    cor(training_consec$med_nforms_1a, training_consec$diff_nforms, use = "complete.obs"),
    cor(training_consec$med_median_visit_duration_1a, training_consec$diff_median_visit_duration, use = "complete.obs"),
    cor(training_consec$med_median_visits_per_day_1a, training_consec$diff_median_visits_per_day, use = "complete.obs"),
    cor(training_consec$med_time_using_cc_1a, training_consec$diff_time_using_cc, use = "complete.obs"),
    cor(training_consec$med_ninteractions_1a, training_consec$diff_ninteractions, use = "complete.obs"),
    cor(training_consec$med_ncases_registered_1a, training_consec$diff_ncases_registered, use = "complete.obs"),
    cor(training_consec$med_register_followup_1a, training_consec$diff_register_followup, use = "complete.obs"),
    cor(training_consec$med_case_register_followup_rate_1a, training_consec$diff_case_register_followup_rate, use = "complete.obs"),
    cor(training_consec$med_ncases_touched_1a, training_consec$diff_ncases_touched, use = "complete.obs"),
    cor(training_consec$med_nunique_followups_1a, training_consec$diff_nunique_followups, use = "complete.obs"),
    cor(training_consec$med_sample_increase_1a, training_consec$diff_sample_increase, use = "complete.obs"),
    cor(training_consec$med_sample_decrease_1a, training_consec$diff_sample_decrease, use = "complete.obs"))
names(test_1a) <- indicators

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
training_typical$prev_ninteractions <- training_typical$ninteractions - training_typical$diff_ninteractions
training_typical$prev_ncases_registered <- training_typical$ncases_registered - training_typical$diff_ncases_registered
training_typical$prev_register_followup <- training_typical$register_followup - training_typical$diff_register_followup
training_typical$prev_case_register_followup_rate <- training_typical$case_register_followup_rate - training_typical$diff_case_register_followup_rate
training_typical$prev_ncases_touched <- training_typical$ncases_touched - training_typical$diff_ncases_touched
training_typical$prev_nunique_followups <- training_typical$nunique_followups - training_typical$diff_nunique_followups
training_typical$prev_sample_increase <- training_typical$sample_increase - training_typical$diff_sample_increase
training_typical$prev_sample_decrease <- training_typical$sample_decrease - training_typical$diff_sample_decrease

#Must only include rows with previous_month_active == T. Exclude F & NA 
training_typical <- training_typical[training_typical$previous_month_active == T,]

test_2a <- 
  c(cor(training_typical$prev_nvisits, training_typical$nvisits, use = "complete.obs"),
    cor(training_typical$prev_active_day_percent, training_typical$active_day_percent, use = "complete.obs"),
    cor(training_typical$prev_nforms, training_typical$nforms, use = "complete.obs"),
    cor(training_typical$prev_median_visit_duration, training_typical$median_visit_duration, use = "complete.obs"),
    cor(training_typical$prev_median_visits_per_day, training_typical$median_visits_per_day, use = "complete.obs"),
    cor(training_typical$prev_time_using_cc, training_typical$time_using_cc, use = "complete.obs"),
    cor(training_typical$prev_ninteractions, training_typical$ninteractions, use = "complete.obs"),
    cor(training_typical$prev_ncases_registered, training_typical$ncases_registered, use = "complete.obs"),
    cor(training_typical$prev_register_followup, training_typical$register_followup, use = "complete.obs"),
    cor(training_typical$prev_case_register_followup_rate, training_typical$case_register_followup_rate, use = "complete.obs"),
    cor(training_typical$prev_ncases_touched, training_typical$ncases_touched, use = "complete.obs"),
    cor(training_typical$prev_nunique_followups, training_typical$nunique_followups, use = "complete.obs"), 
    cor(training_typical$prev_sample_increase, training_typical$sample_increase, use = "complete.obs"), 
    cor(training_typical$prev_sample_decrease, training_typical$sample_decrease, use = "complete.obs"))
names(test_2a) <- indicators

#Measure intra-user consistency only between Oct/Nov 2014
test <- filter(training_typical, calendar_month == "2014-11-01")
cor(test$prev_time_using_cc, test$time_using_cc, use = "complete.obs")

#Convert time_using_cc to seconds to match Mengji's calculations
test$prev_time_using_cc_sec = as.integer(test$prev_time_using_cc*60) 
test$time_using_cc_sec = as.integer(test$time_using_cc*60) 
cor(test$prev_time_using_cc_sec, test$time_using_cc_sec, use = "complete.obs")
test2 <- select(test, user_pk, user_id, calendar_month, time_using_cc, prev_time_using_cc, diff_time_using_cc, 
                time_using_cc_sec, prev_time_using_cc_sec)

#Import Mengji's data
duration <- read.csv(file = "duration.csv")
duration$calendar_month <- as.Date(duration$calendar_month)
names(duration)[names(duration) == "userID"] = "user_id"
summary(test2$user_id %in% duration$user_id)

#Keep only users that are in the journal dataset
#Bring cum_secs to the same row for a given user for Oct/Nov  
duration_journal <- duration[duration$user_id %in% test2$user_id,]
duration_oct <- filter(duration_journal, calendar_month == "2014-10-01")
duration_nov <- filter(duration_journal, calendar_month == "2014-11-01")
names(duration_oct)[names(duration_oct) == "cum_secs"] = "total_sec_oct"
names(duration_nov)[names(duration_nov) == "cum_secs"] = "total_sec_nov"
duration_nov <- select(duration_nov, user_id, total_sec_nov)
duration_journal <- merge(duration_oct, duration_nov, by = "user_id", all.x = T) 
cor(duration_journal$total_sec_oct, duration_journal$total_sec_nov, use = "complete.obs")
duration_journal <- select(duration_journal, user_id, total_sec_oct, total_sec_nov)

#Merge these columns to the crs oct/nov journal set
#Flag the Mengji's values that are different from DP by more than a range of 1
test2 <- merge(test2, duration_journal, by = "user_id", all.x = T)
test2$oct_same <- test2$prev_time_using_cc_sec - test2$total_sec_oct == 0 | 
  test2$prev_time_using_cc_sec - test2$total_sec_oct == -1 | 
  test2$prev_time_using_cc_sec - test2$total_sec_oct == 1
test2$nov_same <- test2$time_using_cc_sec - test2$total_sec_nov == 0 | 
  test2$time_using_cc_sec - test2$total_sec_nov == -1 | 
  test2$time_using_cc_sec - test2$total_sec_nov == 1

#Filter test2 to only include users that are matched with Mengji's values in both months
test3 <- filter(test2, oct_same == T & nov_same == T)
cor(test3$prev_time_using_cc_sec, test3$time_using_cc_sec, use = "complete.obs")


#Checking correlation in Mengji's full dataset
duration_oct <- filter(duration, calendar_month == "2014-10-01")
duration_nov <- filter(duration, calendar_month == "2014-11-01")
names(duration_oct)[names(duration_oct) == "cum_secs"] = "total_sec_oct"
names(duration_nov)[names(duration_nov) == "cum_secs"] = "total_sec_nov"
duration_nov <- select(duration_nov, user_id, total_sec_nov)
duration_journal <- merge(duration_oct, duration_nov, by = "user_id", all.x = T) 
cor(duration_journal$total_sec_oct, duration_journal$total_sec_nov, use = "complete.obs")

