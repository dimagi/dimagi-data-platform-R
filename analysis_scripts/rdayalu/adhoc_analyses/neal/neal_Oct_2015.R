#mHealth conference blog update for Neal based on the document here:
#https://docs.google.com/document/d/1dSYfZC-Pf_7ogEsNeopBTVRpepyM9c6ivh4r00PP54I/edit
#We need to do both of the following:
#(1) run the correlation matrix on all 7 metrics on all data through June 2015
#(2) run the intra-user consistency analysis on all 7 metrics on all data through June 2015

#Run blog data set code from 1/1/10 through 6/1/15
#For some reason that final number of rows for June 2015 is very low. Need to figure out why later on
#Meanwhile, will use this new blog dataset for now. 
#After creating new blog dataset, do thr following.

#Create active_day_percent and convert active_months to numeric
all_monthly$active_months <- as.numeric(all_monthly$active_months)
all_monthly$total_days_month <- as.numeric(days_in_month(all_monthly$calendar_month))
all_monthly$active_day_percent <- round((all_monthly$active_days/all_monthly$total_days_month)*100, 
                                        digits = 2)

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
#Also want differences between indicators for each user from one month to the next
#Differences in indicators will be used for test 1a
all_monthly <- arrange(all_monthly, domain_user)
df <- data.table(all_monthly)
setkey(df,domain_user)
df[,diff_active_day_percent:=c(NA,diff(active_day_percent)),by=domain_user]
df[,diff_nforms:=c(NA,diff(nforms)),by=domain_user]
df[,diff_nvisits:=c(NA,diff(nvisits)),by=domain_user]
df[,diff_nunique_followups:=c(NA,diff(nunique_followups)),by=domain_user]
df[,diff_ncases_touched:=c(NA,diff(ncases_touched)),by=domain_user]
df[,diff_ncases_registered:=c(NA,diff(ncases_registered)),by=domain_user]
df[,diff_time_using_cc:=c(NA,diff(time_using_cc)),by=domain_user]
all_monthly <- as.data.frame(df)

#Indicators to evaluate
indicators <- c("active_day_percent", "nforms", "nvisits", 
                "nunique_followups", "ncases_touched", "ncases_registered",
                "time_using_cc")

#In the blog, I only kept users who have been active for 
#at least 6 mos and I only counted their rows from month 6 onwards 
#See this document for further details:
#https://docs.google.com/a/dimagi.com/spreadsheets/d/1weMI03KGQPffWHM3y2AR1VbSVCZIbJEEXiFd_0jRL7A/edit#gid=0
#Exclude rows before the 6th month on CC AND users with < 6 active months on CC
#We will calculated the intra-user consistency and create the correlation matrix 
#for this subset of users as well as for all users all months
all_monthly2 <- filter(all_monthly, month_index >= 6)
all_monthly2 <- filter(all_monthly2, active_months >= 6)

#write.csv(all_monthly, file = "all_monthly.csv")

training_typical <- all_monthly
#training_typical <- all_monthly2

#------------------------------------------------------------------------#
#Code for intra-user consistency
#------------------------------------------------------------------------#

#Previous month's indicator value
training_typical$prev_active_day_percent <- training_typical$active_day_percent - training_typical$diff_active_day_percent
training_typical$prev_nforms<- training_typical$nforms - training_typical$diff_nforms
training_typical$prev_nvisits <- training_typical$nvisits - training_typical$diff_nvisits
training_typical$prev_nunique_followups <- training_typical$nunique_followups - training_typical$diff_nunique_followups
training_typical$prev_ncases_touched <- training_typical$ncases_touched - training_typical$diff_ncases_touched
training_typical$prev_ncases_registered <- training_typical$ncases_registered - training_typical$diff_ncases_registered
training_typical$prev_time_using_cc <- training_typical$time_using_cc - training_typical$diff_time_using_cc

#Must only include rows with previous_month_active == T. Exclude F & NA 
training_typical <- training_typical[training_typical$previous_month_active == T,]

test_2a <- 
  c(cor(training_typical$prev_active_day_percent, training_typical$active_day_percent, use = "complete.obs"),
    cor(training_typical$prev_nforms, training_typical$nforms, use = "complete.obs"),
    cor(training_typical$prev_nvisits, training_typical$nvisits, use = "complete.obs"),
    cor(training_typical$prev_nunique_followups, training_typical$nunique_followups, use = "complete.obs"),
    cor(training_typical$prev_ncases_touched, training_typical$ncases_touched, use = "complete.obs"),
    cor(training_typical$prev_ncases_registered, training_typical$ncases_registered, use = "complete.obs"),
    cor(training_typical$prev_time_using_cc, training_typical$time_using_cc, use = "complete.obs"))
names(test_2a) <- indicators
test_2a <- data.frame(round(test_2a, digits = 2))
names(test_2a) <- "intra_user_pearson_regular_users"
#test_2a_full <- test_2a

test <- data.frame(cbind(test_2a_full, test_2a))
write.csv(test, file = "intra_user_pearson_corr.csv")

#Correlation matrix
# Make datset with only the following variable of interest

corr_set <- all_monthly
#corr_set <- all_monthly2
corr_set <- corr_set[,names(corr_set) %in% indicators]

#Generate numerical correlation matrix
num_corr <- cor(corr_set, use="complete.obs")
num_corr <- data.frame(round(num_corr, digits = 2))
#num_corr_1 <- num_corr
#test <- num_corr_1 - num_corr

write.csv(num_corr, file = "corr_mat_regular_users.csv")