# compute monthly usage table for each mobile user
dat <- arrange(dat, time_start)
dat$bar1 <- as.Date(timeFirstDayInMonth(as.character(dat$visit_date), format = "%Y-%m-%d")) # this actually repeats the work done in lifetime table but for now i will just leave it the way it is
dat$bar2 <- as.Date(timeLastDayInMonth(as.character(dat$visit_date), format = "%Y-%m-%d"))
dat$month.index <- as.factor(dat$month.index)

user_0 <- ddply(dat, user_id ~ month.index, nrow) # this counts visits happening in each month of a given user
colnames(user_0)[3] <- c("visits")

# total form submissions in each active month
user_1 <- ddply(dat, .(user_id, month.index), summarize, nforms_per_month = sum(total_forms)) 

# follow up visits per month: multiple visits can be done to one case
user_2 <- ddply(dat, .(user_id, month.index), summarize, follow_up_visits = sum(follow_up)) 

# follow up visits to a unique case in an active month
user_3 <- ddply(dat, .(user_id, month.index), function(x) length(unique(x[x$follow_up == 1, ]$case_id)))
colnames(user_3)[3] <- c("follow_up_unique_case") 

# total active days per active month
user_4 <- ddply(dat, .(user_id, month.index), function(x) length(unique(x$visit_date)))
colnames(user_4)[3] <- c("active_days_per_month")

# first/last active day in an active month
user_5 <- ddply(dat, .(user_id, month.index), function(x) min(x$visit_date))
colnames(user_5)[3] <- c("first_visit_date")
user_6 <- ddply(dat, .(user_id, month.index), function(x) max(x$visit_date))
colnames(user_6)[3] <- c("last_visit_date")

# days between first and last active date in a month
user_6$days_on_cc <- as.numeric(user_6$last_visit_date - user_5$first_visit_date) + 1 

# total batch entry visits in an active month
user_7 <- ddply(dat, .(user_id, month.index), function(x) sum(x$batch_entry))
colnames(user_7)[3] <- c("batch_entry")

# median visits per active day in a given month
user_temp <-  ddply(dat, user_id ~ visit_date, nrow)
colnames(user_temp)[2:3] <- c("active_date", "nvisits")
user_temp$month.index <- factor(as.yearmon(user_temp$active_date))
user_8 <- ddply(user_temp, .(user_id, month.index), function(x) median(x$nvisits))
colnames(user_8)[3] <- c("median_visits_per_active_day")

user_9 <- ddply(dat, .(user_id, month.index), function(x) median(x$form_duration))
colnames(user_9)[3] <- c("median_visit_duration") # in seconds


# new cases registered per month
temp_1 <- ddply(dat, .(user_id, month.index), function(x) length(which(x$new_case == 1)))
colnames(temp_1)[3] <- c("case_registered")


# proportion of visits in different time period in a day (am,pm,after-pm, etc)
res1 <- ddply(dat, c("user_id", "month.index"), function(x){
  r1 <- data.frame(table(x$visit_time)/nrow(x))
  names(r1) <- c('time', 'proportion')
  r1$proportion <- round(100*r1$proportion, 2)
  return(r1)    
})
res1_cast <- dcast(res1, user_id + month.index ~ time) # using...override will show up. That's not an err. Don't panic
res1_cast$month.index <- as.factor(res1_cast$month.index)

# merge data frames  
if(length(temp_1)) merge_me <- list(user_0,user_1,user_2,user_3,user_4,user_5,user_6,user_7,user_8,user_9,temp_1, res1_cast) else merge_me <- list(user_0,user_1,user_2,user_3,user_4,user_5,user_6,user_7,user_8,user_9, res1_cast) # cum_case_registered could be empty
user_10 <- Reduce(function(...) merge(..., all=T), merge_me)

# followup rate to registered cases of a mobile worker
user_10$cum_case_registered <- cumsum(user_10$case_registered)
user_10$unique_case_followup_rate <- round(100*user_10$follow_up_unique_case/user_10$cum_case_registered, 2) # could be Inf if a mobile worker only visits cases already registered and shared by other mobile workers
user_10$domain.index <- rep(dat$domain.index[1], nrow(user_10))
user_10$domain <- dname #TODO this shouldn't have to be global


# Percentage indicator computation below
user_10$batch_entry_percent <- user_10$batch_entry/user_10$visits
user_10$follow_up_percent <- user_10$follow_up_visits/user_10$visits # percentage of visits that are interactions with registered cases
user_10$active_day_percent <- user_10$active_days_per_month/user_10$days_on_cc 
user_10$new_case_percent <- user_10$case_registered/user_10$cum_case_registered 

# numeric month index
source(file.path("aggregate_tables","monthly_func.R", fsep = .Platform$file.sep))
user_10 <- user_10[order(user_10$user_id, user_10$month.index),]
user_10 <- ddply(user_10, .(user_id), transform, numeric = monnb(first_visit_date))
user_10 <- ddply(user_10, .(user_id), transform, diff = c(0, diff(numeric)))
user_10 <- ddply(user_10, .(user_id), transform, numeric_index = cumsum(diff) + 1) 

# N+1/3/5 indicators
user_10 <- ddply(user_10, .(user_id), transform, next_mon_1 = perf_predict(numeric_index, 1))
user_10 <- ddply(user_10, .(user_id), transform, next_mon_3 = perf_predict(numeric_index, 3))
user_10 <- ddply(user_10, .(user_id), transform, next_mon_5 = perf_predict(numeric_index, 5))


