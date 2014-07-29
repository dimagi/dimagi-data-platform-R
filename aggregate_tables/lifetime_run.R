makeLifeTime <- function(dat){
  # sort visits by time_start for a given user
  dat <- dat[order(dat$user_id, dat$time_start), ] 
  
  # first visit by a mobile worker throughout her CC lifecycle
  temp1 <- duplicated(dat$user_id) # this returns logical results, first visit = FALSE, all following visits are duplicates & TRUE
  first.visit <- dat[temp1 == FALSE, ] # this returns the first visit done each unqiue mobile user
  first.s <- first.visit[order(first.visit$user_id), ]
  
  # last visit by each unique mobile user
  temp2 <- duplicated(dat$user_id, fromLast = TRUE) # last visit = FALSE
  last.visit <- dat[temp2 == FALSE, ] 
  last.s <- last.visit[order(last.visit$user_id), ]
  
  # total visits of a mobile worker in her CommCare lifecycle
  nvisits <- as.data.frame(table(dat$user_id,useNA = "ifany")) 
  nvisits <- nvisits[order(nvisits$Var1), ] # Var1 = user_id, Freq = occurrence of a mobile worker = total visits  
  
  # total days on CC: [first_active_date, last_active_date]
  days_on_cc <- as.numeric(difftime(last.s$visit_date, first.s$visit_date, units = "days")) + 1  # this returns total days between the first and the last form submission by a mobile user
  
  # days elapsed since the last active date: [last_active_date, today()]
  days_visit_last <- as.numeric(difftime(Sys.Date(), last.s$visit_date, units = "days")) # this returns days elapsed between two visits by a user
  
  # combine indicators into a data frame Y (bad naming choice but ... for now i don't have a better name)
  y <- data.frame(first.s$user_id, first.s$visit_date, last.s$visit_date, nvisits$Freq, days_on_cc, days_visit_last)
  names(y) <- c("user_id", "date_first_visit", "date_last_visit", "nvisits", "days_on_cc", "days_visit_last")
  y$date_first_visit <- as.Date(strptime(y$date_first_visit, format = "%Y-%m-%d"))
  y$date_last_visit <- as.Date(strptime(y$date_last_visit, format = "%Y-%m-%d"))
  
  # define user type: active in last 30 days or not
  y$active_user <- ifelse(y$days_visit_last <= 30, 1, 0) 
  
  # remvoe incomplete first and last month of each mobile worker, keeping complete calendar month only
  y$calendar_month_start <- as.Date(as.yearmon(y$date_first_visit) + 1/12, frac = 0) # incomplete calendar month would not be 
  y$calendar_month_end <- as.Date(as.yearmon(y$date_last_visit), frac = 0) 
  
  # total calendar months on CommCare of each mobile worker
  next1 <- next.month(y$date_first_visit) # this returns the same Date in the following month
  index1 <- which(next1 == y$calendar_month_start)    # this returns the index of mobile users with a starting time on the first day in a month 
  if(length(index1)) y$calendar_month_start[index1] <- as.Date(as.yearmon(y$calendar_month_start[index1]) - 1/12, frac = 0) 
  
  next2 <- next.month(y$calendar_month_end)
  index2 <- which(as.numeric(difftime(next2, y$date_last_visit)) == 1) # this returns the index of mobile users who have a last_visit_date on the last day in a month
  if(length(index2))  y$calendar_month_end[index2] <- as.Date(as.yearmon(y$calendar_month_end[index2]) + 1/12, frac = 0) # update calendar month end for mobile users with a last visit date on the day ending the month
  
  y$calendar_months_on_cc <- 12*(as.yearmon(y$calendar_month_end) - as.yearmon(y$calendar_month_start)) # later to remove all users with total calendar months on CC < 3
  y$calendar_months_on_cc <- ifelse(y$calendar_months_on_cc < 0, 0, y$calendar_months_on_cc)
  
  # total active months of each mobile worker throughout CC lifecycle. 
  # I am not quite confident using this as a metric since we would have to drop a lot of users 
  # who have only been active for less than a month
  # we might have quite a lot of users showing such behavior pattern once we get data from more domains
  activity <- as.data.frame(table(dat$user_id, dat$month.index,useNA = "ifany")) # total visits per month
  activity <- activity[which(activity$Freq != 0), ] # drops months in which there is no form submission/visit
  activity.count <- aggregate(activity$Var2, list(activity$Var1), function(x) length(unique(x))) # number of unique months a worker is actively visiting cases
  activity.count.s <- activity.count[order(activity.count$Group.1), ] # sort data by user_id
  
  y$active_months <- activity.count.s$x - 2 # dropping the first and last active month of a given mobile worker to be consistent with calendar month
  if(length(index1)) y$active_months[index1] <- y$active_months[index1] + 1 # Adding one month back for users with a first visit on month begin
  if(length(index2)) y$active_months[index2] <- y$active_months[index2] + 1 # Adding one month back for users with a last visit on month end
  y$active_months <- ifelse(y$calendar_months_on_cc == 0, 0, y$active_months)
  
  # active month on CC vs. total months on CC
  y$active_month_percent <- as.numeric(round(y$active_months/y$calendar_months_on_cc, 2))  # denominator can be 0, active_month_percent in this case is NaN
  
  # total visits per day 
  activity_by_day <- as.data.frame(table(dat$user_id, dat$visit_date,useNA = "ifany"))
  activity_by_day <- activity_by_day[which(activity_by_day$Freq != 0),] # drop inactive days
  activity_by_day.count <- aggregate(activity_by_day$Var2, list(activity_by_day$Var1), function(x) length(unique(x))) # total active days of a user
  activity_by_day.count.s <- activity_by_day.count[order(activity_by_day.count$Group.1), ]
  y$active_days <- activity_by_day.count.s$x
  # active days on CC vs. total days on CC
  y$active_day_percent <- round(y$active_days/y$days_on_cc, 2) 
  
  # total forms submitted by a mobile worker on CC
  nforms <- aggregate(total_forms~user_id, data = dat, sum) 
  nforms <- nforms[order(nforms$user_id), ]
  y$nforms <- nforms$forms_per_visit
  
  # Visit duration. Median
  visit_duration <- aggregate(form_duration~user_id, data = dat, median) # in seconds
  y$median_visit_duration <- as.numeric(round(visit_duration$form_duration/60, 2)) # in minutes
  
  # total visits per day. Median
  d <- as.data.frame(table(dat$visit_date, dat$user_id,useNA = "ifany")) # number of unique visits in a day by a given mobile user
  d <- subset(d, Freq != 0) # drop inactive days
  dsub <- d[order(d$Var2), ] # sort by user_id
  d.median <- aggregate(dsub$Freq, list(dsub$Var2), median, na.rm = TRUE) 
  y$median_visits_per_day <- d.median$x  # median of unique visits in a day by a given user
  
  # total visits per month. Median
  m <- as.data.frame(table(dat$month.index, dat$user_id,useNA = "ifany"))
  m <- subset(m, Freq != 0)
  msub <- m[order(m$Var2), ]
  m.median <- aggregate(msub$Freq, list(msub$Var2), median, na.rm = TRUE)
  y$median_visits_per_month <- m.median$x
  
  # batch-entry visits vs. total visits 
  if(nrow(dat) > length(unique(dat$case_id))){
    temp <- as.matrix(table(dat$user_id, dat$batch_entry,useNA = "ifany")) # this returns a matrix of batch/non-batch for each unique worker
    # be careful when all users in a domain have one one visit only... 
    if(length(unlist(dimnames(temp)[2])) > 1) { # meaning there are both batch and non-batch visits in a domain
      batch <- as.data.frame(temp[, "1"]) 
      user_id <- dimnames(batch)[[1]]
      be <- cbind(batch, user_id)  
      names(be) <- c("total_batch_entry_visits", "user_id")
      y <- merge(be, y, by = "user_id", all.y = TRUE) # this merges batch_visit data frame with the big data frame, extra rows in be will be excluded
      y$batch_entry_percent <- round(y$total_batch_entry_visits/y$nvisits, 2)
    }else if(unlist(dimnames(temp)[2]) == 1){
      y$total_batch_entry_visits <- y$nvisits
      y$batch_entry_percent <- 1}else{
        y$total_batch_entry_visits <- 0
        y$batch_entry_percent <- 0}     
  }else{
    y$total_batch_entry_visits <- 0
    y$batch_entry_percent <- 0}     
  
  
  # compute total number of cases registered by a user
  new_case <- ddply(dat, .(user_id), function(x) length(which(x$new_case == 1))) # this returns total new cases with a first visit 
  follow_up <- ddply(dat, .(user_id), function(x) length(which(x$follow_up == 1)))
  colnames(new_case)[2] <- c("new_cases") # rename variables would make it cleaner when we merge all data frames
  colnames(follow_up)[2] <- c("follow_up")
  
  # time elapsed (mins) between followup visits to the same case
    ## exception: domains in which each user has one registration visit only
  dat <- dat[order(dat$user_id, dat$time_start), ] 
  if(nrow(dat[!is.na(dat$time_since_previous),])>1){
    median_time_elapse <- aggregate(time_since_previous~user_id, data = dat, median, na.rm = TRUE)
    names(median_time_elapse) <- c("user_id", "median_time_elapse")
    mean_time_elapse <- aggregate(time_since_previous~user_id, data = dat, mean, na.rm = TRUE)
    names(mean_time_elapse) <- c("user_id", "mean_time_elapse")    
  } else{
    median_time_elapse <- data.frame(user_id = dat$user_id, median_time_elapse = NA)
    mean_time_elapse <- data.frame(user_id = dat$user_id, mean_time_elapse = NA)  
  }
  
  
  dat <- dat[order(dat$user_id, dat$case_id, dat$time_start), ]
  dt1 <- tail(dat$time_start, -1) # time_start of all visits except the first one
  dt2 <- head(dat$time_end, -1) # time_end of all visits except the last one
  dat$mins_elapse_flw_same_case <- c(NA, as.numeric(difftime(dt1, dt2, units = "mins"))) # for a given case, time elapsed between interactions with the same mobile worker
  dat$mins_elapse_flw_same_case[which(diff(as.numeric(factor(dat$user_id, ordered = TRUE))) != 0) + 1] <- NA # replace each mobile worker's first visit with NA
  dat$mins_elapse_flw_same_case[which(diff(as.numeric(factor(dat$case_id, ordered = TRUE))) != 0) + 1] <- NA # replace each mobile worker's registration visit with NA
  
  
  visitTimePercentage(dat) -> visit_time_percentage
  reshapedVisitTime(visit_time_percentage) -> visit_time_percent # this returns the percentage of visits in morning, afternoon, night, and after midnight
  colnames(visit_time_percent)[1] <- c("user_id")
  
  
  # cases visited in an hour by a given user
  dat$visit_hour <- strftime(dat$time_start, format = "%H") # extract hour from time vector
  avg_cases_per_hour <- avgCasesPerHour(dat$user_id, dat$visit_date, dat$visit_hour)
  
  
  merge_me <- list(y, new_case, follow_up, median_time_elapse, mean_time_elapse, visit_time_percent, avg_cases_per_hour)
  y <- Reduce(function(...) merge(..., all=T), merge_me)
  y$follow_up_percent <- round(y$follow_up/y$new_cases, 2) # this returns percentage of cases with >1 iteractions with the mobile user
  
  # adding domain name 
  y$domain <- rep(dat$domain[1], nrow(y))  
  
  return(y)
}


