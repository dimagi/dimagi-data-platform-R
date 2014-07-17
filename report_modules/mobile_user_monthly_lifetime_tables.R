# report module - should be run from either r_script_runner.R or test_runner to R
# without these, the required config variables and file paths will not be set!

# install/import libraries 
library(zoo)
library(digest)
library(plyr)
library(timeDate)
library(reshape2)

if (debug_mode) {
  # get data from a csv file
  v<-get_interaction_table_from_csv(test_data_dir)
} else {
  # get data from a database query
  v <- get_interaction_table(con, domain_list)
}

v$visit_date <- as.Date(v$time_start)
v$month.index <- as.yearmon(v$visit_date) # obtaining year and month from Date

# remove demo_user
v0 <- v[which(v$user_id != "demo_user"),]
v0$user_id <- factor(v0$user_id)

# split into a list of data frames to operate
v1 <- dlply(v0, .(domain))

for (i in seq_along(v1)) {
  v1[[i]]$domain.index <- i 
  v1[[i]]$domain <- names(v1[i])
} # assign domain index 

# operate on list of data frames
v2 <- lapply(v1, function(x){
  # remove test users 
  nvisits_raw <- as.data.frame(table(x$user_id))  # there might be overlapping user_id across domains? Not sure
  nvisits_raw <- nvisits_raw[order(nvisits_raw$Var1), ]
  user_remove <- nvisits_raw$Var1[which(nvisits_raw$Freq == 1)] # remove users with one visit only (test users)
  x <- x[!x$user_id %in% user_remove,] # this removes users who have been active only for once in their CC lifecycle
  x$user_id <- factor(x$user_id)
  
  # batch entry visits
  x <- x[order(x$user_id, x$time_start), ]  
  x$date_difference <- c(NA, diff(x$visit_date)) # this examines if sorted visits happen in the same or different days
  x$date_difference[which(is.na(x$time_since_previous))] <- NA # date_difference should be NA for the first visit by each unique worker
  x$visit_weekday <- as.numeric(format(x$visit_date, "%w"))
  x$batch_entry <- ifelse(x$date_difference == 0 & x$time_since_previous < 600, 1, 0)  # time_since_previous is calculated in seconds
  temp <- which(x$batch_entry == 1) # this gives the index of batch_entry visits
  x$batch_entry[temp - 1] <- 1 # this flags previous visit of a batch_entry visit as batch
  
  # visit hours 
  x$time_ffs <- strftime(x$time_start, format = "%H:%M:%S")
  x$visit_hour <- strftime(x$time_start, format = "%H") # extract hour from time vector
  x$visit_time <- ifelse(x$time_ffs >= "06:00:00" & x$time_ffs < "12:00:00", "morning", 
                         ifelse(x$time_ffs >= "12:00:00" & x$time_ffs < "18:00:00", "afternoon",
                                ifelse(x$time_ffs >= "18:00:00" & x$time_ffs < "24:00:00", "night", "after midnight")))
  
  # days since last visit to a same case by any mobile worker (allows case-sharing)
  x <- x[order(x$case_id, x$time_start), ]
  x$days_elapsed_case <- c(NA, as.numeric(diff(x$visit_date, units = "days")))
  x$days_elapsed_case[which(diff(as.numeric(factor(x$case_id, ordered = TRUE))) != 0) + 1] <- NA 
  
  # identify visit type
  x$new_case <- ifelse(is.na(x$days_elapsed_case) == TRUE, 1, 0) # this flags all visits registering a new case
  x$follow_up <- ifelse(is.na(x$days_elapsed_case) == FALSE, 1, 0) # flagging all followup visits
  
  return(x)
})

# avg number of cases visited per hour throughout CC lifecycle
avg.cases.list <- lapply(v2, function(x){
  y <- as.data.frame(table(x$user_id, x$visit_date, x$visit_hour))
  names(y) <- c("user_id", "visit_date", "visit_hour", "cases_visited_per_hour")
  y1 <- y[which(y$cases_visited_per_hour != 0),]  # exclude non-active visit_hour index 
  y1_aggr <- aggregate(cases_visited_per_hour~user_id, data = y1, mean) # this returns the average number of cases that had interactions with user in an active hour
  names(y1_aggr) <- c("user_id", "avg.cases_visited_per_active_hour")
  y1_aggr$avg.cases_visited_per_active_hour <- round(y1_aggr$avg.cases_visited_per_active_hour, 2)
  return(y1_aggr) 
})

# compute user indexed table
f0 <- lapply(v2, function(x) {
  x$user_id <- factor(x$user_id) # drop unused factor level
  x <- x[order(x$user_id, x$time_start), ] # sort visits by time_start for a given user
  
  # first visit by a mobile worker throughout her CC lifecycle
  temp1 <- duplicated(x$user_id) # this returns logical results, first visit = FALSE, all following visits are duplicates & TRUE
  first.visit <- x[temp1 == FALSE, ] # this returns the first visit done each unqiue mobile user
  
  # last visit by each unique mobile user
  temp2 <- duplicated(x$user_id, fromLast = TRUE) # last visit = FALSE
  last.visit <- x[temp2 == FALSE, ] 
  
  # total visits of a mobile worker in her CommCare lifecycle
  nvisits <- as.data.frame(table(x$user_id)) 
  nvisits <- nvisits[order(nvisits$Var1), ] # Var1 = user_id, Freq = occurrence of a mobile worker = total visits
  
  # total days on CC: [first_active_date, last_active_date]
  first.s <- first.visit[order(first.visit$user_id), ]
  last.s <- last.visit[order(last.visit$user_id), ]
  days_on_cc <- as.numeric(difftime(last.s$visit_date, first.s$visit_date, units = "days")) + 1  # this returns total days between the first and the last form submission by a mobile user
  
  # days elapsed since the last active date: [last_active_date, today()]
  days_visit_last <- as.numeric(difftime(Sys.Date(), last.s$visit_date, units = "days")) # this returns days elapsed between two visits by a user
  
  # combine indicators into a data frame
  y <- data.frame(first.s$user_id, first.s$visit_date, last.s$visit_date, nvisits$Freq, days_on_cc, days_visit_last)
  names(y) <- c("user_id", "date_first_visit", "date_last_visit", "nvisits", "days_on_cc", "days_visit_last")
  y$date_first_visit <- as.Date(strptime(y$date_first_visit, format = "%Y-%m-%d"))
  y$date_last_visit <- as.Date(strptime(y$date_last_visit, format = "%Y-%m-%d"))
  
  # define user type: active in last 30 days or not
  y$active_user <- ifelse(y$days_visit_last <= 30, 1, 0) 
  
  # remvoe incomplete first and last month of each mobile worker, keeping complete calendar month only
  y$calendar_month_start <- as.Date(as.yearmon(y$date_first_visit) + 1/12, frac = 0) # incomplete calendar month would not be 
  y$calendar_month_end <- as.Date(as.yearmon(y$date_last_visit), frac = 0) 
  next.month <- function(d) as.Date(as.yearmon(d) + 1/12) + 
    as.numeric(d - as.Date(as.yearmon(d)))
  next1 <- next.month(y$date_first_visit) # this returns the same Date in the following month
  index1 <- which(next1 == y$calendar_month_start)    # this returns the index of mobile users with a starting time on the first day in a month 
  if(length(index1)) y$calendar_month_start[index1] <- as.Date(as.yearmon(y$calendar_month_start[index1]) - 1/12, frac = 0) 
  
  # total calendar months on CommCare of each mobile worker
  next2 <- next.month(y$calendar_month_end)
  index2 <- which(as.numeric(difftime(next2, y$date_last_visit)) == 1) # this returns the index of mobile users who have a last_visit_date on the last day in a month
  if(length(index2))  y$calendar_month_end[index2] <- as.Date(as.yearmon(y$calendar_month_end[index2]) + 1/12, frac = 0) # update calendar month end for mobile users with a last visit date on the day ending the month
  y$calendar_months_on_cc <- 12*(as.yearmon(y$calendar_month_end) - as.yearmon(y$calendar_month_start)) # later to remove all users with total calendar months on CC < 3
  y$calendar_months_on_cc <- ifelse(y$calendar_months_on_cc < 0, 0, y$calendar_months_on_cc)
  
  # total active months of each mobile worker throughout CC lifecycle. 
  # I am not quite confident using this as a metric since we would have to drop a lot of users 
  # who have only been active for less than a month
  # we might have quite a lot of users showing such behavior pattern once we get data from more domains
  activity <- as.data.frame(table(x$user_id, x$month.index)) # total visits per month
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
  activity_by_day <- as.data.frame(table(x$user_id, x$visit_date))
  activity_by_day <- activity_by_day[which(activity_by_day$Freq != 0),] # drop inactive days
  activity_by_day.count <- aggregate(activity_by_day$Var2, list(activity_by_day$Var1), function(x) length(unique(x))) # total active days of a user
  activity_by_day.count.s <- activity_by_day.count[order(activity_by_day.count$Group.1), ]
  y$active_days <- activity_by_day.count.s$x
  # active days on CC vs. total days on CC
  y$active_day_percent <- round(y$active_days/y$days_on_cc, 2) 
  
  # total forms submitted by a mobile worker on CC
  nforms <- aggregate(total_forms~user_id, data = x, sum) 
  nforms <- nforms[order(nforms$user_id), ]
  y$nforms <- nforms$forms_per_visit
  
  # Visit duration. Median
  visit_duration <- aggregate(form_duration~user_id, data = x, median) # in seconds
  y$median_visit_duration <- as.numeric(round(visit_duration$form_duration/60, 2)) # in minutes
  
  # total visits per day. Median
  d <- as.data.frame(table(x$visit_date, x$user_id)) # number of unique visits in a day by a given mobile user
  d <- subset(d, Freq != 0) # drop inactive days
  dsub <- d[order(d$Var2), ] # sort by user_id
  d.median <- aggregate(dsub$Freq, list(dsub$Var2), median, na.rm = TRUE) 
  y$median_visits_per_day <- d.median$x  # median of unique visits in a day by a given user
  
  # total visits per month. Median
  m <- as.data.frame(table(x$month.index, x$user_id))
  m <- subset(m, Freq != 0)
  msub <- m[order(m$Var2), ]
  m.median <- aggregate(msub$Freq, list(msub$Var2), median, na.rm = TRUE)
  y$median_visits_per_month <- m.median$x
  
  # batch-entry visits vs. total visits 
  temp <- as.matrix(table(x$user_id, x$batch_entry)) # this returns a matrix of batch/non-batch for each unique worker
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
  
  
  # adding domain index and domain name 
  y$domain.index <- rep(x$domain.index[1], nrow(y)) 
  y$domain.index <- factor(y$domain.index)
  y$domain.name <- rep(x$domain[1], nrow(y))
  
  # compute total number of cases registered by a user
  new_case <- ddply(x, .(user_id), function(x) length(which(x$new_case == 1))) # this returns total new cases with a first visit 
  follow_up <- ddply(x, .(user_id), function(x) length(which(x$follow_up == 1)))
  colnames(new_case)[2] <- c("new_cases") # rename variables would make it cleaner when we merge all data frames
  colnames(follow_up)[2] <- c("follow_up")
  
  merge_me <- list(y, new_case, follow_up)
  y1 <- Reduce(function(...) merge(..., all=T), merge_me)
  return(y1)
})


# time elapsed between interactions with cases 
f1 <- lapply(v2, function(x){
  # time elapsed (in seconds) between visits {Median, Mean} for a mobile worker in her lifecycle
  v <- x[order(x$user_id, x$time_start), ] 
  median_time_elapse <- aggregate(time_since_previous~user_id, data = v, median, na.rm = TRUE)
  names(median_time_elapse) <- c("user_id", "median_time_elapse")
  mean_time_elapse <- aggregate(time_since_previous~user_id, data = v, mean, na.rm = TRUE)
  names(mean_time_elapse) <- c("user_id", "mean_time_elapse")
  
  # time elapsed (in minutes) between followup visits to (interactions with) the same case
  # unique mobile workers here might be less than total unique mobile workers active on the domain bc some mobile worker might have registration-only visits
  c <- x[order(x$user_id, x$case_id, x$time_start), ]
  ct1 <- tail(c$time_start, -1) # time_start of all visits except the first one
  ct2 <- head(c$time_end, -1) # time_end of all visits except the last one
  c$mins_elapse_flw_same_case <- c(NA, as.numeric(difftime(ct1, ct2, units = "mins"))) # for a given case, time elapsed between interactions with the same mobile worker
  c$mins_elapse_flw_same_case[which(diff(as.numeric(factor(c$user_id, ordered = TRUE))) != 0) + 1] <- NA # replace each mobile worker's first visit with NA
  c$mins_elapse_flw_same_case[which(diff(as.numeric(factor(c$case_id, ordered = TRUE))) != 0) + 1] <- NA # replace each mobile worker's registration visit with NA
  c1 <- aggregate(mins_elapse_flw_same_case~user_id+case_id, data = c, median, na.rm = TRUE) # this calculates the median minutes elapsed between followup visits to each registered case  
  c2 <- aggregate(c1$mins_elapse_flw_same_case~user_id, data = c1, median, na.rm = TRUE) # this calculates the median time elapsed between followup visits throughout the mobile worker's lifecycle
  names(c2) <- c("user_id", "median_time_btw_followup")
  c3 <- aggregate(c1$mins_elapse_flw_same_case~user_id, data = c1, mean, na.rm = TRUE)
  names(c3) <- c("user_id", "mean_time_btw_followup")
  
  merge_me <- list(median_time_elapse, mean_time_elapse, c2, c3)
  y <- Reduce(function(...) merge(..., all=T), merge_me)
  y$median_days_btw_followup <- round(y$median_time_btw_followup/60/24, 2)
  y$mean_days_btw_followup <- round(y$mean_time_btw_followup/60/24, 2) # convert minutes into days
  return(y)
})

visit_time_percentage <- lapply(v2, function(x){
  ddply(x, .(user_id), function(x){
    r1 <- data.frame(table(x$visit_time)/nrow(x))
    names(r1) <- c('time', 'proportion')
    r1$proportion <- round(100*r1$proportion, 2)
    return(r1)    
  })
})
reshaped_visit_time <- lapply(visit_time_percentage, function(x){
  r2 <- dcast(x, user_id~time)
  return(r2)
})

# adding avg.cases visited per hour to user-indexed table
# apply merge to two lists of data frames
f0.flat <- do.call("rbind", f0) 
f1.flat <- do.call("rbind", f1)
avg.cases <- do.call("rbind", avg.cases.list) 
reshaped_visit_time <- do.call("rbind.fill", reshaped_visit_time)

# merge all data frames into lifetime table for all mobile workers across domains
merge_me <- list(f0.flat, f1.flat, avg.cases, reshaped_visit_time)
f2 <- Reduce(function(...) merge(..., all=T), merge_me)
f2$case_followup_rate <- round(f2$follow_up/f2$new_cases, 2)


######################################
# Monthly table computation
######################################

# split the big visit table into small ones per mobile worker
user_list <- lapply(v2, function(x) {
  x.split <- split(x, x$user_id)
  return(x.split)
}) # this splits visit tables into a list of list. Layer 1 is domain list, the element in each domain list is mobile worker

user_list <- unlist(user_list, recursive = F) # unlist melts domain list

# this operation returns first and last day in each active month 
user_list_1 <- lapply(user_list, function(x) {
  arrange(x, time_start) 
  x$bar1 <- as.Date(timeFirstDayInMonth(as.character(x$visit_date), format = "%Y-%m-%d")) # this actually repeats the work done in lifetime table but for now i will just leave it the way it is
  x$bar2 <- as.Date(timeLastDayInMonth(as.character(x$visit_date), format = "%Y-%m-%d"))
  x$month.index <- as.factor(x$month.index)
  return(x)
})


m <- lapply(user_list_1, function(x){
  # total visits per active month
  y0 <- as.data.frame(table(x$month.index))
  names(y0) <- c("month.index", "visits") 
  
  # total form submissions per active month
  y1 <- aggregate(total_forms~month.index, data = x, sum)
  names(y1) <- c("month.index", "nforms_per_month") # total form submissions in each active month
  
  # follow up visits per month: multiple visits can be done to one case
  y2 <- aggregate(follow_up~month.index, data = x, sum) 
  names(y2) <- c("month.index", "follow_up_visits")  
  y2$follow_up_percent <- round(100*y2$follow_up_visits/y0$visits, 2)
  
  # follow up visits to each unique case  
  y3 <- ddply(x, .(month.index), function(x) length(unique(x[x$follow_up == 1, ]$case_id)))
  names(y3) <- c("month.index", "follow_up_unique_case") # This calculates follow up visits to a unique case in an active month 
  
  # total active days per active month
  y4 <- aggregate(x$visit_date, list(x$month.index), function(x) length(unique(x)))
  names(y4) <- c("month.index", "active_days_per_month") 
  y4$active_days_percent <- round(100*y4$active_days_per_month/30, 2) # demoninator should be specified number of days in each month...
  
  # first/last active day in an active month
  y5 <- aggregate(visit_date~month.index, data = x, min)
  names(y5) <- c("month.index", "first_visit_date") 
  y6 <- aggregate(visit_date~month.index, data = x, max)
  names(y6) <- c("month.index", "last_visit_date") 
  # days between first and last active date in a month
  y6$days_on_cc <- as.numeric(y6$last_visit_date - y5$first_visit_date) + 1 
  
  # total batch entry visits in an active month
  y7 <- aggregate(batch_entry~month.index, data = x, sum, na.action = na.pass)
  y7$batch_entry_percent <- round(100*y7$batch_entry/y0$visits, 2) 
  
  # median visits per active day in a given month
  y_temp <- as.data.frame(table(x$visit_date))
  names(y_temp) <- c("active_date", "nvisits") # this computes total visits on each active day
  y_temp$month.index <- factor(as.yearmon(y_temp$active_date))
  y8 <- aggregate(nvisits~month.index, data = y_temp, median)
  names(y8) <- c("month.index", "median_visits_per_active_day")
  y9 <- aggregate(form_duration~month.index, data = x, median)
  names(y9) <- c("month.index", "median_visit_duration")
  y9$median_visit_duration <- round(y9$median_visit_duration/60, 2) # convert from seconds to mins
  
  # new cases registered per month
  t1 <- as.data.frame(table(x$new_case, x$month.index))
  names(t1) <- c("new_case", "month.index", "count") # new_case is a dummy created earlier to flag registration/followup visits
  t2 <- ddply(t1, .(month.index), function(x) sum(x[x$new_case == 1, ]$count)) 
  names(t2) <- c("month.index", "case_registered") 
  t2$cum_case_registered <- cumsum(t2$case_registered) # could be an empty data frame if there is no new case in a month
  
  # percentage of cases registered per active month 
  tot_new_case <- length(which(x$new_case == 1)) # TOTAL REGISTERED CASES
  t2$new_case_percent <- round(100*t2$case_registered/tot_new_case, 2) 
  
  
  # proportion of visits in different time period in a day (am,pm,after-pm, etc)
  res1 <- ddply(x, c("user_id", "month.index"), function(x){
    r1 <- data.frame(table(x$visit_time)/nrow(x))
    names(r1) <- c('time', 'proportion')
    r1$proportion <- round(100*r1$proportion, 2)
    return(r1)    
  })
  res1_cast <- dcast(res1, user_id + month.index ~ time) # using...override will show up. That's not an err. Don't panic
  res1_cast$month.index <- as.factor(res1_cast$month.index)
  
  # merge data frames  
  if(length(t2)) merge_me <- list(y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,t2, res1_cast) else merge_me <- list(y0,y1,y2,y3,y4,y5,y6,y7,y8,y9, res1_cast) # cum_case_registered could be empty
  y10 <- Reduce(function(...) merge(..., all=T), merge_me)
  
  # followup rate to registered cases of a mobile worker
  y10$unique_case_followup_rate <- round(100*y10$follow_up_unique_case/y10$cum_case_registered, 2) # could be Inf if a mobile worker only visits cases already registered and shared by other mobile workers
  y10$user_id <- x$user_id[1] # paste user_id to the data frame
  y10$domain.index <- rep(x$domain.index[1], nrow(y10))
  y10$domain <- rep(x$domain[1], nrow(y10))
  y10 <- arrange(y10, y10$month.index)
  y10$month.index.numeric <- as.numeric(y10$month.index) # compute month index in numeric order for visualization
  
  return(y10)
})

monthly_merge <- do.call("rbind.fill", m) # this does not filter out any flw with only a few visits in one incomplete calendar month



#################################
mainDir <- output_dir
subDir0 <- "monthly"
dir.create(file.path(mainDir, subDir0))
setwd(file.path(mainDir, subDir0))
write.csv(monthly_merge, "monthly_merge.csv") # this keeps the monthly table output into wherever the working directory of R session is

#export visit table output
subDir1 <- "visit"
dir.create(file.path(mainDir, subDir1))
setwd(file.path(mainDir, subDir1))

filename = vector()
visitOut <- function(x) {
  for (i in seq_along(x)) {
    filename[i] <- paste(x[[i]]$domain[1], sep = "", ".csv")
    write.csv(x[[i]], filename[i])
  }
}
visitOut(v2)



# export lifetime table 
subDir2 <- "lifetime"
dir.create(file.path(mainDir, subDir2))
setwd(file.path(mainDir, subDir2))
lifetime.split <- split(f2, f2$domain.name)

lifeOut <- function(x) {
  for (i in seq_along(x)) {
    filename[i] = paste(x[[i]]$domain.name[1], ".csv")
    write.csv(x[[i]], filename[i])
  }
}
lifeOut(lifetime.split) 


######################################
# Rashmi: attrition report