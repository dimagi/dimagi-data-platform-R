# Formatting
dat$visit_date <- as.Date(dat$time_start)
dat$month.index <- as.yearmon(dat$visit_date) # obtaining year and month from Date
# Sorting
dat <- dat[order(dat$user_id, dat$time_start), ]  # sort visits by user_id and first interaction time

source(file.path("aggregate_tables","lifetime_func.R", fsep = .Platform$file.sep))
dat <- within(dat, date_difference <- dateDiff(visit_date, time_since_previous))
dat <- within(dat, batch_entry <- batchEntry(date_difference, time_since_previous, 600))

# days since last visit to a same case by any mobile worker (allows case-sharing)
dat <- dat[order(dat$case_id, dat$time_start), ]
dat <- within(dat, days_elapsed_case <- daysElapsedCase(case_id, visit_date))
dat <- within(dat, new_case <- newCase(days_elapsed_case)) # registering new cases
dat <- within(dat, follow_up <- followUp(days_elapsed_case)) # follow up visits


# visit hours (needs to be functioned)
dat$time_ffs <- strftime(dat$time_start, format = "%H:%M:%S") # extracts hours and minutes 
dat$visit_time <- ifelse(dat$time_ffs >= "06:00:00" & dat$time_ffs < "12:00:00", "morning", 
                          ifelse(dat$time_ffs >= "12:00:00" & dat$time_ffs < "18:00:00", "afternoon",
                                 ifelse(dat$time_ffs >= "18:00:00" & dat$time_ffs < "24:00:00", "night", "after midnight")))


# cases visited in an hour by a given user
dat$visit_hour <- strftime(dat$time_start, format = "%H") # extract hour from time vector
y <- as.data.frame(table(dat$user_id, dat$visit_date, dat$visit_hour))
names(y) <- c("user_id", "visit_date", "visit_hour", "cases_visited_per_hour")
y1 <- y[which(y$cases_visited_per_hour != 0),]  # exclude non-active visit_hour index 
y1_aggr <- aggregate(cases_visited_per_hour~user_id, data = y1, mean) # this returns the average number of cases that had interactions with user in an active hour
names(y1_aggr) <- c("user_id", "avg.cases_visited_per_active_hour")
y1_aggr$avg.cases_visited_per_active_hour <- round(y1_aggr$avg.cases_visited_per_active_hour, 2)


