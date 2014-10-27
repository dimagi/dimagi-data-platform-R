# total cases not created/followed up by demo_user (all cases, not by type)
merged <- merged[-which(merged$user_id == "demo_user"),]
length(unique(merged$case_id))

# date of data export
export_date <- c("2014-10-12")
get_inactive_line <- function(d1, d2) {  # d1 format: YY-MM-DD
	as.Date(d1) - d2
}


# data by case type
get_case_data <- function(dt, case_type) {
	case_dt <- dt[which(dt$case_type == case_type), ]
	return(case_dt)
}

# open case 
get_open_case <- function(dt) {
  case_open <- dt[which(dt$closed == "FALSE"),]
  return(case_open)
}

# this function selects the first row within a grouping
get_first_visit <- function(dt) {
	library(plyr)
	dt <- dt[order(dt$case_id, dt$time_start),]
	dt1 <- ddply(dt, .(case_id), function(x) x[1,])
	colnames(dt1)[8] <- c("first_visit")
	return(dt1)	
}


# this function selects the last row within a grouping 
get_last_visit <- function(dt) {
	library(plyr)
	dt <- dt[order(dt$case_id, dt$time_start),]
	dt2 <- ddply(dt, .(case_id), function(x) x[nrow(x),])
	colnames(dt2)[8] <- c("last_visit")
	return(dt2)
} 


# total visits for each case
get_total_visits <- function(dt) {
	library(plyr)
	dt <- dt[order(dt$case_id, dt$time_start),]
	dt3 <- ddply(dt, .(case_id), function(x) nrow(x))
	colnames(dt3)[2] <- c("total_visits")
	return(dt3)
}


# avg. days between each visit 
get_avg_days_elapsed <- function(dt) {
	library(plyr)
	dt <- dt[order(dt$case_id, dt$time_start),]
	dt4 <- ddply(dt, .(case_id), function(x) mean(as.numeric(diff(as.Date(x$time_start))))) # unit is days
	colnames(dt4)[2] <- c("avg_days_elapsed_btw_visits")
	return(dt4)
}

inactive_line <- get_inactive_line(export_date, 120)
baby <- get_case_data(merged, "baby")
baby_first <- get_first_visit(baby)
baby_last <- get_last_visit(baby)
baby_total_visits <- get_total_visits(baby)

# merge all tables having case_id as the primary key
baby_first <- baby_first[order(baby_first$case_id),]
baby_last <- baby_first[order(baby_last$case_id),]
baby_total_visits <- baby_total_visits[order(baby_total_visits$case_id),]
baby_first$last_visit <- baby_last$last_visit
baby_first$total_visits <- baby_total_visits$total_visits

# total days between first and last visit
baby_first$total_days <- as.numeric(as.Date(baby_first$last_visit) - as.Date(baby_first$first_visit))
avg_days_between_visits <- get_avg_days_elapsed(baby)
baby_first$avg_days_between_visits <- avg_days_between_visits$avg_days_elapsed_btw_visits
baby_first$inactive <- ifelse(as.Date(baby_first$last_visit) < get_inactive_line(export_date, 120), "yes", "no")
baby_first$days_since_registration <- as.numeric(as.Date(export_date) - as.Date(baby_first$first_visit))

# crosstab active/inactive
case_activity <- as.data.frame(table(baby_first$inactive))

# inactive subset
inactive_baby <- baby_first[which(baby_first$inactive == "yes"),]

# get owner_id from case_hq data: 
matched_case_seq <- which(case_hq$info.case_id %in% baby_first$case_id)
matched_case_owner <- subset(case_hq[matched_case_seq,], select = c(info.case_id, info.owner_id))
baby_with_owner <- merge(baby_first, matched_case_owner, by.x = "case_id", by.y = "info.case_id", all.y = T)

# baby_with_owner will be the subset to work on from here
table(baby_with_owner$inactive)

inactive_baby_with_owner <- baby_with_owner[which(baby_with_owner$inactive == "yes"),]
length(which(inactive_baby_with_owner$total_visits == 1)) # inactive baby cases with registration visit only 
length(which(inactive_baby_with_owner$closed == TRUE)) # inactive baby cases that have been closed
length(which(inactive_baby_with_owner$closed == TRUE && inactive_baby_with_owner$total_visits == 1))

avg_visits <- ddply(inactive_baby_with_owner, .(closed), function(x) mean(x$total_visits)) # average visits to closed/open inactive cases
avg_days <- ddply(inactive_baby_with_owner, .(closed), function(x) mean(x$total_days)) 


# days since registration
get_days_since_registration <- function(data) {
	data$days_since_registration <- as.numeric(Sys.Date() - as.Date(data$first_visit))
	return(data)
}
baby_first$days_since_registration <- as.numeric(as.Date("2014-10-12") - as.Date(baby_first$first_visit))
mother_first$days_since_registration <- as.numeric(as.Date("2014-10-12") - as.Date(mother_first$first_visit))
referral_first$days_since_registration <- as.numeric(as.Date("2014-10-12") - as.Date(referral_first$first_visit))

get_active_days_percent <- function(data) {
	data$active_days_percent <- round(data$total_days/data$days_since_registration, digits = 2)
	return(data)
}

baby_first <- get_active_days_percent(baby_first)
mother_first <- get_active_days_percent(mother_first)
referral_first <- get_active_days_percent(referral_first)

get_anormal <- function(data, n, status) {
	anormal_list <- length(which(data$days_since_registration > n & data$closed == "FALSE" & data$inactive == status)) # obtaining cases which have been open on CC for more than 2 years
	anormal_percent <- round(anormal_list/nrow(data), digits = 2)
	to_return <- c(anormal_list, anormal_percent)
	return (to_return)
}

get_anormal(baby_first, 730, "yes")
get_anormal(baby_first, 730, "no")

get_anormal(mother_first, 270, "yes")
get_anormal(mother_first, 270, "no")

get_anormal(referral_first, 14, "yes")
get_anormal(referral_first, 14, "no")

# get mobile user activity status
baby_with_owner <- baby_with_owner[order(baby_with_owner$user_id, baby_with_owner$last_visit),]
user_last_visit <- ddply(baby_with_owner, .(user_id), function(x) x[nrow(x),]) # first_visit in this dataset = the latest visit made by the user
user_last_visit$user_inactive <- ifelse(as.Date(user_last_visit$first_visit) - as.Date(inactive_line) > 120, "yes", "no")
colnames(user_last_visit)[8] <- c("case_last_visit")

# total inactive cases owned by each user
