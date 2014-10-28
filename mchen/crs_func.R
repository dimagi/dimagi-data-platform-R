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


# total open/closed cases
get_open_close <- function(dt) {
  closed <- which(dt$closed == "TRUE")
  closed_case <- length(unique(dt[closed,]$case_id))
  open_case <- length(unique(dt$case_id)) - closed_case
  to_return <- c(open_case, closed_case)
  return(to_return)
}


# visits to open/closed case
get_visits_closed_case <- function(dt) {
  closed_case <- unique(dt[which(dt$closed == "TRUE"),]$case_id) # this returns a vector of cases that have been closed 
  return(closed_case)
}


# open case data
get_open_case <- function(dt) {
  case_open <- dt[which(dt$closed == "FALSE"),]
  return(case_open)
}


# this function selects the first row within a grouping
get_first_visit <- function(dt) {
  library(plyr)
  dt <- dt[order(dt$case_id, dt$time_start),]
  dt1 <- ddply(dt, .(case_id), function(x) x[1,])
  colnames(dt1)[9] <- c("first_visit")
  return(dt1)	
}


# this function selects the last row within a grouping 
get_last_visit <- function(dt) {
  library(plyr)
  dt <- dt[order(dt$case_id, dt$time_start),]
  dt2 <- ddply(dt, .(case_id), function(x) x[nrow(x),])
  colnames(dt2)[9] <- c("last_visit")
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

# match vector x, y and find element in vector x that do not match y
"%w/o%" <- function(x, y) x[!x %in% y]

# get active users for each type of cases
get_last_interaction <- function(dt) {
  library(plyr)
  dt <- dt[order(dt$user_id, dt$time_start),]
  dt5 <- ddply(dt, .(user_id), function(x) x[nrow(x),])
  colnames(dt5)[9] <- c("last_interaction")
  return(dt5)  
}

get_active_users <- function(dt) {
  user_interaction <- get_last_interaction(dt)
  inactive_line <- get_inactive_line(export_date, 30)
  user_interaction$active <- ifelse(as.Date(user_interaction$last_interaction) >= as.Date(inactive_interaction_line), "yes", "no")
  print(table(user_interaction$active))  
}
