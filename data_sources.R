library(lubridate)
library(zoo)
source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep),chdir=T)

get_data_source <- function (db, table_name, limit) {
  tryCatch({
    query<-build_sql('SELECT * FROM ', ident(table_name),' limit ', as.integer(limit))
    return(tbl(db, sql(query)))
  }, error = function(err) {
    s <- do.call(sprintf("get_%s",table_name),args=list(db, limit))
    return(s)
  })
}

get_visit_detail <- function(db, limit){
  source(file.path("aggregate_tables", "lifetime_func.R", fsep=.Platform$file.sep))
  print(paste('Fetching visit detail table, limit is ', limit))
  dat <- get_visit_detail_table(db, limit)
  user_start_dates <- dat %.% group_by (domain,user_id) %.% summarise(user_start_date = min(time_start) )
  dat <- merge(dat,user_start_dates,by=c("domain","user_id"))
  
  dat$user_id[is.na(dat$user_id)] <- "NONE"
  # Formatting
  dat$visit_date <- as.Date(dat$time_start)
  dat$month.index <- as.character(as.yearmon(dat$visit_date)) # dplyr doesn't handle yearmon data type
  
  # Sorting
  dat <- dat[order(dat$user_id, dat$time_start), ]  # sort visits by user_id and first interaction time
  dat <- within(dat, date_difference <- dateDiff(visit_date, time_since_previous))
  dat <- within(dat, batch_entry <- batchEntry(date_difference, time_since_previous, 600))
  
  # visit hours (needs to be functioned)
  dat$time_ffs <- strftime(dat$time_start, format = "%H:%M:%S") # extracts hours and minutes
  dat$visit_time <- ifelse(dat$time_ffs >= "06:00:00" & dat$time_ffs < "12:00:00", "morning",
                           ifelse(dat$time_ffs >= "12:00:00" & dat$time_ffs < "18:00:00", "afternoon",
                                  ifelse(dat$time_ffs >= "18:00:00" & dat$time_ffs < "24:00:00", "night", "after midnight")))
  return(dat)
}

get_interactions <- function(db, limit){
  source(file.path("aggregate_tables", "lifetime_func.R", fsep=.Platform$file.sep))
  print(paste('Fetching interactions table, limit is ', limit))
  dat <- get_interaction_table(db, limit)
  
  dat$user_id[is.na(dat$user_id)] <- "NONE"
  # Formatting
  dat$visit_date <- as.Date(dat$time_start)
  dat$month.index <- as.character(as.yearmon(dat$visit_date)) # dplyr doesn't handle yearmon data type
  
  # Sorting
  dat <- dat[order(dat$user_id, dat$time_start), ]  # sort visits by user_id and first interaction time
  
  # days since last visit to a same case by any mobile worker (allows case-sharing)
  dat <- dat[order(dat$case_id, dat$time_start), ]
  dat <- within(dat, days_elapsed_case <- daysElapsedCase(case_id, visit_date))
  dat <- within(dat, new_case <- newCase(days_elapsed_case)) # registering new cases
  dat <- within(dat, follow_up <- followUp(days_elapsed_case)) # follow up visits
  
  return(dat)
}

get_device_type <- function(db, limit){
  print(paste('Fetching device type table, limit is ', limit))
  device_type_table <- get_device_type_table(db, limit)
  device_type_table <- collect (device_type_table)
  device_type_table$month.index <-  as.character(as.yearmon(device_type_table$time_start))
  return(device_type_table)
}