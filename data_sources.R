library(lubridate)
library(zoo)
source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep),chdir=T)

get_data_source <- function (db, table_name, limit=-1) {
  tryCatch({
    if (limit>0) {
      query<-build_sql('SELECT * FROM ', ident(table_name),' limit ', as.integer(limit))
    }
    else {
      query<-build_sql('SELECT * FROM ', ident(table_name))
    }
    return(tbl(db, sql(query)))
  }, error = function(err) {
    s <- do.call(sprintf("get_%s",table_name),args=list(db, limit))
    return(s)
  })
}

get_visit_detail <- function(db, limit=-1){
  source(file.path("aggregate_tables", "lifetime_func.R", fsep=.Platform$file.sep))
  print(paste('Fetching visit detail table, limit is ', limit))
  dat <- get_visit_detail_table(db, limit)
  user_start_dates <- dat %.% group_by (domain,user_id) %.% summarise(user_start_date = min(time_start) )
  dat <- merge(dat,user_start_dates,by=c("domain","user_id"))

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

get_interactions <- function(db, limit=-1){
  print(paste('Fetching interactions table, limit is ', limit))
  dat <- get_interaction_table(db, limit)
  
  # Formatting
  dat$visit_date <- as.Date(dat$time_start)
  dat$month.index <- as.character(as.yearmon(dat$visit_date)) # dplyr doesn't handle yearmon data type
  dat <- dat[order(dat$user_id, dat$time_start), ]  # sort visits by user_id and first interaction time
  
  # days since last visit to a same case by any mobile worker
  dat$days_elapsed_case <- as.integer(difftime(dat$time_start, dat$prev_visit_start, units = "days"))
  
  return(dat)
}

get_device_type <- function(db, limit=-1){
  print(paste('Fetching device type table, limit is ', limit))
  device_type_table <- get_device_type_table(db, limit)
  device_type_table <- collect (device_type_table)
  device_type_table$month.index <-  as.character(as.yearmon(device_type_table$time_start))
  return(device_type_table)
}

get_device_log_types_by_user <- function(db, limit=-1){
  print(paste('Fetching device log types table, limit is ', limit))
  logs <- get_device_log_table(db, limit)
  logs_by_type <- logs %.%
    group_by (log_type,user_id, user_pk, domain, month.index) %.% 
    summarise (num_logs = count(id))
  return(as.data.frame(collect(logs_by_type)))
}

get_user_type_table<- function(db, limit=-1) {
  user_tbl <- get_user_table(db)
  return(user_tbl)
}

get_salesforce_contract_table <- function(db, limit=-1){
  sf_tbl <- get_salesforce_contract_data(db)
  sf_tbl <- collect (sf_tbl)
  names(sf_tbl)[names(sf_tbl)=="dname"] <- "domain"
  return(sf_tbl)
}

get_salesforce_reportout_table <- function (db, limit = -1){
  sf_tbl <- get_salesforce_reportouts(db)
  sf_tbl <- collect (sf_tbl)
  names(sf_tbl)[names(sf_tbl)=="dname"] <- "domain"
  return(sf_tbl)
}