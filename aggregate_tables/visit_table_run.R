library(dplyr)
library(zoo)
library(DBI)
library(RPostgreSQL)

source(file.path("aggregate_tables", "lifetime_func.R", fsep=.Platform$file.sep))

makeVisit <- function(db, dat) {
  # Formatting
  dat$visit_date <- as.Date(dat$time_start)
  dat$month.index <- as.yearmon(dat$visit_date) # obtaining year and month from Date

  # Sorting
  dat <- dat[order(dat$user_id, dat$time_start), ]  # sort visits by user_id and first interaction time
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

  dbWriteTable(db$con, value=as.data.frame(dat), name="visits", append=TRUE)
  return(data.frame(written=TRUE))
}

args <- commandArgs(trailingOnly=TRUE)
inpath <- args[1]
dbname <- args[2]

cat(paste("Reading interactions from:", inpath, "\n"))
interactions <- read.csv(inpath, header=TRUE, stringsAsFactors=FALSE, nrows=10000)
interactions$user_id[is.na(interactions$user_id)] <- "NONE"

cat(paste("Writing visits to database:", dbname, "\n"))
db <- src_postgres(dbname=dbname)
dbRemoveTable(db$con, name='visits')
makeVisit2 <- function(dat) makeVisit(db, dat)
quiet <- interactions %.% group_by(domain) %.% do(makeVisit2(.))
