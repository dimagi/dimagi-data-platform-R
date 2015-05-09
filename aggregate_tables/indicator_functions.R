## TODO: I think it would be good to switch over to lubridate to do
## all our date manipulations. We have some legacy code that uses
## zoo.
library(lubridate)
library(zoo)

# VISIT TABLE INDICATORS:
date_first_visit <- function(x) min(x$visit_date, na.rm=TRUE)
date_last_visit <- function(x) max(x$visit_date, na.rm=TRUE)
nvisits <- function(x) NROW(x)
median_visit_duration <- function(x) round(as.numeric(median((x$time_end - x$time_start)/ 60, na.rm=TRUE)), digits = 1)
time_using_cc <- function(x) sum(x$form_duration, na.rm = T)/60
median_visits_per_day <- function(x) median(as.numeric(table(x$visit_date)), na.rm=TRUE)
nvisits_travel <- function(x) sum(x$home_visit, na.rm=T)
nvisits_travel_batch <- function(x) sum(x$time_since_previous_hv/60<10, na.rm = T)

# Proportion of visits by time of day
morning <- function(x) mean(x$visit_time == 'morning')*100
afternoon <- function(x) mean(x$visit_time == 'afternoon')*100
evening <- function(x) mean(x$visit_time == 'night')*100
night <- function(x) mean(x$visit_time == 'after midnight')*100

# User's first, second, third... etc. month on CC
numeric_index <- function (x) {
  first_possible_visit_date <- as.POSIXct(strptime("2010-01-01 00:00:00", "%Y-%m-%d %H:%M:%S"))
  
  this_month <- as.POSIXct(format(min(x$time_start),"%Y-%m-01"), tz = "UTC")
  if (this_month < first_possible_visit_date) { return (1) }
  
  start_month <- as.POSIXct(format(min(x$user_start_date),"%Y-%m-01"), tz = "UTC")
  if (start_month < first_possible_visit_date) {start_month <- first_possible_visit_date}
  
  total_months <- length(seq(from=start_month, to=this_month, by='month'))
  return (total_months)
}

## The next indicators are only applicable for the lifetime table.
calendar_month_on_cc <- function(x) {
  first.month <- as.yearmon(date_first_visit(x))
  last.month <- as.yearmon(date_last_visit(x))
  nmonths <- 12 * as.numeric(last.month - first.month) + 1
  return(nmonths)
}
median_visits_per_month <- function(x) median(as.numeric(table(as.yearmon(x$visit_date))), na.rm=TRUE)

# INTERACTION TABLE INDICATORS:
ninteractions <- function(x) NROW(x)
ncases_registered <- function(x) sum(x$created, na.rm=TRUE)
register_followup <- function(x) sum(!x$created)
case_register_followup_rate <- function(x) mean(!x$created)*100

ncases_touched <- function(x) length(unique(x$case_id))
n_followups <- function(x) {
  stopifnot(!any(is.na(x$created)))
  stopifnot(all(x$created == 0 | x$created == 1))
  return(length(x$case_id[x$created == 0]))
}
nunique_followups <- function(x) {
  stopifnot(!any(is.na(x$created)))
  stopifnot(all(x$created == 0 | x$created == 1))
  return(length(unique(x$case_id[x$created == 0])))
}
median_days_btw_followup <- function(x) median(x$days_elapsed_case)

# DEVICE TYPE TABLE INDICATORS:
summary_device_type <- function (x) {
  if (length(unique(x$device)) == 1) {
    s <- paste(toupper(substring(x$device[1], 1,1)), substring(x$device[1], 2),
               sep="", collapse=" ")
    return (s)
  } else {
    return ('Multi')
  }
}
nforms <- function(x) NROW(x)
active_days <- function(x) length(unique(as.Date(x$time_start)))
active_months <- function(x) length(unique(x$month.index))

# DEVICE LOG TABLE INDICATORS:
total_logs <-function(x) sum(x$num_logs)
old_audio_plays <-function(x) if (NROW(x[x$log_type=='audio',]) > 0) sum(x[x$log_type=='audio',c('num_logs')]) else 0
network_warnings <-function(x) if (NROW(x[x$log_type=='warning-network',]) > 0) sum(x[x$log_type=='warning-network',c('num_logs')]) else 0

plays <- function(filetype) {
  function(x) {
    if (NROW(x[x$log_type=="media" & grepl("start", x$msg) & grepl(filetype, x$msg), ]) > 0) {
      sum(x[x$log_type=="media" & grepl("start", x$msg) & grepl(filetype, x$msg), c('num_logs')])
    } else {
      0
    }  
  }
}
audio_plays <- plays(".mp3")
video_plays <- function(x) plays(".3gp")(x) + plays(".mp4")(x)