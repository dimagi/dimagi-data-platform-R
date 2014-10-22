## TODO: I think it would be good to switch over to lubridate to do
## all our date manipulations. We have some legacy code that uses
## zoo.
library(lubridate)
library(zoo)

# VISIT TABLE INDICATORS:
date_first_visit <- function(x) min(x$visit_date, na.rm=TRUE)
date_last_visit <- function(x) max(x$visit_date, na.rm=TRUE)
days_on_cc <- function(x) as.numeric(date_last_visit(x) - date_first_visit(x)) + 1
active_days <- function(x) length(unique(x$visit_date))
active_day_percent <- function(x) active_days(x) / days_in_month(date_first_visit(x))

## The next indicators are only applicable for the lifetime table.
days_visit_last <- function(x) as.numeric(Sys.Date() - date_last_visit(x))
active_user <- function(x) ifelse(days_visit_last(x) <= 30, 1, 0)
calendar_month_on_cc <- function(x) {
    first.month <- as.yearmon(date_first_visit(x))
    last.month <- as.yearmon(date_last_visit(x))
    nmonths <- 12 * as.numeric(last.month - first.month) + 1
    return(nmonths)
}
active_months <- function(x) length(unique(as.yearmon(x$visit_date)))
active_month_percent <- function(x) active_months(x) / calendar_month_on_cc(x)
nvisits <- function(x) NROW(x)
nforms <- function(x) sum(x$total_forms, na.rm=TRUE)
median_visit_duration <- function(x) as.numeric(median((x$time_end - x$time_start)/ 60, na.rm=TRUE))
time_using_cc <- function(x) sum(x$form_duration, na.rm = T)
median_visits_per_day <- function(x) median(as.numeric(table(x$visit_date)), na.rm=TRUE)

## These next indicators are only applicable for the lifetime table.
median_visits_per_month <- function(x) median(as.numeric(table(as.yearmon(x$visit_date))), na.rm=TRUE)

# median time since previous visit (to any case)
median_time_elapsed_btw_visits <- function(x) median(x$time_since_previous, na.rm=TRUE)

#This should be calculated by case id, so calculated based on interaction table
median_time_btw_followup <- function(x) {
    f <- function(block) {
        sorted.times <- sort(ymd_hms(block$time_start))
        value <- ifelse(
            nrow(block) <= 1,
            NA,
            difftime(sorted.times[2], sorted.times[1], units='mins')
            )
        return(data.frame(followup_time=value))
    }
    times <- x %.% group_by(case_id) %.% do(f(.))
    return(median(times$followup_time, na.rm=TRUE))
}
median_days_btw_followup <- function(x) median_time_btw_followup(x) / 60 / 24

# TODO batch entry calc must be fixed, should take into account home visit or no - ML
batch_entry_visit <- function(x) sum(x$batch_entry, na.rm=TRUE)
batch_entry_percent <- function(x) mean(x$batch_entry, na.rm=TRUE)

morning <- function(x) mean(x$visit_time == 'morning')
afternoon <- function(x) mean(x$visit_time == 'afternoon')
night <- function(x) mean(x$visit_time == 'night')
after_midnight <- function(x) mean(x$visit_time == 'after midnight')

numeric_index <- function (x) {
  first_possible_visit_date <- as.POSIXct(strptime("2010-01-01 00:00:00", "%Y-%m-%d %H:%M:%S"))
  
  this_month <- as.POSIXct(format(min(x$time_start),"%Y-%m-01"), tz = "UTC")
  if (this_month < first_possible_visit_date) { return (1) }
  
  start_month <- as.POSIXct(format(min(x$user_start_date),"%Y-%m-01"), tz = "UTC")
  if (start_month < first_possible_visit_date) {start_month <- first_possible_visit_date}
  
  total_months <- length(seq(from=start_month, to=this_month, by='month'))
  return (total_months)
}

# INTERACTION TABLE INDICATORS:
ncases_registered <- function(x) sum(x$new_case, na.rm=TRUE)
register_followup <- function(x) sum(x$follow_up)
case_register_followup_rate <- function(x) mean(x$follow_up)
ncases_opened <- function(x) sum(x$new_case)
ncases_touched <- function(x) length(unique(x$case_id))
nunique_followups <- function(x) {
  stopifnot(!any(is.na(x$follow_up)))
  stopifnot(all(x$follow_up == 0 | x$follow_up == 1))
  return(length(x$case_id[x$follow_up == 1]))
}

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

