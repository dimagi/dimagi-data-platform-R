## TODO: I think it would be good to switch over to lubridate to do
## all our date manipulations. We have some legacy code that uses
## zoo.
library(lubridate)
library(zoo)

# INTERACTION TABLE INDICATORS:
date_first_visit <- function(x) min(x$visit_date, na.rm=TRUE)
date_last_visit <- function(x) max(x$visit_date, na.rm=TRUE)
#The next indicator (days_on_cc) only makes sense for the lifetime table. We don't need it here.
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


# VISIT TABLE INDICATORS:
# removes all case-specific columns from the interactions table, then returns one row per visit
# TODO add in batch entry and date difference once these are correct i.e one value for every visit
unique_visits <- function (x) {
  df <- subset(x, select = -c(case_id,days_elapsed_case,new_case,follow_up,date_difference,batch_entry))
  return (unique(df))
}
  
nvisits <- function(x) NROW(unique_visits(x))
nforms <- function(x) sum((unique_visits(x))$total_forms, na.rm=TRUE)
median_visit_duration <- function(x) as.numeric(median(((unique_visits(x))$time_end - (unique_visits(x))$time_start)/ 60, na.rm=TRUE))
time_using_cc <- function(x) sum((unique_visits(x))$form_duration, na.rm = T)
median_visits_per_day <- function(x) median(as.numeric(table((unique_visits(x))$visit_date)), na.rm=TRUE)

## These next indicators are only applicable for the lifetime table.
median_visits_per_month <- function(x) median(as.numeric(table(as.yearmon((unique_visits(x))$visit_date))), na.rm=TRUE)

# (I am not sure what this next indicator is doing - RD)
median_time_elapsed_btw_visits <- function(x) median((unique_visits(x))$time_since_previous, na.rm=TRUE)

## To get reliable follow-up rates we'll need the full lifetime
## data. Additionally, can the case be followed-up by a different FLW?
## I'm not sure how to interpret this indicator description: "median
## time (in mins) elapsed between two followup visits conducted by a
## mobile user"
## I took these two indicators out of the config file because they
## were running too slowly. - Andrew?

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

#This should be calculated based on visit table
# TODO batch entry calc must be fixed first - ML
batch_entry_visit <- function(x) sum(x$batch_entry, na.rm=TRUE)
batch_entry_percent <- function(x) mean(x$batch_entry, na.rm=TRUE)

#This should be calculated based on interaction table
ncases_registered <- function(x) sum(x$new_case, na.rm=TRUE)
register_followup <- function(x) sum(x$follow_up)
case_register_followup_rate <- function(x) mean(x$follow_up)

morning <- function(x) mean(x$visit_time == 'morning')
afternoon <- function(x) mean(x$visit_time == 'afternoon')
night <- function(x) mean(x$visit_time == 'night')
after_midnight <- function(x) mean(x$visit_time == 'after midnight')

ncases_opened <- function(x) sum(x$new_case)

first_possible_visit_date <- as.POSIXct(strptime("2010-01-01 00:00:00", "%Y-%m-%d %H:%M:%S"))
numeric_index <- function (x) {
  start_month <- as.POSIXct(format(min(x$user_start_date),"%Y-%m-01"), tz = "UTC")
  if (start_month < first_possible_visit_date) {start_month <- first_possible_visit_date}
  this_month <- as.POSIXct(format(min(x$time_start),"%Y-%m-01"), tz = "UTC")
  total_months <- length(seq(from=start_month, to=this_month, by='month'))
  return (total_months)
}

## TODO: Some indicators do not fit into our current framework. For
## instance, the total number of cases that are not opened at some
## time point before this month and not closed yet until this month.
## cum_open_cases: total number of cases that are not opened at some
## time point before this month and not closed yet until this month

ncases_touched <- function(x) length(unique(x$case_id))
nunique_followups <- function(x) {
    stopifnot(!any(is.na(x$follow_up)))
    stopifnot(all(x$follow_up == 0 | x$follow_up == 1))
    return(length(x$case_id[x$follow_up == 1]))
}

summary_device_type <- function (x) {
  if (length(unique(x$device)) == 1) {
    s <- paste(toupper(substring(x$device[1], 1,1)), substring(x$device[1], 2),
               sep="", collapse=" ")
    return (s)
  } else {
    return ('Multi')
  }
}

