# Indicators in both the monthly and lifetime table.
date_first_visit <- function(x) min(x$visit_date, na.rm=TRUE)
date_last_visit <- function(x) max(x$visit_date, na.rm=TRUE)
nvisits <- function(x) nrow(x)
days_on_cc <- function(x) as.numeric(date_last_visit(x) - date_first_visit(x)) + 1
active_days <- function(x) length(unique(x$visit_date))
active_day_percent <- function(x) active_days(x) / days_on_cc(x)
## TODO: Right now this only considers days with at least on visit.
median_visits_per_day <- function(x) median(as.numeric(table(x$visit_date)))

nforms <- 'Number of total forms submitted by a mobile user'

median_visit_duration <- 'the median time spent (in minutes) on a visit by a mobile user'
median_time_elapsed_btw_visits <- 'median time elapsed between two continuous visits by a mobile user '
median_time_btw_followup <- 'median time (in mins) elapsed between two followup visits conducted by a mobile user'
median_days_btw_followup <- 'median days elapsed between two followup visits conducted by a mobile user'
batch_entry_visit <- 'visits with a starting time within less than 10mins after the previous visit. Both the previous and the current visits would be labeled as batch entry.'
batch_entry_percent <- 'percentage of batch entry visits in total visits conducted by a mobile user'
ncases_registered <- 'Number of total cases registered by a mobile user throughout the lifetime of a mobile user on CC'
register_followup <- 'Number of registered cases that are followed up by any mobile user (not necessarily have to be the mobile user who registers them)'
case_register_followup_rate <- 'Percentage of registered cases that are followed up at some point by a mobile user on CC'

morning <- 'Visits happening between 6am and 11:59am'
afternoon <- 'Visits happening between 12:00pm and 5:59pm'
evening <- 'Visits happening between 6pm and 12am'
after_midnight <- 'Visits happening between 12:01am and 5:59am'

cases_opened <- 'number of cases that are registered in this month'

## TODO: Some indicators do not fit into our current framework. For
## instance, the total number of cases that are not opened at some
## time point before this month and not closed yet until this month.

# Indicators in the lifetime table only.
days_visit_last <- function(x) as.numeric(difftime(Sys.Date(), date_last_visit(x), units = "days"))
active_user <- function(x) ifelse(days_visit_last(x) <= 30, 1, 0)
calendar_month_on_cc <- 'Number of complete months that a mobile user is actively using CommCare'
active_months <- 'Number of months that a mobile user has submitted at least 1 form'
active_month_percent <- 'percentage of months in which a mobile user is actively submitting forms'
median_visits_per_month <- 'median visits within a month by a mobile user '
