nvisits <- function(x) nrow(x)
first_visit_date <- function(x) min(x$visit_date)
last_visit_date <- function(x) max(x$visit_date)
days_on_cc <- function(x) as.numeric(last_visit_date(x) - first_visit_date(x)) + 1
