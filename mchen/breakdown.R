x_first <- get_first_visit(x)
x_last <- get_last_visit(x)
x_total_visits <- get_total_visits(x)

x_first <- x_first[order(x_first$case_id),]
x_last <- x_last[order(x_last$case_id),]
x_total_visits <- x_total_visits[order(x_total_visits$case_id),]
x_last$first_visit <- x_first$first_visit
x_last$total_visits <- x_total_visits$total_visits

colnames(x_last)[5:7] <- c("last_visit_created", "last_visit_updated", "last_visit_closed")

x_last$touched_120 <- ifelse(as.Date(x_last$last_visit) > get_inactive_line(export_date, 120), "yes", "no")
x_last$touched_60 <- ifelse(as.Date(x_last$last_visit) > get_inactive_line(export_date, 60), "yes", "no")

x_last$total_days <- as.numeric(as.Date(x_last$last_visit) - as.Date(x_last$first_visit))
avg_days_between_visits <- get_avg_days_elapsed(x)
x_last$avg_days_between_visits <- round(avg_days_between_visits$avg_days_elapsed_btw_visits, digits = 1)



# WAR METRICS
# cases created in last 120 (first visit happened in last 120 days)
cases_created_120 <- which(as.Date(x_last$first_visit) > get_inactive_line(export_date, 120))

# cases closed in last 120
cases_closed_120 <- which(x_last$touched_120 == "yes" & x_last$last_visit_closed == "TRUE")

# active cases: cases touched within the date range
cases_touched <- which(x_last$touched_120 == "yes")

# total cases: cases that are open at some point during the date range 
total_cases <- length(unique(x$case_id)) - get_num_cases_closed_before_range(x, export_date, 120)

# % active case
fu_war <- round(length(cases_touched)/total_cases, digits = 2)

print(c(length(cases_created_120), length(cases_closed_120), length(cases_touched), total_cases, fu_war))


# PMP METRICS
# cases active in last 60: cases that are created or updated but not closed in last 60 days
cases_created_updated_60 <- length(which(x_last$touched_60 == "yes" & x_last$last_visit_closed == "FALSE"))

# active cases: cases that are created or updated but not closed in last 120
cases_created_updated_120 <- length(which(x_last$touched_120 == "yes" & x_last$last_visit_closed == "FALSE"))

# inactive cases: open cases that are untouched in last 120
cases_inactive <- length(which(x_last$touched_120 == "no" & x_last$last_visit_closed == "FALSE"))

# cases
length(unique(x$case_id))

# fu_rate
fu_pmp <- cases_created_updated_60/(cases_inactive + cases_created_updated_120)

print(c(cases_created_updated_60, cases_created_updated_120, cases_inactive, fu_pmp))

# cases that have lived longer than they are supposed to
cases_suspicious <- length(which(x_last$total_days > 270 & x_last$last_visit_closed == "FALSE"))


