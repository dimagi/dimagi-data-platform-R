# remove visits by demo_user
merged <- merged[-which(merged$user_id == "demo_user"),]

# total cases
length(unique(merged$case_id)) 

# total closed cases
get_open_close(merged)

# get case table with the following fields: 
  # visit_first, visit_last, 
  # visit_last_created, visit_last_updated, visit_last_closed, 
  # days elapsed between creation and last visit
  # average days elapsed between visits for a given case
  # touched_120, touched_60
merged_first <- get_first_visit(merged)
merged_last <- get_last_visit(merged)
merged_total_visits <- get_total_visits(merged)

merged_first <- merged_first[order(merged_first$case_id),]
merged_last <- merged_last[order(merged_last$case_id),]
merged_total_visits <- merged_total_visits[order(merged_total_visits$case_id),]
merged_last$first_visit <- merged_first$first_visit
merged_last$total_visits <- merged_total_visits$total_visits

colnames(merged_last)[5:7] <- c("last_visit_created", "last_visit_updated", "last_visit_closed")

merged_last$touched_120 <- ifelse(as.Date(merged_last$last_visit) > get_inactive_line(export_date, 120), "yes", "no")
merged_last$touched_60 <- ifelse(as.Date(merged_last$last_visit) > get_inactive_line(export_date, 60), "yes", "no")

merged_last$total_days <- as.numeric(as.Date(merged_last$last_visit) - as.Date(merged_last$first_visit))
avg_days_between_visits <- get_avg_days_elapsed(merged)
merged_last$avg_days_between_visits <- round(avg_days_between_visits$avg_days_elapsed_btw_visits, digits = 1)


# WAR METRICS
# cases created in last 120 (first visit happened in last 120 days)
cases_created_120 <- which(as.Date(merged_last$first_visit) > get_inactive_line(export_date, 120))

# cases closed in last 120
cases_closed_120 <- which(merged_last$touched_120 == "yes" & merged_last$last_visit_closed == "TRUE")

# active cases: cases touched within the date range
cases_touched <- which(merged_last$touched_120 == "yes")

# total cases: cases that are open at some point during the date range 
total_cases <- length(unique(merged$case_id)) - get_num_cases_closed_before_range(merged, export_date, 120)

# % active case
fu_war <- round(length(cases_touched)/total_cases, digits = 2)

print(c(length(cases_created_120), length(cases_closed_120), length(cases_touched), total_cases, fu_war))


# PMP METRICS
# cases active in last 60: cases that are created or updated but not closed in last 60 days
cases_created_updated_60 <- length(which(merged_last$touched_60 == "yes" & merged_last$last_visit_closed == "FALSE"))

# active cases: cases that are created or updated but not closed in last 120
cases_created_updated_120 <- length(which(merged_last$touched_120 == "yes" & merged_last$last_visit_closed == "FALSE"))

# inactive cases: open cases that are untouched in last 120
cases_inactive <- length(which(merged_last$touched_120 == "no" & merged_last$last_visit_closed == "FALSE"))

# cases
length(unique(merged$case_id))

# fu_rate
fu_pmp <- cases_created_updated_60/(cases_inactive + cases_created_updated_120)






# cases that are currently open but not touched in last 120
cases_inactive_120 <- which(merged_last$touched_120 == "no" & merged_last$last_visit_closed == "FALSE")

# visit table to open cases only
merged_closed_case <- get_visits_closed_case(merged)
merged_open <- merged[-which(merged$case_id %in% merged_closed_case),]

# remove cases closed before the inactive line
count_cases_open_120 <- length(which(merged_last$last_visit_closed == "FALSE"))
cases_closed_old <- which(merged_last$touched_120 == "no" & merged_last$last_visit_closed == "TRUE")


