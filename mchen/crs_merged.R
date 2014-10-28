# total cases
length(unique(merged$case_id))

# total closed cases
get_open_close(merged)

# visit table to open cases only
merged_closed_case <- get_visits_closed_case(merged)
merged_open <- merged[-which(merged$case_id %in% merged_closed_case),]

# total inactive cases
merged_first <- get_first_visit(merged_open)
merged_last <- get_last_visit(merged_open)
merged_total_visits <- get_total_visits(merged_open)

merged_first <- merged_first[order(merged_first$case_id),]
merged_last <- merged_last[order(merged_last$case_id),]
merged_total_visits <- merged_total_visits[order(merged_total_visits$case_id),]
merged_first$last_visit <- merged_last$last_visit
merged_first$total_visits <- merged_total_visits$total_visits

merged_first$total_days <- as.numeric(as.Date(merged_first$last_visit) - as.Date(merged_first$first_visit))
avg_days_between_visits <- get_avg_days_elapsed(merged_open)
merged_first$avg_days_between_visits <- avg_days_between_visits$avg_days_elapsed_btw_visits
merged_first$inactive <- ifelse(as.Date(merged_first$last_visit) < get_inactive_line(export_date, 120), "yes", "no")
merged_first$inactive_60 <- ifelse(as.Date(merged_first$last_visit) < get_inactive_line(export_date, 60), "yes", "no")

table(merged_first$inactive); table(merged_first$inactive_60)
