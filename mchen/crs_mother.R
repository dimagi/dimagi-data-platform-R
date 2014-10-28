mother <- get_case_data(merged_open, "pregnant_mother")
mother_first <- get_first_visit(mother)
mother_last <- get_last_visit(mother)
mother_total_visits <- get_total_visits(mother)

# merge all tables having case_id as the primary key
mother_first <- mother_first[order(mother_first$case_id),]
mother_last <- mother_last[order(mother_last$case_id),]
mother_total_visits <- mother_total_visits[order(mother_total_visits$case_id),]
mother_first$last_visit <- mother_last$last_visit
mother_first$total_visits <- mother_total_visits$total_visits

# total days between first and last visit
mother_first$total_days <- as.numeric(as.Date(mother_first$last_visit) - as.Date(mother_first$first_visit))
avg_days_between_visits <- get_avg_days_elapsed(mother)
mother_first$avg_days_between_visits <- avg_days_between_visits$avg_days_elapsed_btw_visits
mother_first$inactive <- ifelse(as.Date(mother_first$last_visit) < get_inactive_line(export_date, 120), "yes", "no")
mother_first$inactive_60 <- ifelse(as.Date(mother_first$last_visit) < get_inactive_line(export_date, 60), "yes", "no")


# crosstab active/inactive
case_activity <- as.data.frame(table(mother_first$inactive))

# inactive subset
inactive_mother <- mother_first[which(mother_first$inactive == "yes"),]

# get owner_id from case export from HQ:
pregnant_mother <- read.csv("pregnant_mother.csv", stringsAsFactors = FALSE)
matched_case_seq <- which(pregnant_mother$info.case_id %in% mother_first$case_id)
matched_case_owner <- subset(pregnant_mother[matched_case_seq,], select = c(info.case_id, info.owner_id))
mother_with_owner <- merge(mother_first, matched_case_owner, by.x = "case_id", by.y = "info.case_id", all.y = T)

# baby_with_owner will be the subset to work on from here
table(mother_with_owner$inactive)

inactive_mother_with_owner <- mother_with_owner[which(mother_with_owner$inactive == "yes"),]
length(which(inactive_mother_with_owner$total_visits == 1)) # inactive baby cases with registration visit only 
length(which(inactive_mother_with_owner$closed == TRUE)) # inactive mother cases that have been closed
length(which(inactive_mother_with_owner$closed == TRUE && inactive_mother_with_owner$total_visits == 1))

avg_visits <- ddply(inactive_mother_with_owner, .(closed), function(x) mean(x$total_visits)) # average visits to closed/open inactive cases
avg_days <- ddply(inactive_mother_with_owner, .(closed), function(x) mean(x$total_days)) 
