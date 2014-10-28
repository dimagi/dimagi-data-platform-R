# total cases not created/followed up by demo_user (all cases, not by type)
merged <- read.csv("merged.csv", stringsAsFactors = FALSE)
merged <- merged[-which(merged$user_id == "demo_user"),]
length(unique(merged$case_id))

inactive_line <- get_inactive_line(export_date, 120)
baby <- get_case_data(merged_open, "baby")
baby_first <- get_first_visit(baby)
baby_last <- get_last_visit(baby)
baby_total_visits <- get_total_visits(baby)

# merge all tables having case_id as the primary key
baby_first <- baby_first[order(baby_first$case_id),]
baby_last <- baby_last[order(baby_last$case_id),]
baby_total_visits <- baby_total_visits[order(baby_total_visits$case_id),]
baby_first$last_visit <- baby_last$last_visit
baby_first$total_visits <- baby_total_visits$total_visits

# total days between first and last visit
baby_first$total_days <- as.numeric(as.Date(baby_first$last_visit) - as.Date(baby_first$first_visit))
avg_days_between_visits <- get_avg_days_elapsed(baby)
baby_first$avg_days_between_visits <- avg_days_between_visits$avg_days_elapsed_btw_visits
baby_first$inactive <- ifelse(as.Date(baby_first$last_visit) < get_inactive_line(export_date, 120), "yes", "no")
baby_first$inactive_60 <- ifelse(as.Date(baby_first$last_visit) < get_inactive_line(export_date, 60), "yes", "no")


# crosstab active/inactive
case_activity <- as.data.frame(table(baby_first$inactive))

# inactive subset
inactive_baby <- baby_first[which(baby_first$inactive == "yes"),]

# get owner_id from case_hq data: 
matched_case_seq <- which(case_hq$info.case_id %in% baby_first$case_id)
matched_case_owner <- subset(case_hq[matched_case_seq,], select = c(info.case_id, info.owner_id))
baby_with_owner <- merge(baby_first, matched_case_owner, by.x = "case_id", by.y = "info.case_id", all.y = T)

# baby_with_owner will be the subset to work on from here
table(baby_with_owner$inactive)

inactive_baby_with_owner <- baby_with_owner[which(baby_with_owner$inactive == "yes"),]
length(which(inactive_baby_with_owner$total_visits == 1)) # inactive baby cases with registration visit only 
length(which(inactive_baby_with_owner$closed == TRUE)) # inactive baby cases that have been closed
length(which(inactive_baby_with_owner$closed == TRUE && inactive_baby_with_owner$total_visits == 1))

avg_visits <- ddply(inactive_baby_with_owner, .(closed), function(x) mean(x$total_visits)) # average visits to closed/open inactive cases
avg_days <- ddply(inactive_baby_with_owner, .(closed), function(x) mean(x$total_days)) 



# get mobile user activity status
baby_with_owner <- baby_with_owner[order(baby_with_owner$user_id, baby_with_owner$last_visit),]
user_last_visit <- ddply(baby_with_owner, .(user_id), function(x) x[nrow(x),]) # first_visit in this dataset = the latest visit made by the user
user_last_visit$user_inactive <- ifelse(as.Date(user_last_visit$first_visit) - as.Date(inactive_line) > 120, "yes", "no")
colnames(user_last_visit)[8] <- c("case_last_visit")

# total inactive cases owned by each user
