referral <- get_case_data(merged_open, "referral_followup")
referral_first <- get_first_visit(referral)
referral_last <- get_last_visit(referral)
referral_total_visits <- get_total_visits(referral)

# merge all tables having case_id as the primary key
referral_first <- referral_first[order(referral_first$case_id),]
referral_last <- referral_last[order(referral_last$case_id),]
referral_total_visits <- referral_total_visits[order(referral_total_visits$case_id),]
referral_first$last_visit <- referral_last$last_visit
referral_first$total_visits <- referral_total_visits$total_visits

# total days between first and last visit
referral_first$total_days <- as.numeric(as.Date(referral_first$last_visit) - as.Date(referral_first$first_visit))
avg_days_between_visits <- get_avg_days_elapsed(referral)
referral_first$avg_days_between_visits <- avg_days_between_visits$avg_days_elapsed_btw_visits
referral_first$inactive <- ifelse(as.Date(referral_first$last_visit) < get_inactive_line(export_date, 120), "yes", "no")
referral_first$inactive_60 <- ifelse(as.Date(referral_first$last_visit) < get_inactive_line(export_date, 60), "yes", "no")


# crosstab active/inactive
case_activity <- as.data.frame(table(referral_first$inactive))

# inactive subset
inactive_referral <- referral_first[which(referral_first$inactive == "yes"),]

# get owner_id from case export from HQ:
referral <- read.csv("referral_followup.csv", stringsAsFactors = FALSE)
matched_case_seq <- which(referral$info.case_id %in% referral_first$case_id)
matched_case_owner <- subset(referral[matched_case_seq,], select = c(info.case_id, info.owner_id))
referral_with_owner <- merge(referral_first, matched_case_owner, by.x = "case_id", by.y = "info.case_id", all.y = T)

# baby_with_owner will be the subset to work on from here
table(referral_with_owner$inactive)

inactive_referral_with_owner <- referral_with_owner[which(referral_with_owner$inactive == "yes"),]
length(which(inactive_referral_with_owner$total_visits == 1)) # inactive baby cases with registration visit only 
length(which(inactive_referral_with_owner$closed == TRUE)) # inactive baby cases that have been closed
length(which(inactive_referral_with_owner$closed == TRUE && inactive_referral_with_owner$total_visits == 1))

avg_visits <- ddply(inactive_referral_with_owner, .(closed), function(x) mean(x$total_visits)) # average visits to closed/open inactive cases
avg_days <- ddply(inactive_referral_with_owner, .(closed), function(x) mean(x$total_days)) 



# get mobile user activity status
mother_with_owner <- mother_with_owner[order(mother_with_owner$user_id, mother_with_owner$last_visit),]
user_last_visit <- ddply(mother_with_owner, .(user_id), function(x) x[nrow(x),]) # first_visit in this dataset = the latest visit made by the user
user_last_visit$user_inactive <- ifelse(as.Date(user_last_visit$first_visit) - as.Date(inactive_line) > 120, "yes", "no")
colnames(user_last_visit)[8] <- c("case_last_visit")

# total inactive cases owned by each user
