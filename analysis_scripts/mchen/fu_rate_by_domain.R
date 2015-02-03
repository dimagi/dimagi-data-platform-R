
# to be written into functions: for now it's 6 domains i will just do the split and export manually
care <- interactions_subset[which(interactions_subset$domain == "care-ecd"),]
aaharbaseline <- interactions_subset[which(interactions_subset$domain == "aaharbaseline"),]
myrada <- interactions_subset[which(interactions_subset$domain == "myrada"),]
spandan <- interactions_subset[which(interactions_subset$domain == "spandan"),]
rdi_hiht <- interactions_subset[which(interactions_subset$domain == "rdi-hiht"),]
crs_remind <- interactions_subset[which(interactions_subset$domain == "crs-remind"),]

write.csv(care, "care.csv")
write.csv(aaharbaseline, "aaharbaseline.csv")
write.csv(myrada, "myrada.csv")
write.csv(spandan, "spandan.csv")
write.csv(rdi_hiht, "rdi_hiht.csv")
write.csv(crs_remind, "crs_remind.csv")


# remove demo_user 
merged <- spandan[-which(spandan$user_id == "demo_user"),]

# nrow(merged) 

merged_first <- get_first_visit(merged)
merged_last <- get_last_visit(merged)
merged_total_visits <- get_total_visits(merged)

merged_first <- merged_first[order(merged_first$case_id),]
merged_last <- merged_last[order(merged_last$case_id),]
merged_total_visits <- merged_total_visits[order(merged_total_visits$case_id),]
merged_last$first_visit <- as.Date(merged_first$first_visit)
merged_last$total_visits <- merged_total_visits$total_visits

merged_last <- rename(merged_last, c("created" = "last_visit_created", "updated" = "last_visit_updated", "closed" = "last_visit_closed"))

merged_last$total_days <- as.numeric(as.Date(merged_last$last_visit) - as.Date(merged_last$first_visit))

merged_last$touched_120 <- ifelse(as.Date(merged_last$last_visit) > get_inactive_line(export_date, 120), "yes", "no")
merged_last$touched_60 <- ifelse(as.Date(merged_last$last_visit) > get_inactive_line(export_date, 60), "yes", "no")

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


# HQ FU-rate
# active cases: cases that are created or updated but not closed in last 120
cases_created_updated_120 <- length(which(merged_last$touched_120 == "yes" & merged_last$last_visit_closed == "FALSE"))

# inactive cases: open cases that are untouched in last 120
cases_inactive <- length(which(merged_last$touched_120 == "no" & merged_last$last_visit_closed == "FALSE"))

hq_fu_rate <- cases_created_updated_120/(cases_inactive + cases_created_updated_120)

# DP FU-rate
# cases touched / (cases touched + inactive cases)
dp_fu_rate <- length(cases_touched)/(length(cases_touched) + cases_inactive)




# breakdown by case type and their time on CommCare
merged_last_temp <- get_life_length(merged_last, export_date)
merged_last_temp <- get_age_range(merged_last_temp, 90, 180, 360)
table(merged_last_temp$age_range); nrow(merged_last_temp) 

cases_touched_by_type_age <- ddply(merged_last_temp, .(case_type, age_range), function(x)length(which(x$touched_120 == "yes")))
cases_inactive_by_type_age <- ddply(merged_last_temp, .(case_type, age_range), function(x)length(which(x$touched_120 == "no" & x$last_visit_closed == "FALSE")))
cases_closed_by_type_age <-  ddply(merged_last_temp, .(case_type, age_range), function(x)length(which(x$touched_120 == "yes" & x$last_visit_closed == "TRUE")))
cases_created_by_type_age <- ddply(merged_last_temp, .(case_type, age_range), function(x)length(which(as.Date(x$first_visit) > get_inactive_line(export_date, 120))))

cases_touched_by_type_age <- rename(cases_touched_by_type_age, c("V1" = "cases_touched")) 
cases_inactive_by_type_age <- rename(cases_inactive_by_type_age, c("V1" = "cases_inactive"))
cases_closed_by_type_age <- rename(cases_closed_by_type_age, c("V1" = "cases_closed"))
cases_created_by_type_age <- rename(cases_created_by_type_age, c("V1" = "cases_created"))


dp_fu <- merge(cases_touched_by_type_age, cases_inactive_by_type_age, by = c("case_type", "age_range"))
dp_fu$fu_rate <- round(dp_fu$cases_touched/(dp_fu$cases_touched + dp_fu$cases_inactive), digits = 3)

dp_fu <- merge(dp_fu, cases_closed_by_type_age, by = c("case_type", "age_range"))
dp_fu <- merge(dp_fu, cases_created_by_type_age, by = c("case_type", "age_range")) # to be written into a function merging multiple data frames
