#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_nvisits_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_nvisits_1b = median(per_diff_nvisits, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_nvisits_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_active_day_percent_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_active_day_percent_1b = median(per_diff_active_day_percent, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_active_day_percent_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_nforms_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_nforms_1b = median(per_diff_nforms, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_nforms_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_median_visit_duration_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_median_visit_duration_1b = median(per_diff_median_visit_duration, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_median_visit_duration_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_median_visits_per_day_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_median_visits_per_day_1b = median(per_diff_median_visits_per_day, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_median_visits_per_day_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_time_using_cc_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_time_using_cc_1b = median(per_diff_time_using_cc, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_time_using_cc_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_ninteractions_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_ninteractions_1b = median(per_diff_ninteractions, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_ninteractions_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_ncases_registered_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_ncases_registered_1b = median(per_diff_ncases_registered, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_ncases_registered_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_register_followup_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_register_followup_1b = median(per_diff_register_followup, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_register_followup_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_case_register_followup_rate_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_case_register_followup_rate_1b = median(per_diff_case_register_followup_rate, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_case_register_followup_rate_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_ncases_touched_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_ncases_touched_1b = median(per_diff_ncases_touched, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_ncases_touched_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_nunique_followups_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_nunique_followups_1b = median(per_diff_nunique_followups, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_nunique_followups_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)

#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_sample_increase_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_sample_increase_1b = median(per_diff_sample_increase, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_sample_increase_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)


#Initialize dataframe
test_1_rel <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_rel) <- c("calendar_month", "med_sample_decrease_1b", "user_pk")
test_1_rel$calendar_month <- as.Date(test_1_rel$calendar_month)
test_1_rel$user_pk <- as.numeric(test_1_rel$user_pk)
users <- unique(training_consec$user_pk)

for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_sample_decrease_1b = median(per_diff_sample_decrease, na.rm = T),
              user_pk = i)
  test_1_rel <- rbind(test_1_rel, exclude_user_median)
}
test_1_rel$concat <- paste(test_1_rel$user_pk, test_1_rel$calendar_month, sep = "_") 
test_1_rel <- select(test_1_rel, concat, med_sample_decrease_1b)
training_consec <- merge(training_consec, test_1_rel, by = "concat", all.x = T)