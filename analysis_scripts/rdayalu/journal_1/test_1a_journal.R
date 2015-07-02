detach("package:data.table")

#Initialize dataframe
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_nvisits_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_nvisits_1a = median(diff_nvisits, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_nvisits_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_active_day_percent_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_active_day_percent_1a = median(diff_active_day_percent, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_active_day_percent_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_nforms_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_nforms_1a = median(diff_nforms, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_nforms_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_median_visit_duration_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_median_visit_duration_1a = median(diff_median_visit_duration, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_median_visit_duration_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_median_visits_per_day_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_median_visits_per_day_1a = median(diff_median_visits_per_day, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_median_visits_per_day_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_time_using_cc_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_time_using_cc_1a = median(diff_time_using_cc, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_time_using_cc_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_ninteractions_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_ninteractions_1a = median(diff_ninteractions, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_ninteractions_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)



#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_ncases_registered_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_ncases_registered_1a = median(diff_ncases_registered, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_ncases_registered_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_register_followup_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_register_followup_1a = median(diff_register_followup, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_register_followup_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_case_register_followup_rate_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_case_register_followup_rate_1a = median(diff_case_register_followup_rate, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_case_register_followup_rate_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_ncases_touched_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_ncases_touched_1a = median(diff_ncases_touched, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_ncases_touched_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_nunique_followups_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_nunique_followups_1a = median(diff_nunique_followups, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_nunique_followups_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)

#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_sample_increase_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_sample_increase_1a = median(diff_sample_increase, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_sample_increase_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)


#Initialize matrix
test_1_abs <- data.frame(matrix(ncol = 3, nrow = 1)) 
names(test_1_abs) <- c("calendar_month", "med_sample_decrease_1a", "user_pk")
test_1_abs$calendar_month <- as.Date(test_1_abs$calendar_month)
test_1_abs$user_pk <- as.numeric(test_1_abs$user_pk)
users <- unique(training_consec$user_pk)
for (i in users) {
  domain_id <- unique((filter(training_consec, user_pk == i))$domain)
  exclude_user <- filter(training_consec, user_pk != i & domain == domain_id)
  exclude_user_median <- exclude_user %.% 
    group_by(calendar_month) %.%
    summarise(med_sample_decrease_1a = median(diff_sample_decrease, na.rm = T),
              user_pk = i)
  test_1_abs <- rbind(test_1_abs, exclude_user_median)
}
test_1_abs$concat <- paste(test_1_abs$user_pk, test_1_abs$calendar_month, sep = "_") 
test_1_abs <- select(test_1_abs, concat, med_sample_decrease_1a)
training_consec <- merge(training_consec, test_1_abs, by = "concat", all.x = T)