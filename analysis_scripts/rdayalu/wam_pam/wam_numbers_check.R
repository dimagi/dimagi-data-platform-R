domain$domain_id[domain$name == "ssqh-cs"]

test <- filter(form_table, domain_id == 146)
test$received_date <- as.Date(substr(test$received_on, 1, 10))
test$received_month <- floor_date(test$received_date, "month")
test$start_date <- as.Date(substr(test$time_start, 1, 10))
test$start_month <- floor_date(test$start_date, "month")
test2 <- filter(test, received_month == "2015-05-01")
length(unique(test2$user_pk[test2$start_month == "2015-04-01"]))

test2 <- filter(test, start_month == "2015-04-01")
length(unique(test2$user_pk))

test3 <- filter(user_exclusion, domain == "ssqh-cs")

table_2_wam <- merge(table_2_wam, t2_apr_old, by = "domain", all = T)
table_2_wam$diff_apr <- table_2_wam$nusers - table_2_wam$nusers_apr_old
table_2_wam$diff_apr[is.na(table_2_wam$nusers_apr_old)] <- table_2_wam$nusers[is.na(table_2_wam$nusers_apr_old)]
table_2_wam <- arrange(table_2_wam, desc(diff_apr))