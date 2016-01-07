#Which domains have forms in the DP but do not have monthly rows?
all_monthly <- test
domain <- get_domain_table(db)
domain <- select(domain, id, name, X..Active.Cases, X..Active.Mobile.Workers, 
                 is_test, internal.can_use_data)

form_table$form_date <- as.Date(substr(form_table$time_start, 1, 10))


domains_with_forms <- form_table %>% group_by(domain_id) %>% 
  summarise(nforms = length(unique(id)), 
            wam_domain = sum(amplifies_workers, na.rm=T) > 0,
            earliest_form_date = min(form_date))
domains_with_forms <- merge(domains_with_forms, domain, 
                            by.x = "domain_id", by.y = "id", all.x = T)
domains_with_forms$has_monthly_rows <- domains_with_forms$name %in% all_monthly$domain
test <- filter(domains_with_forms, has_monthly_rows == F)
test <- arrange(test, earliest_form_date)

test2 <- filter(test, wam_domain == T)
write.csv(test, file="domains_without_monthly_rows.csv")

