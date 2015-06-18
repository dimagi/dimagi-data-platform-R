
test <- domain
test$mvp <- NA
test$mvp[grep("mvp", test$name, fixed = T)] <- T
test <- filter(test, mvp == T)

test$has_amplifies_workers_app <- test$domain_id %in% filter(app_amplifies, amplifies_workers == T)$domain_id
test$has_amplifies_project_app <- test$domain_id %in% filter(app_amplifies, amplifies_project == T)$domain_id

#What is the app volume for all mvp domains?
app_volume <- form_table %>% group_by(application_id) %>% 
  summarise(nforms = length(unique(id)), 
            domain_id = unique(domain_id))

app_volume <- filter(app_volume, app_volume$domain_id %in% test$domain_id)
test2 <- merge(app_volume, select(domain, domain_id, name), by = "domain_id", all.x = T)
test2 <- filter(test2, nforms >= 10000)
test2 <- merge(test2, select(app_amplifies, id, app_id), by.x = "application_id", 
               by.y = "id", all.x = T)

#MVP apps with >10K forms submissions
test2 <- filter(test2, nforms >= 10000)
