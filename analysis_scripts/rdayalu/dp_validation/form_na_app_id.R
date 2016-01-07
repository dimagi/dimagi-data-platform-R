#Pull form_table from db
form_table <- tbl(db, "form")
form_table <- collect(form_table)

#Add domain name to form_table
domain <- get_domain_table(db)
names(domain)[names(domain) == "id"] = "domain_id"
form_table <- merge(form_table, select(domain, name, domain_id), by = "domain_id", all.x = T)

#Summary of domains by nforms and nforms with app_id = NA
domain_forms <- form_table %>% group_by(name) %>% 
  summarise(nforms = length(unique(id)), 
            nforms_app_id_na = length(unique(id[is.na(application_id)])))
domain_forms$per_app_id_na <- (domain_forms$nforms_app_id_na/domain_forms$nforms)*100
domain_forms$per_app_id_na <- round(domain_forms$per_app_id_na, digits = 2)
domain_forms <- arrange(domain_forms, desc(nforms))