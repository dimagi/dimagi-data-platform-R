#Final data for attrition study for Nisha and Reem
#Dec 4, 2014
#Utilization Ratio study (UR)
#Using blog data with 60,047 observations
ur_study <- read.csv(file = "blog_data.csv")
ur_study$calendar_month <- as.Date(ur_study$calendar_month)

#Prepare domain_table for merging
#Bring in sector information
sector <- tbl(db, "sector")
sector <- collect(sector)
names(sector)[names(sector) == "name"] = "sector_final"
domain_sector <- tbl(db, "domain_sector")
domain_sector <- collect(domain_sector)
domain_sector <- select(domain_sector, domain_id, sector_id)
domain_table <- merge(domain_table, domain_sector, by.x = "id", by.y = "domain_id", all.x = T)
domain_table <- merge(domain_table, sector, by.x = "sector_id", by.y = "id", all.x = T)
#Bring in subsector information
subsector <- tbl(db, "subsector")
subsector <- collect(subsector)
subsector <- select(subsector, id, name)
subsector <- filter(subsector, !is.na(name))
subsector <- filter(subsector, name != "")
names(subsector)[names(subsector) == "name"] = "subsector_final"
domain_subsector <- tbl(db, "domain_subsector")
domain_subsector <- collect(domain_subsector)
domain_subsector <- select(domain_subsector, domain_id, subsector_id)
domain_table <- merge(domain_table, domain_subsector, by.x = "id", by.y = "domain_id", all.x = T)
domain_table <- merge(domain_table, subsector, by.x = "subsector_id", by.y = "id", all.x = T)
#Consolidate country information
is.na(domain_table$deployment.country) <- domain_table$deployment.country == ""
is.na(domain_table$country) <- domain_table$country == ""
domain_table$country_final <- domain_table$deployment.country
keep_country <- which(is.na(domain_table$deployment.country) & !is.na(domain_table$country))
domain_table$country_final[keep_country] <- domain_table$country[keep_country]
#Consolidate Dimagi level of support
is.na(domain_table$internal.services) <- domain_table$internal.services == ""
is.na(domain_table$internal.self_started) <- domain_table$internal.self_started == ""
domain_table$self_start[domain_table$internal.self_started == "True"] <- "self"
domain_table$dimagi_services <- domain_table$internal.services
keep_self <- which(is.na(domain_table$internal.services) & !is.na(domain_table$self_start))
domain_table$dimagi_services[keep_self] <- domain_table$self_start[keep_self]

#Keep only columns of interest
names(domain_table)[names(domain_table) == "id"] = "domain_id"
facets_to_merge <- select(domain_table, name, domain_id, country_final, sector_final, 
                          subsector_final, dimagi_services, test)

#Merge to ur_study
ur_study <- merge(ur_study, facets_to_merge, by.x = "domain", 
                  by.y = "name", all.x = T)
ur_study <- filter(ur_study, sector_final == "Health")

#Exclude domains for EULA
exclude_domains <- read.csv(file = "can_use_true.csv")
exclude_domains <- exclude_domains$x 
exclude_domains <- exclude_domains[-c(2,3,4,13)]
exclude_domains <- append(as.character(exclude_domains), "ccdt")
ur_study <- filter(ur_study, !(domain %in% exclude_domains))

write.csv(ur_study, file = "ur_study_12_4_14.csv")




#------------------------------------------------------------------------#
#Older code
#------------------------------------------------------------------------#

ur_study <- monthly_table
ur_study <- ur_study[!(ur_study$user_id =="demo_user"),]
ur_study$keep_domain <- ur_study$domain %in% true_domains$name
ur_study <- ur_study[ur_study$keep_domain == T,]
ur_study <- merge(ur_study, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)
ur_study$domain_numeric <- as.numeric(as.factor(ur_study$domain))
#Need to convert user_id to user_numeric, but have a problem with duplicate
#user_ids from different domains 
ur_study$date_first_visit <- as.Date(ur_study$date_first_visit)
ur_study$date_last_visit <- as.Date(ur_study$date_last_visit)

ur_study <- select(ur_study, domain_numeric, user_id, month.index, numeric_index, 
                   date_first_visit, date_last_visit, active_days, summary_device_type,
                   country, Sector, Sub.Sector, Test.Project., active)

names(ur_study)[names(ur_study) == "active"] = "active_domain"
names(ur_study)[names(ur_study) == "month.index"] = "calendar_month"
names(ur_study)[names(ur_study) == "Sector"] = "sector"
names(ur_study)[names(ur_study) == "Sub.Sector"] = "subsector"
names(ur_study)[names(ur_study) == "Test.Project."] = "test_project"

#Convert calendar month to actual date
ur_study$calendar_month <- parse_date_time(paste('01', ur_study$calendar_month), '%d %b %Y!')
ur_study$calendar_month <- as.Date(ur_study$calendar_month)

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
ur_study <- arrange(ur_study, domain_numeric, user_id, calendar_month)
df <- data.table(ur_study)
#Can we setkey by domain and user_id since some user_ids might be the same?
setkey(df,user_id)
df[,diff_days:=c(NA,diff(calendar_month)),by=user_id]
ur_study <- as.data.frame(df)
ur_study$previous_month_active <- ur_study$diff_days <= 31
ur_study <- arrange(ur_study, domain_numeric, user_id, calendar_month)
#This following code is a problem due to duplicate user_id
users <- unique(ur_study$user_id)

next_month_active <- c()
for (i in users) {
  single_user <- ur_study[ur_study$user_id == i,]
  next_active <- c()
  next_active <- append(single_user$previous_month_active[-1], F)
  next_month_active <- append(next_month_active, next_active)
}
ur_study$next_month_active <- next_month_active
#If calendar_month = 8/1/14 then next_month_active = NA
#because we don't know if the user will be active in the following month
is.na(ur_study$next_month_active) <- ur_study$calendar_month == "2014-08-01"

ur_study <- select(ur_study, -(diff_days))

write.csv(ur_study, file = "attrition_study.csv")

#175 domains in total (only includes Test.Project. == F)
#119 domains have active == T and 56 have active == F