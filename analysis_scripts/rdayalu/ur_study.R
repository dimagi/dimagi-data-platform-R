#Data for attrition study for Nisha and Reem
#Utilization Ratio study (UR)

true_domains <- filter(domain_table, Test.Project. == "false")
facets_to_merge <- select(domain_table, name, country, Sector, Sub.Sector,
                          business_unit, active, Test.Project.)

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