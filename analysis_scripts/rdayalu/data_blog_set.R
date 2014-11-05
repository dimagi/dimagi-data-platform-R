# Import monthly_table for all test = F domains


library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate)

#------------------------------------------------------------------------#
#DATA MANAGEMENT
#------------------------------------------------------------------------#

all_monthly <- monthly_table

#Set report_options
report = run_conf$reports$modules$name
report_options <- get_report_options(run_conf,report)

#Remove demo users and NA/NONE users
#We also need to find a way to exclude admin/web users
all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
all_monthly = all_monthly[!(all_monthly$user_id =="NONE"),]
all_monthly = all_monthly[!(all_monthly$user_id =="none"),]
all_monthly = all_monthly[!is.na(all_monthly$user_id),]

#Remove any dates before report start_date and after report end_date
all_monthly$date_first_visit = as.Date(all_monthly$date_first_visit)
all_monthly$date_last_visit = as.Date(all_monthly$date_last_visit)
start_date = as.Date(report_options$start_date)
end_date = as.Date(report_options$end_date)
all_monthly = subset(all_monthly, all_monthly$date_first_visit >= start_date
                     & all_monthly$date_last_visit <= end_date)

#Change column names as needed
names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"

#Round median visit duration to one decimal place
all_monthly$median_visit_duration <- round(all_monthly$median_visit_duration,
                                           digits = 1)
# Convert relevant indicators to percentages
all_monthly$active_day_percent= (all_monthly$active_day_percent)*100

#Merge domain facets from domain table into all_monthly table
facets_to_merge <- select(domain_table, name, country, Sector, Sub.Sector,
                          business_unit, active, Test.Project.)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

# Check # and % of users per domain
nusers <- all_monthly %>% group_by(domain) %>% summarise(n_users = length(unique(user_pk)))
nusers$per_users <- (nusers$n_users/length(unique(all_monthly$user_pk)))*100

#Check lifetime table for nunique_followups
lifetime_table <- get_aggregate_table(db, "aggregate_lifetime_interactions", domains_for_run)
lifetime_table <- lifetime_table[lifetime_table$domain %in% all_monthly$domain,]

length(unique(lifetime_table$domain))
table(lifetime_table$nunique_followups, useNA = "always")
table(lifetime_table$ncases_registered, useNA = "always")

lifetime_table$zero_reg <- lifetime_table$ncases_registered == 0
lifetime_table$one_reg <- lifetime_table$ncases_registered == 1
lifetime_table$two_reg <- lifetime_table$ncases_registered == 2
lifetime_table$three_plus_reg <- lifetime_table$ncases_registered >= 3
lifetime_table$na_reg <- is.na(lifetime_table$ncases_registered)

lifetime_table$zero_fu <- lifetime_table$nunique_followups == 0
lifetime_table$one_fu <- lifetime_table$nunique_followups == 1
lifetime_table$two_fu <- lifetime_table$nunique_followups == 2
lifetime_table$three_plus_fu <- lifetime_table$nunique_followups >= 3
lifetime_table$na_fu <- is.na(lifetime_table$nunique_followups)

nusers_lifetime <- lifetime_table %>% group_by(domain) %>% 
  summarise(nusers_zero_reg = sum(zero_reg, na.rm = T)/length(unique(user_pk))*100,
            nusers_one_reg = sum(one_reg, na.rm = T)/length(unique(user_pk))*100,
            nusers_two_reg = sum(two_reg, na.rm = T)/length(unique(user_pk))*100,
            nusers_three_plus_reg = sum(three_plus_reg, na.rm = T)/length(unique(user_pk))*100,
            nusers_na_reg = sum(na_reg, na.rm = T)/length(unique(user_pk))*100,
            nusers_zero_fu = sum(zero_fu, na.rm = T)/length(unique(user_pk))*100,
            nusers_one_fu = sum(one_fu, na.rm = T)/length(unique(user_pk))*100,
            nusers_two_fu = sum(two_fu, na.rm = T)/length(unique(user_pk))*100,
            nusers_three_plus_fu = sum(three_plus_fu, na.rm = T)/length(unique(user_pk))*100,
            nusers_na_fu = sum(na_fu, na.rm = T)/length(unique(user_pk))*100,
            nusers = length(unique(user_pk)))

nusers_lifetime <- arrange(nusers_lifetime, desc(nusers))

