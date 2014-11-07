#The purpose of this code is to create a standard dataset that people can use for 
#the "Under the Data Tree" data blog series

#First import monthly_table for all test = F domains then run the following code

library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate)
library(ggplot2)

#------------------------------------------------------------------------#
#DATA MANAGEMENT
#------------------------------------------------------------------------#

#Import mobile and web user lists
#Note that we need to merge/select by user_id instead of user_pk for now because we 
#don't currently have complete/correct user_pk for all user_ids. Melissa will be changing this.
#The mobile user table has duplicate user_ids only for demo_user, ntest, etc.
#Otherwise, all user_ids are unique. Dedup this anyway.
mobile_users <- read.csv(file = "mobile_users.csv", na.strings = "") 
mobile_users <- unique(mobile_users)

#The web_user table does have duplicate user_ids, but since user_pk is different for those users
#(even though we know they are the same users), we need to exclude user_pk and dedup web_users
web_users <- read.csv(file = "web_users1.csv", na.strings = "")
web_users <- select(web_users, user_id, username, is_dimagi)
web_users <- unique(web_users)

summary(mobile_users$user_id %in% web_users$user_id)
summary(all_monthly$user_id %in% web_users$user_id)
summary(all_monthly$user_id %in% mobile_users$user_id)

#All users in mobile_users are true mobile users
mobile_users$user_type <- "mobile"

#All users in web_users are web | dimagi users
web_users$user_type[web_users$is_dimagi == "t"] <- "dimagi"
web_users$user_type[web_users$is_dimagi == "f"] <- "web"
web_users <- select(web_users, user_id, username, user_type)

#Combine into one dataset
user_type <- rbind(mobile_users, web_users)

#Create aggregate monthly data set
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

#Modify relevant variables
all_monthly$active_day_percent= (all_monthly$active_day_percent)*100
all_monthly$domain_numeric = as.numeric(as.factor(all_monthly$domain))
all_monthly$median_visit_duration <- round(all_monthly$median_visit_duration,
                                           digits = 1)

#Merge domain facets from domain table into all_monthly table
facets_to_merge <- select(domain_table, name, country, Sector, Sub.Sector,
                          business_unit, active, Test.Project.)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

#Keep only mobile/unknown users
all_monthly <- merge(all_monthly, user_type, by = "user_id", all.x = T)
all_monthly$user_type[is.na(all_monthly$user_type)] <- "unknown"
all_monthly <- filter(all_monthly, user_type == "mobile" | user_type == "unknown")

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
all_monthly <- arrange(all_monthly, domain_numeric, user_pk, calendar_month)
df <- data.table(all_monthly)
#Can we setkey by domain and user_id since some user_ids might be the same?
setkey(df,user_pk)
df[,diff_days:=c(NA,diff(calendar_month)),by=user_pk]
all_monthly <- as.data.frame(df)
all_monthly$previous_month_active <- all_monthly$diff_days <= 31

users <- unique(all_monthly$user_pk)

next_month_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_pk == i,]
  next_active <- c()
  next_active <- append(single_user$previous_month_active[-1], F)
  next_month_active <- append(next_month_active, next_active)
}
all_monthly$next_month_active <- next_month_active

#If calendar_month = 10/1/14 then next_month_active = NA
#because we don't know if the user will be active in the following month
is.na(all_monthly$next_month_active) <- all_monthly$calendar_month == "2014-10-01"

#Get lifetime table for total nunique_followups, active_months per user
lifetime_table <- get_aggregate_table(db, "aggregate_lifetime_interactions", domains_for_run)
lifetime_table <- lifetime_table[lifetime_table$user_pk %in% all_monthly$user_pk,]

#Merge nunique_followups, active_months to all_monthly
lifetime_table <- select(lifetime_table, user_pk, nunique_followups, active_months)
names(lifetime_table)[names(lifetime_table) == "nunique_followups"] = "lifetime_followup"
all_monthly <- merge(all_monthly, lifetime_table, by = "user_pk", all.x = T)
#----------------------------------------------------------------------#
#Random analysis code
#----------------------------------------------------------------------#

# Check # and % of users per domain
nusers <- all_monthly %>% group_by(domain) %>% summarise(n_users = length(unique(user_pk)))
nusers$per_users <- (nusers$n_users/length(unique(all_monthly$user_pk)))*100
nusers <- arrange(nusers, desc(per_users))

#Frequency of nunique_followups for all relevant users 
table(lifetime_table$lifetime_followup, useNA = "always")

myhist <- ggplot(lifetime_table, aes(x=lifetime_followup)) + 
  geom_histogram(binwidth=1, colour="black", fill="lightblue") +
  scale_x_continuous(limits=c(0,25)) +
  geom_vline(aes(xintercept=median(lifetime_followup, na.rm=T)),
             color="red", linetype="dashed", size=1)

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

