#The purpose of this code is to create a standard dataset that people can use for 
#the "Under the Data Tree" data blog series

#First import monthly_table for all test = F domains then run the following code
#Be sure to set config_run first
source(file.path("analysis_scripts","raw_data","data_import.R", fsep = .Platform$file.sep))


library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate)
library(ggplot2)

#------------------------------------------------------------------------#
#DATA MANAGEMENT
#------------------------------------------------------------------------#

# Load system config file
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))

# Get db connection
# db <- get_db_connection(system_conf)

#List of users by user_type, keeping only mobile users
#Get user_type table from db (mobile, web, superuser, etc.)
user_type <- get_user_type_table(db)
user_type <- filter(user_type, user_type == "mobile")
user_type <- select(user_type, user_pk, user_type)

#Create aggregate monthly data set
all_monthly <- monthly_table

#Merge these two lists together, keeping only mobile users
all_monthly <- merge(all_monthly, user_type, by = "user_pk", all.x = T)
all_monthly <- filter(all_monthly, user_type == "mobile")

#Set report_options
report <- run_conf$reports$modules$name
report_options <- get_report_options(run_conf,report)

#Remove demo users and NA/NONE users
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
report_end_date <- as.Date(report_options$end_date)
end_month <- as.yearmon(report_end_date)
end_month <- parse_date_time(paste('01', end_month), '%d %b %Y!')
end_month <- as.Date(end_month)

#Bring in domain_numeric
#domain <- tbl(db, "domain")
#domain <- collect(domain)
#domain <- select(domain, id, name)
#write.csv(domain, file = "domain_master_list.csv")

#Merge domain ID into all_monthly table
#all_monthly <- merge(all_monthly, domain, by.x = "domain", 
#                     by.y = "name", all.x = T)

#Merge domain facets into all_monthly table
#Prepare domain_table for merging in domain facets
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

#Merge domain facets from domain table into all_monthly table
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#Change column names as needed
names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names (all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
#names (all_monthly)[names(all_monthly) == "id"] = "domain_numeric"
names(all_monthly)[names(all_monthly) == "domain_id"] = "domain_numeric"

#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

#Exclude any users who logged > 100 visits in any month
#These are probably atypical users
all_monthly$visits_ge_100 <- all_monthly$nvisits > 100
user_ge_100 <- all_monthly %.%
  group_by(user_id) %.%
  summarise(ge_100 = sum(visits_ge_100))
user_le_100 <- filter(user_ge_100, ge_100 == 0)
all_monthly <- all_monthly[all_monthly$user_id %in% user_le_100$user_id, ]

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
all_monthly <- arrange(all_monthly, domain_numeric, user_pk, calendar_month)
df <- data.table(all_monthly)
setkey(df,user_pk)
df[,diff_days:=c(NA,diff(calendar_month)),by=user_pk]
all_monthly <- as.data.frame(df)
all_monthly$previous_month_active <- all_monthly$diff_days <= 31
all_monthly$previous_two_months_active <- all_monthly$diff_days <= 62
all_monthly$previous_three_months_active <- all_monthly$diff_days <= 93

#We are working with user_pk/domain combination since user_pk
#might not be unique across domains. A single user_pk can submit to multiple domains. 
all_monthly$domain_user <- paste(all_monthly$domain, all_monthly$user_pk, sep = "_") 
users <- unique(all_monthly$domain_user)

next_month_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$domain_user == i,]
  next_active <- c()
  next_active <- c(single_user$previous_month_active[-1], F)
  next_month_active <- c(next_month_active, next_active)
}
all_monthly$next_month_active <- next_month_active

next_two_months_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$domain_user == i,]
  next_active <- c()
  next_active <- c(single_user$previous_two_months_active[-1], F)
  next_two_months_active <- c(next_two_months_active, next_active)
}
all_monthly$next_two_months_active <- next_two_months_active

next_three_months_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$domain_user == i,]
  next_active <- c()
  next_active <- c(single_user$previous_three_months_active[-1], F)
  next_three_months_active <- c(next_three_months_active, next_active)
}
all_monthly$next_three_months_active <- next_three_months_active

#Based on the end_month in our dataset, we don't know if the user will be active in any of
#the months following end_month. Must change all those attrition values to NA. 
is.na(all_monthly$next_month_active) <- all_monthly$calendar_month == end_month
is.na(all_monthly$next_two_months_active) <- all_monthly$calendar_month >= end_month - months(1) 
is.na(all_monthly$next_three_months_active) <- all_monthly$calendar_month >= end_month - months(2)

#Get lifetime table for total nunique_followups, active_months per user
lifetime_table <- get_aggregate_table(db, "aggregate_lifetime_interactions", domains_for_run)
lifetime_table <- lifetime_table[lifetime_table$user_pk %in% all_monthly$user_pk,]

#Merge nunique_followups, active_months to all_monthly
lifetime_table <- select(lifetime_table, user_pk, nunique_followups, active_months, calendar_month_on_cc)
names(lifetime_table)[names(lifetime_table) == "nunique_followups"] = "lifetime_followup"
names(lifetime_table)[names(lifetime_table) == "calendar_month_on_cc"] = "months_on_cc"
all_monthly <- merge(all_monthly, lifetime_table, by = "user_pk", all.x = T)

#Lifetime aggregate table is not available on the db as of 2/17/15.
#I will calculate months_on_cc, active_months here until the lifetime table is available.
total_months_cc <- all_monthly %>% group_by(domain_user) %>% 
  summarise(first_month = min(calendar_month),
            last_month = max(calendar_month), 
            active_months = length(unique(calendar_month)))
total_months_cc$months_on_cc <- (interval(total_months_cc$first_month, 
                                          total_months_cc$last_month) %/% months(1))+1
total_months_cc <- select(total_months_cc, domain_user, active_months, months_on_cc)
all_monthly <- merge(all_monthly, total_months_cc, by = "domain_user", all.x = T)

#Exclude a samples of users (from pradan-mis-dev?) so that each domain contributes only 
#< 5% of the total users
nusers <- all_monthly %>% group_by(domain) %>% summarise(nusers = length(unique(user_pk)))
nusers <- arrange(nusers, desc(nusers))
nusers$total_users <- sum(nusers$nusers)
nusers$per_users <- (nusers$nusers/nusers$total_users)*100
exclude_users_pathfinder <- sample(unique(all_monthly$user_pk[all_monthly$domain == "pradan-mis-dev"]), 299)
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude_users_pathfinder),]

#Was the user ever active again after an attrition event (defined as next_month_active == F)?
all_monthly$attrition_event <- !(all_monthly$next_month_active == T | is.na(all_monthly$next_month_active))
all_monthly$continuing <- all_monthly$month_index < all_monthly$months_on_cc
all_monthly$ever_active_again <- all_monthly$attrition_event == T & all_monthly$continuing == T
is.na(all_monthly$ever_active_again) <- all_monthly$attrition_event == F

all_monthly <- select(all_monthly, -c(user_id, domain, test, visits_ge_100))

#Flag users from typical FLW domains
#Note that it's better to flag at the user level (based on apps) in the future as 
#opposed to flagging on the domain level as I am doing here. 
typical_flw_domains <- 
c("aaharsneha", "aarohi", "acf", "aed-hth", "arogyasarita", "care-ecd",
  "chasssmt-moz", "crc-intervention", "crhp", "crs-catch", "crs-remind",
  "crs-senegal", "dtree-familyplanning", "engender-ethiopia-pilot", "icap-tb", 
  "kawok-malaria-p", "keiskamma", "kgvk", "maternalznz", "nutritionmeast", "opm", 
  "pasmo-nicaragua-dmg", "pci-india", "project", "puami-tsf-mnch-myanmar", "rdi-hiht", 
  "savethechildren", "savethechildren-nepal", "slttc", "ssqh-cs", "teba-hbc", 
  "tulasalud", "world-renew", "wvindia", "wvindia-nutrition", "wvindia2",
  "wvindonesia", "wvug", "yonsei-emco")

blog <- merge(all_monthly, domain_master_list, by = "domain_numeric", all.x = T)
blog$typical_flw <- blog$domain %in% typical_flw_domains
blog <- select(blog, -domain)

#write.csv(blog, file = "blog_data_2_13_15.csv")

#----------------------------------------------------------------------#
#Older code - not used to create future blog datasets
#----------------------------------------------------------------------#

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

all_monthly$domain_numeric = as.numeric(as.factor(all_monthly$domain))

#Keep only mobile/unknown users
all_monthly <- merge(all_monthly, user_type, by = "user_id", all.x = T)
all_monthly$user_type[is.na(all_monthly$user_type)] <- "unknown"
all_monthly <- filter(all_monthly, user_type == "mobile" | user_type == "unknown")

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

