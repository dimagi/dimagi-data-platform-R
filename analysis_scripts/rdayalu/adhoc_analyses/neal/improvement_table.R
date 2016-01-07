# Create improvement table for Gates grant
# This table is to see if WAR usage (based on GA stats) can be correlated with 
# improvement in CommCare usage (as measured by % active days)
# https://docs.google.com/a/dimagi.com/document/d/1Lx_q9jVaDORTSkBE6iEvPqlC2rrrRSOYB590Cq5AZmw/edit
# 2/11/15

#Create aggregate monthly data set
all_monthly <- monthly_table

#Set report_options
report = run_conf$reports$modules$name
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

#Keep only columns of interest from domain_table
names(domain_table)[names(domain_table) == "id"] = "domain_id"
facets_to_merge <- select(domain_table, name, domain_id)

#Merge domain facets from domain table into all_monthly table
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
names(all_monthly)[names(all_monthly) == "domain_id"] = "domain_numeric"

#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

# Table for 2014 users
all_2014 <- filter(all_monthly, calendar_month >= "2014-01-01" & calendar_month <= "2014-12-31")

# Tables of 4 quarters
q1 <- filter(all_monthly, calendar_month >= "2014-01-01" & calendar_month <= "2014-03-31")
q2 <- filter(all_monthly, calendar_month >= "2014-04-01" & calendar_month <= "2014-06-30")
q3 <- filter(all_monthly, calendar_month >= "2014-07-01" & calendar_month <= "2014-09-30")
q4 <- filter(all_monthly, calendar_month >= "2014-10-01" & calendar_month <= "2014-12-31")

# Exclude users who submitted data to > 1 domain
# Create dataset for all active users in 2014
n_domains <- all_2014 %>% group_by(user_pk) %>% summarise(ndomains = length(unique(domain)))
n_domains <- filter(n_domains, ndomains == 1)
all_2014 <- all_2014[all_2014$user_pk %in% n_domains$user_pk,]

# Create user 2014 table
user_2014 <- all_2014 %>% 
  group_by(user_pk) %>% 
  summarise(user_id = unique(user_id),
            domain = unique(domain),
            domain_numeric = unique(domain_numeric))

# Calculate quarterly active day % only for users who were active for the full quarter
# of interest
n_active_months_q1 <- q1 %>% group_by(user_pk) %>% 
  summarise(nmos = length(unique(calendar_month)), 
            active_days_q1 = sum(active_days),
            total_days_q1 = as.numeric(as.Date("2014-04-01") - as.Date("2014-01-01")),
            per_active_days_q1 = (active_days_q1/total_days_q1)*100,
            med_per_active_days_q1 = median(active_day_percent, na.rm = T),
            nmos_ge_4_days_active = sum(active_days >= 4))
n_active_months_q1$ge_4_days_active_q1 <- n_active_months_q1$nmos_ge_4_days_active == 3
n_active_months_q1 <- filter(n_active_months_q1, nmos == 3)
n_active_months_q1 <- select(n_active_months_q1, user_pk, per_active_days_q1, 
                             med_per_active_days_q1, ge_4_days_active_q1)
user_2014 <- merge(user_2014, n_active_months_q1, by = "user_pk", all.x = T)

n_active_months_q2 <- q2 %>% group_by(user_pk) %>% 
  summarise(nmos = length(unique(calendar_month)), 
            active_days_q2 = sum(active_days),
            total_days_q2 = as.numeric(as.Date("2014-07-01") - as.Date("2014-04-01")),
            per_active_days_q2 = (active_days_q2/total_days_q2)*100,
            med_per_active_days_q2 = median(active_day_percent, na.rm = T),
            nmos_ge_4_days_active = sum(active_days >= 4))
n_active_months_q2$ge_4_days_active_q2 <- n_active_months_q2$nmos_ge_4_days_active == 3
n_active_months_q2 <- filter(n_active_months_q2, nmos == 3)
n_active_months_q2 <- select(n_active_months_q2, user_pk, per_active_days_q2, 
                             med_per_active_days_q2, ge_4_days_active_q2)
user_2014 <- merge(user_2014, n_active_months_q2, by = "user_pk", all.x = T)

n_active_months_q3 <- q3 %>% group_by(user_pk) %>% 
  summarise(nmos = length(unique(calendar_month)), 
            active_days_q3 = sum(active_days),
            total_days_q3 = as.numeric(as.Date("2014-10-01") - as.Date("2014-07-01")),
            per_active_days_q3 = (active_days_q3/total_days_q3)*100,
            med_per_active_days_q3 = median(active_day_percent, na.rm = T),
            nmos_ge_4_days_active = sum(active_days >= 4))
n_active_months_q3$ge_4_days_active_q3 <- n_active_months_q3$nmos_ge_4_days_active == 3
n_active_months_q3 <- filter(n_active_months_q3, nmos == 3)
n_active_months_q3 <- select(n_active_months_q3, user_pk, per_active_days_q3, 
                             med_per_active_days_q3, ge_4_days_active_q3)
user_2014 <- merge(user_2014, n_active_months_q3, by = "user_pk", all.x = T)

n_active_months_q4 <- q4 %>% group_by(user_pk) %>% 
  summarise(nmos = length(unique(calendar_month)), 
            active_days_q4 = sum(active_days),
            total_days_q4 = as.numeric(as.Date("2015-01-01") - as.Date("2014-10-01")),
            per_active_days_q4 = (active_days_q4/total_days_q4)*100,
            med_per_active_days_q4 = median(active_day_percent, na.rm = T),
            nmos_ge_4_days_active = sum(active_days >= 4))
n_active_months_q4$ge_4_days_active_q4 <- n_active_months_q4$nmos_ge_4_days_active == 3
n_active_months_q4 <- filter(n_active_months_q4, nmos == 3)
n_active_months_q4 <- select(n_active_months_q4, user_pk, per_active_days_q4, 
                             med_per_active_days_q4, ge_4_days_active_q4)
user_2014 <- merge(user_2014, n_active_months_q4, by = "user_pk", all.x = T)

# Lists of users who have been active in particular quarters
active_q2_q4 <- n_active_months_q2$user_pk[n_active_months_q2$user_pk %in% n_active_months_q4$user_pk]
active_q2_q3_q4 <- n_active_months_q3$user_pk[n_active_months_q3$user_pk %in% active_q2_q4]
active_q2_q3 <- n_active_months_q2$user_pk[n_active_months_q2$user_pk %in% n_active_months_q3$user_pk]
active_q3_q4 <- n_active_months_q3$user_pk[n_active_months_q3$user_pk %in% n_active_months_q4$user_pk]
active_q1_q2_q3 <- n_active_months_q1$user_pk[n_active_months_q1$user_pk %in% active_q2_q3]

# Flag users who were active in relevant quarters
user_2014$active_q2_q3_q4 <- user_2014$user_pk %in% active_q2_q3_q4 
user_2014$active_q2_q3 <- user_2014$user_pk %in% active_q2_q3
user_2014$active_q3_q4 <- user_2014$user_pk %in% active_q3_q4
user_2014$active_q1_q2_q3 <- user_2014$user_pk %in% active_q1_q2_q3

# Calculate change in % active days per user between quarters of interest
user_2014$change_q2_q4 <- user_2014$per_active_days_q4 - user_2014$per_active_days_q2
user_2014$change_q2_q3 <- user_2014$per_active_days_q3 - user_2014$per_active_days_q2
user_2014$change_q3_q4 <- user_2014$per_active_days_q4 - user_2014$per_active_days_q3
user_2014$change_q1_q3 <- user_2014$per_active_days_q3 - user_2014$per_active_days_q1

user_2014$change_med_q2_q4 <- user_2014$med_per_active_days_q4 - user_2014$med_per_active_days_q2
user_2014$change_med_q2_q3 <- user_2014$med_per_active_days_q3 - user_2014$med_per_active_days_q2
user_2014$change_med_q3_q4 <- user_2014$med_per_active_days_q4 - user_2014$med_per_active_days_q3
user_2014$change_med_q1_q3 <- user_2014$med_per_active_days_q3 - user_2014$med_per_active_days_q1


# Create improvement table
improve_2014 <- all_2014 %>% 
  group_by(domain_numeric) %>% 
  summarise(domain = unique(domain), 
            nusers_2014 = length(unique(user_pk)),
            nusers_q1 = length(unique(user_pk[calendar_month >= "2014-01-01" & calendar_month <= "2014-03-31"])),
            nusers_q2 = length(unique(user_pk[calendar_month >= "2014-04-01" & calendar_month <= "2014-06-30"])),
            nusers_q3 = length(unique(user_pk[calendar_month >= "2014-07-01" & calendar_month <= "2014-09-30"])),
            nusers_q4 = length(unique(user_pk[calendar_month >= "2014-10-01" & calendar_month <= "2014-12-31"])))

#--------------------------------------------------------------------------------------#

# Populate improvement table: Q2 to Q4
domain_stats <- user_2014 %>% group_by(domain_numeric) %>% 
  summarise(active_users_q2_q3_q4 = length(user_pk[active_q2_q3_q4 == T]))
improve_2014 <- merge(improve_2014, domain_stats, by = "domain_numeric", all.x = T)

user_2014_active <- filter(user_2014, active_q2_q3_q4 == T)
domain_stats_active <- user_2014_active %>% 
  group_by(domain_numeric) %>% 
  summarise(med_change_q2_q4 = median(change_q2_q4, na.rm=T), 
            mean_change_q2_q4 = mean(change_q2_q4, na.rm = T),
            med_change_med_q2_q4 = median(change_med_q2_q4, na.rm=T),
            nusers_ge_4_active_days_q2 = sum(ge_4_days_active_q2), 
            nusers_ge_4_active_days_q4 = sum(ge_4_days_active_q4))
improve_2014 <- merge(improve_2014, domain_stats_active, by = "domain_numeric", all.x = T)
improve_2014$prop_users_ge_4_active_days_q2 <- improve_2014$nusers_ge_4_active_days_q2/improve_2014$nusers_q2
improve_2014$prop_users_ge_4_active_days_q4 <- improve_2014$nusers_ge_4_active_days_q4/improve_2014$nusers_q4
improve_2014$per_change_users_ge_4_active_days_q2_q4 <- 100*(improve_2014$prop_users_ge_4_active_days_q4-improve_2014$prop_users_ge_4_active_days_q2)  

# Populate improvement table: Q2 to Q3
domain_stats <- user_2014 %>% group_by(domain_numeric) %>% 
  summarise(active_users_q2_q3 = length(user_pk[active_q2_q3 == T]))
improve_2014 <- merge(improve_2014, domain_stats, by = "domain_numeric", all.x = T)

user_2014_active <- filter(user_2014, active_q2_q3 == T)
domain_stats_active <- user_2014_active %>% 
  group_by(domain_numeric) %>% 
  summarise(med_change_q2_q3 = median(change_q2_q3, na.rm=T), 
            mean_change_q2_q3 = mean(change_q2_q3, na.rm = T))
improve_2014 <- merge(improve_2014, domain_stats_active, by = "domain_numeric", all.x = T)

# Populate improvement table: Q3 to Q4
domain_stats <- user_2014 %>% group_by(domain_numeric) %>% 
  summarise(active_users_q3_q4 = length(user_pk[active_q3_q4 == T]))
improve_2014 <- merge(improve_2014, domain_stats, by = "domain_numeric", all.x = T)

user_2014_active <- filter(user_2014, active_q3_q4 == T)
domain_stats_active <- user_2014_active %>% 
  group_by(domain_numeric) %>% 
  summarise(med_change_q3_q4 = median(change_q3_q4, na.rm=T), 
            mean_change_q3_q4 = mean(change_q3_q4, na.rm = T))
improve_2014 <- merge(improve_2014, domain_stats_active, by = "domain_numeric", all.x = T)

# Populate improvement table: Q1 to Q3
domain_stats <- user_2014 %>% group_by(domain_numeric) %>% 
  summarise(active_users_q1_q2_q3 = length(user_pk[active_q1_q2_q3 == T]))
improve_2014 <- merge(improve_2014, domain_stats, by = "domain_numeric", all.x = T)

user_2014_active <- filter(user_2014, active_q1_q2_q3 == T)
domain_stats_active <- user_2014_active %>% 
  group_by(domain_numeric) %>% 
  summarise(med_change_q1_q3 = median(change_q1_q3, na.rm=T), 
            mean_change_q1_q3 = mean(change_q1_q3, na.rm = T))
improve_2014 <- merge(improve_2014, domain_stats_active, by = "domain_numeric", all.x = T)

#Merge improvement table with GA table from google_analytics_WAR.R
#First need to aggregate GA table by domain
war_domain <- ga_users %>% group_by(domain) %>% 
  summarise(nviews = sum(pageviews),
            nunique_views = sum(unique_pageviews))

#            med_time_on_page = median(avg_time_on_page, na.rm=T))

improve_2014 <- merge(improve_2014, war_domain, by = "domain", all.x = T)
improve_2014$used_war <- !is.na(improve_2014$nviews)

write.csv(improve_2014, file = "improvement_q2_q4_2014.csv")

# Test correlations between WAR usage and CommCare usage
test <- filter(improve_2014, !is.na(med_change_q1_q3) & used_war == T)
test <- filter(improve_2014, !is.na(med_change_med_q2_q4) & used_war == T)
test <- filter(improve_2014, !is.na(per_change_users_ge_4_active_days_q2_q4) & used_war == T)

cor(test$per_change_users_ge_4_active_days_q2_q4, test$nviews)
cor(test$per_change_users_ge_4_active_days_q2_q4, test$nunique_views)

# T-test for mean CommCare usage between WAR domains and non-WAR domains
test <- filter(improve_2014, !is.na(per_change_users_ge_4_active_days_q2_q4))
t.test(test$per_change_users_ge_4_active_days_q2_q4~test$used_war)

#Visuals
test <- all_2014 %>% group_by(month_abbr) %>% 
  summarise(med_ncases_touched = median(ncases_touched, na.rm=T))

ggplot(data=test, aes(x=month_abbr, y=med_ncases_touched, group=1)) + 
  geom_line() + 
  scale_y_continuous(limits=c(0,40))

ggplot(test, aes(x=med_change_med_q2_q4, y=nunique_views)) +
  geom_point(shape=1) + 
  #geom_smooth(method=lm) + 
  scale_x_continuous(limits=c(-20,25)) + 
  scale_y_continuous(limits=c(0,22)) + 
  annotate("text", label="r^2 == 0.196", parse = T, x=-15, y=21) 

test <- filter(all_2014, calendar_month >= "2014-04-01" & calendar_month <= "2014-09-30")
nactive_users <- test %>% group_by(user_pk) %>%
  summarise(nmos = length(unique(calendar_month)))
nactive_users <- filter(nactive_users, nmos == 6)
test <- test[test$user_pk %in% nactive_users$user_pk,]
test$calendar_factor <- as.factor(test$calendar_month)
test$month_number <- as.numeric(test$calendar_factor)
cor(test$month_number, test$active_day_percent)

ggplot(test, aes(x=month_number, y=active_day_percent)) +
  geom_point(shape=1)
  #geom_smooth(method=lm) + 
  #scale_x_continuous(limits=c(-20,25)) + 
  #scale_y_continuous(limits=c(0,22)) + 
  #annotate("text", label="r^2 == 0.196", parse = T, x=-15, y=21) 