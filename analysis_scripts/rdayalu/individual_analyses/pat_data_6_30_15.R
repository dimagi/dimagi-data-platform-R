library(dplyr)
library(lubridate)
pat <- read.csv(file = "blog_data_6_11_15.csv")
pat$calendar_month <- as.Date(pat$calendar_month, "%m/%d/%Y")
pat$domain_calendar_month <- paste(pat$domain_numeric, pat$calendar_month, sep = "_")
pat <- select(pat, -c(diff_days, unique_int))

#Flag each new user in the blog dataset 
#We are flagging the first calendar month of each user
pat <- pat %>% group_by(domain_numeric, user_pk) %>% 
  mutate(new_user = calendar_month == min(calendar_month))

#Sort by domain and calendar_month before calculating the cumulative sum 
#of new users in the next step
pat <- ungroup(pat)
pat <- arrange(pat, domain_numeric, calendar_month)

#Calculate cumulative sum of new users by domain
pat <- pat %>% group_by(domain_numeric) %>% 
  mutate(max_users = cumsum(new_user))

# Create aggregate table for #active users, #max users, 
#users with 3 mo attrition by domain/mont
max_users <- pat %>% group_by(domain_numeric, calendar_month) %>% 
  summarise(total_users_to_date = max(max_users))
max_users$domain_calendar_month <- paste(max_users$domain_numeric, max_users$calendar_month, sep = "_")
max_users <- select(max_users, total_users_to_date, domain_calendar_month)

#Merge to main dataframe
pat <- merge(pat, max_users, by = c("domain_calendar_month", "domain_numeric"), all.x=T)
pat <- select(pat, -c(max_users, new_user))
pat$dup_domain_month <- duplicated(pat$domain_calendar_month)
pat <- filter(pat, dup_domain_month == F)

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
detach("package:lubridate", unload=TRUE)
library(data.table)
library(lubridate)
pat <- arrange(pat, domain_numeric, calendar_month)
df <- data.table(pat)
setkey(df,domain_numeric)
df[,diff_days_domain:=c(NA,diff(calendar_month)),by=domain_numeric]
pat <- as.data.frame(df)
pat$previous_month_active_domain <- pat$diff_days_domain <= 31
pat$previous_two_months_active_domain <- pat$diff_days_domain <= 62
pat$previous_three_months_active_domain <- pat$diff_days_domain <= 93

domains <- unique(pat$domain_numeric)

next_month_active_domain <- c()
for (i in domains) {
  single_user <- pat[pat$domain_numeric == i,]
  next_active <- c()
  next_active <- c(single_user$previous_month_active_domain[-1], F)
  next_month_active_domain <- c(next_month_active_domain, next_active)
}
pat$next_month_active_domain <- next_month_active_domain

next_two_months_active_domain <- c()
for (i in domains) {
  single_user <- pat[pat$domain_numeric == i,]
  next_active <- c()
  next_active <- c(single_user$previous_two_months_active_domain[-1], F)
  next_two_months_active_domain <- c(next_two_months_active_domain, next_active)
}
pat$next_two_months_active_domain <- next_two_months_active_domain

next_three_months_active_domain <- c()
for (i in domains) {
  single_user <- pat[pat$domain_numeric == i,]
  next_active <- c()
  next_active <- c(single_user$previous_three_months_active_domain[-1], F)
  next_three_months_active_domain <- c(next_three_months_active_domain, next_active)
}
pat$next_three_months_active_domain <- next_three_months_active_domain

#Based on the end_month in our dataset, we don't know if the domain will be active in any of
#the months following end_month. Must change all those attrition values to NA.
end_month <- as.Date("2015-04-01")
is.na(pat$next_month_active_domain) <- pat$calendar_month == end_month
is.na(pat$next_two_months_active_domain) <- pat$calendar_month >= end_month - months(1) 
is.na(pat$next_three_months_active_domain) <- pat$calendar_month >= end_month - months(2)

#Calculate months_on_cc_domain and active_calendar_months_domain
total_months_cc <- pat %>% group_by(domain_numeric) %>% 
  summarise(first_month = min(calendar_month),
            last_month = max(calendar_month), 
            active_calendar_months_domain = length(unique(calendar_month)))
total_months_cc$months_on_cc_domain <- (interval(total_months_cc$first_month, 
                                          total_months_cc$last_month) %/% months(1))+1
total_months_cc <- select(total_months_cc, domain_numeric, active_calendar_months_domain, 
                          months_on_cc_domain)
pat <- merge(pat, total_months_cc, by = "domain_numeric", all.x = T)

#active_months_rolling
#Add rolling # active months to each domain's rows
pat <- pat %>% group_by(domain_numeric) %>% 
  mutate(active_months_rolling = seq_along(calendar_month))

#Was the domain ever active again after an attrition event 
#(defined as next_month_active_domain == F)?
pat$attrition_event_domain <- !(pat$next_month_active_domain == T | 
                                  is.na(pat$next_month_active_domain))
pat$continuing_domain <- pat$active_months_rolling < pat$active_calendar_months_domain
pat$ever_active_again_domain <- pat$attrition_event_domain == T & pat$continuing_domain == T
is.na(pat$ever_active_again_domain) <- pat$attrition_event_domain == F

#active_users_max_rolling 
#Maximum number of active users for any month including and prior to this month
names(pat)[names(pat) == "total_users_to_date"] = "active_users_max_rolling"

write.csv(pat, file="domain_monthly_rows_pat_6_30_15.csv")

