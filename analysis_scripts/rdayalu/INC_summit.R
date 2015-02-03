#This code is to calculate the numbers that Neal wants for the INC summit
#https://docs.google.com/a/dimagi.com/document/d/1WxjTeRNgXPvxMqn1m46jFlU_rt21AfuoVF_t2SL4mzY/edit

all_monthly <- monthly_table
library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate)
library(ggplot2)

#Format dates
all_monthly$date_first_visit = as.Date(all_monthly$date_first_visit)
all_monthly$date_last_visit = as.Date(all_monthly$date_last_visit)

#Change column names as needed
names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names (all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"

#Modify relevant variables
all_monthly$domain_numeric = as.numeric(as.factor(all_monthly$domain))

#Merge domain facets from domain table into all_monthly table
facets_to_merge <- select(domain_table, name, country, Sector, Sub.Sector,
                          business_unit, active, Test.Project.)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

#Remove users with device_type = "Sms" at any point in time
device_type_sms <- unique(filter(all_monthly, summary_device_type == "Sms")$user_pk)
all_monthly <- all_monthly[!(all_monthly$user_pk %in% device_type_sms),]

#Run the code below first without excluding any more users - keep these results for comparison
users_start_month <- all_monthly %>% group_by(user_pk) %>% 
  summarise(start_month = min(calendar_month), 
            total_active_days = sum(active_days, na.rm = T),
            start_date = min(date_first_visit, na.rm = T),
            end_date = max(date_last_visit, na.rm = T))
users_start_month$days_on_cc <- 
  as.numeric(users_start_month$end_date - users_start_month$start_date) + 1
users_start_month$le_31 <- users_start_month$days_on_cc <= 31
#There are a lot of users with days_on_cc == NA - need to figure out why.
#Then exclude users with <= 3 active days and keep those results
users_start_month <- filter(users_start_month, total_active_days >= 4)
all_monthly <- all_monthly[all_monthly$user_pk %in% users_start_month$user_pk,]

#2: 2014 gains and losses
#List of users active in Q4 2013
q4_2013 <- filter(all_monthly, calendar_month >= "2013-10-01" & 
                 calendar_month <= "2013-12-01")
users_2013_Q4 <- unique(q4_2013$user_pk)
#List of users active in Q4 2014
q4_2014 <- filter(all_monthly, calendar_month >= "2014-10-01" & 
                    calendar_month <= "2014-12-01")
users_2014_Q4 <- unique(q4_2014$user_pk)
#List of users active in Q3 2014
q3_2014 <- filter(all_monthly, calendar_month >= "2014-07-01" & 
                    calendar_month <= "2014-09-01")
users_2014_Q3 <- unique(q3_2014$user_pk)
#List of users started in Q1/Q2/Q3 2014
users_started_2014_three_qs <- (filter(users_start_month, start_month >= "2014-01-01" & 
                 start_month <= "2014-09-01"))$user_pk
users_started_2014_q4 <- (filter(users_start_month, start_month >= "2014-10-01"))$user_pk

#Kept: #users who were active in Q4 2013 and in Q4 2014
summary(users_2013_Q4 %in% users_2014_Q4)
kept <- (sum(users_2013_Q4 %in% users_2014_Q4)/length(users_2013_Q4))*100

#Lost: #users who were active in Q4 2013 but not active in Q4 2014
summary(!(users_2013_Q4 %in% users_2014_Q4))
lost <- (sum(!(users_2013_Q4 %in% users_2014_Q4))/length(users_2013_Q4))*100

#New: #users  who started in Q1, Q2, or Q3 2014 and were active in Q4 2014
summary(users_started_2014_three_qs %in% users_2014_Q4)
new <- (sum(users_started_2014_three_qs %in% users_2014_Q4)/length(users_started_2014_three_qs))*100

#Temporary: #users who started in Q1, Q2, or Q3 2014 but were not active in Q4 2014
summary(!(users_started_2014_three_qs %in% users_2014_Q4))
temporary <- (sum(!(users_started_2014_three_qs %in% users_2014_Q4))/length(users_started_2014_three_qs))*100

#Started: #users who started in Q4 2014
length(users_started_2014_q4)

##########################################################################

ggplot(users_start_month, aes(x=total_active_days)) + 
  geom_histogram(binwidth=1, colour = "black", fill = "antiquewhite1") + 
  scale_x_continuous(limits=c(0,30)) + 
  xlab("Total # active days") +
  ylab("Number of users") +
  theme(axis.title.x=element_text(size=14), axis.text.x=element_text(size=14, colour = "black")) + 
  theme(axis.title.y=element_text(size=14), axis.text.y=element_text(size=14, colour = "black"))

active_day_summary <- users_start_month %>% group_by(total_active_days) %>% 
  summarise(nusers = length(user_pk),
            days_on_cc_le_31 = sum(le_31))
active_day_summary$per_le_31 <- 
  (active_day_summary$days_on_cc_le_31/active_day_summary$nusers)*100

g <- ggplot(active_day_summary, aes(x = total_active_days, y = per_le_31)) +
  geom_point(shape = 15, size = 3.0, colour="darkblue") +
  geom_line(size = 1.0, colour = "cornflowerblue") + 
  scale_x_continuous(limits = c(0,30)) +
  xlab("Total # active days") +
  ylab("% Users with total days on CC <= 31") +
  theme(axis.title.x=element_text(size=14), axis.text.x=element_text(size=14, colour = "black")) + 
  theme(axis.title.y=element_text(size=14), axis.text.y=element_text(size=14, colour = "black"))
