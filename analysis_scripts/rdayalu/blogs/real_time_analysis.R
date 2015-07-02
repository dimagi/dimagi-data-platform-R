# 5/22/15
# This script is for the real-time CommCare usage analysis for the Open Data Science Conference
# at the end of the month. 
# Outline of analysis here:
# https://docs.google.com/document/d/1udyW-YO5d2sdVDAKC-pE2b44tORsDjfLZShJJwlF9MI/edit

library(ggplot2)
library(scales)

# Import visit table
visit <- tbl(db, "visit")
visit <- collect(visit)

# Extract and format time/date variables
visit$visit_date <- as.Date(substr(visit$time_start, 1, 10))
visit$visit_time_start <- hms(substr(visit$time_start, 12, 19))
visit$visit_time_end <- hms(substr(visit$time_end, 12, 19))
visit$visit_date_time_start <- parse_date_time(visit$time_start,"%Y-%m-%d %H:%M:%S")
visit$visit_date_time_end <- parse_date_time(visit$time_end,"%Y-%m-%d %H:%M:%S")

# Visit duration
# Create categorical visit durations
visit$visit_duration <- 
  as.numeric(difftime(visit$visit_date_time_end, 
                      visit$visit_date_time_start, units = "mins"))

visit$duration_lt_1min <- visit$visit_duration < 1 & visit$visit_duration >= 0
visit$duration_ge_1min_lt_5min <- visit$visit_duration >= 1 & visit$visit_duration < 5
visit$duration_ge_5min <- visit$visit_duration >= 5 & visit$visit_duration <= 30

# Visit time of day
visit$daytime <- visit$visit_time_start >= hms("06:00:00") & visit$visit_time_start <= hms("17:59:59")
visit$nighttime <- visit$visit_time_start >= hms("18:00:00") | visit$visit_time_start <= hms("05:59:59")

# Time between visits (per user per day)
# Need to remove visit_time_start and visit_time_end because dplyr
# doesn't like class = Period
visit <- select(visit, -c(visit_time_start, visit_time_end))
# Sort by user, date and time 
visit <- arrange(visit, user_id, visit_date_time_start)

# Divide into 5 groups of 1.5 million rows (5K users) each (we have 25161 users total)
users <- unique(visit$user_id)
visit1 <- filter(visit, user_id <= 5000)
visit2 <- filter(visit, user_id > 5000 & user_id <= 5750)
visit3 <- filter(visit, user_id > 5750 & user_id <= 10000)
visit4 <- filter(visit, user_id > 10000 & user_id <= 17500)
visit5 <- filter(visit, user_id > 17500)

# Add previous visit times to each row by user_id and date
visit1 <- visit1 %>% group_by(user_id, visit_date) %>% 
  mutate(previous_visit_end_time = lag(visit_date_time_end))
visit2 <- visit2 %>% group_by(user_id, visit_date) %>% 
  mutate(previous_visit_end_time = lag(visit_date_time_end))
visit3 <- visit3 %>% group_by(user_id, visit_date) %>% 
  mutate(previous_visit_end_time = lag(visit_date_time_end))
visit4 <- visit4 %>% group_by(user_id, visit_date) %>% 
  mutate(previous_visit_end_time = lag(visit_date_time_end))
visit5 <- visit5 %>% group_by(user_id, visit_date) %>% 
  mutate(previous_visit_end_time = lag(visit_date_time_end))

# Add next visit times to each row by user_id and date
visit1 <- visit1 %>% group_by(user_id, visit_date) %>% 
  mutate(next_visit_start_time = lead(visit_date_time_start))
visit2 <- visit2 %>% group_by(user_id, visit_date) %>% 
  mutate(next_visit_start_time = lead(visit_date_time_start))
visit3 <- visit3 %>% group_by(user_id, visit_date) %>% 
  mutate(next_visit_start_time = lead(visit_date_time_start))
visit4 <- visit4 %>% group_by(user_id, visit_date) %>% 
  mutate(next_visit_start_time = lead(visit_date_time_start))
visit5 <- visit5 %>% group_by(user_id, visit_date) %>% 
  mutate(next_visit_start_time = lead(visit_date_time_start))

#Append all tables together
summary(names(visit1) == names(visit2))
visit <- rbind(visit1, visit2, visit3, visit4, visit5)
remove(visit1)

#Calculate time to next visit and time from previous visit
visit$time_to_next_visit <- as.numeric((visit$next_visit_start_time - visit$visit_date_time_end)/60)
visit$time_from_prev_visit <- as.numeric((visit$visit_date_time_start - visit$previous_visit_end_time)/60)

#Flag batch visits
visit$batch_visit <- visit$time_to_next_visit < 10 | visit$time_from_prev_visit < 10

# A basic box plot
visit$cond <- 1
visits_valid_duration <- filter(visit, visit_duration >= 0 & visit_duration <= 30)
ggplot(visits_valid_duration, aes(x=cond, y=visit_duration)) + 
  geom_boxplot()

#duration_ge_5min
#duration_ge_1min_lt_5min
#duration_lt_1min
test2 <- filter(visits_valid_duration, batch_visit == T & duration_lt_1min == T & nighttime == T)
length(unique(test2$domain_id))

#-----------------------------------------------------------------------------------------#

#Look at DSA domains
test <- filter(monthly_table, nvisits_travel > 0)

#Import form_annotations from S3 server
form_annotations <- read.csv(file = "form_annotations.csv")
form_annotations$travel_form <- form_annotations$Travel.visit == "Yes"
domains_travel_yes <- form_annotations %>% group_by(Domain.name) %>% 
  summarise(nforms_travel = sum(Travel.visit == "yes"))
domains_travel <- form_annotations %>% group_by(Domain.name) %>% 
  summarise(nforms_travel = sum(travel_form))
domains_travel <- filter(domains_travel, nforms_travel > 0)

monthly_travel <- filter(monthly_table, domain %in% domains_travel$Domain.name)
monthly_travel_yes <- filter(monthly_table, domain %in% 
                               domains_travel_yes$Domain.name[domains_travel_yes$nforms_travel > 0])

monthly_travel <- merge(monthly_travel, select(domains, name, id), by.x = "domain", 
                        by.y = "name", all.x = T)
names(monthly_travel)[names(monthly_travel) == "id"] = "domain_id"
monthly_travel <- filter(monthly_travel, nvisits_travel > 0)
monthly_travel$batch_travel_percent <- (monthly_travel$nvisits_travel_batch/monthly_travel$nvisits_travel)*100
monthly_travel$unique_row <- 1:nrow(monthly_travel)
domain_month_batch_travel_median <- monthly_travel %>% group_by(domain) %>% 
  summarise(nuser_months_travel = length(unique(unique_row)), 
            batch_travel_percent_user_month_median = median(batch_travel_percent, na.rm=T))



visits_domain_travel <- filter(visits_valid_duration, domain_id %in% monthly_travel$domain_id)
visits_domain_travel$visit_month <- floor_date(visits_domain_travel$visit_date, "month")
monthly_travel_manual <- visits_domain_travel %>% group_by(domain_id, user_id, visit_month) %>% 
  summarise(nvisits = length(unique(id)), 
            nvisits_batch = sum(batch_visit, na.rm=T))
monthly_travel_manual$batch_percent <- (monthly_travel_manual$nvisits_batch/monthly_travel_manual$nvisits)*100
monthly_travel_manual$unique_row <- 1:nrow(monthly_travel_manual)

domain_month_batch_median <- monthly_travel_manual %>% group_by(domain_id) %>% 
  summarise(nuser_months = length(unique(unique_row)), 
            batch_percent_user_month_median = median(batch_percent, na.rm=T))
domain_month_batch_median <- merge(domain_month_batch_median, select(domains, name, id), by.x = "domain_id", 
                        by.y = "id", all.x = T)

domain_month_batch <- merge(domain_month_batch_median, domain_month_batch_travel_median, by.x = "name", 
                                   by.y = "domain", all.x = T)
domain_month_batch <- select(domain_month_batch, -domain_id)

write.csv(domain_month_batch, file = "domain_month_batch.csv")

domain_month_batch <- filter(domain_month_batch, batch_percent_user_month_median > 0)
domain_month_batch$program <- 1:nrow(domain_month_batch)
batch_graph <- select(domain_month_batch, batch_percent_user_month_median, program)
batch_graph$visit_type <- "All visits"
names(batch_graph)[names(batch_graph) == "batch_percent_user_month_median"] = "batch_percent"
batch_graph2 <- select(domain_month_batch, batch_travel_percent_user_month_median, program)
batch_graph2$visit_type <- "Travel visits"
names(batch_graph2)[names(batch_graph2) == "batch_travel_percent_user_month_median"] = "batch_percent"
summary(names(batch_graph) == names(batch_graph2))
batch_graph <- rbind(batch_graph, batch_graph2)

ggplot(batch_graph, aes(x = program, y = batch_percent, colour = visit_type)) + 
  geom_line()

ggplot(batch_graph, aes(x = program, y = batch_percent, fill = visit_type)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  scale_x_continuous(breaks= pretty_breaks(9)) + 
  xlab("Program") +
  ylab("% Batch Visits") +
  labs(fill = "Visit type")
