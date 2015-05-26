# 5/22/15
# This script is for the real-time CommCare usage analysis for the Open Data Science Conference
# at the end of the month. 
# Outline of analysis here:
# https://docs.google.com/document/d/1udyW-YO5d2sdVDAKC-pE2b44tORsDjfLZShJJwlF9MI/edit

library(ggplot2)

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
test <- filter(visit, visit_duration >= 0 & visit_duration <= 30)
ggplot(test, aes(x=cond, y=visit_duration)) + 
  geom_boxplot()

#duration_ge_5min
#duration_ge_1min_lt_5min
#duration_lt_1min
test2 <- filter(test, batch_visit == T & duration_lt_1min == T & nighttime == T)
length(unique(test2$domain_id))


#Look at DSA domains
test <- filter(monthly_table, nvisits_travel > 0)
