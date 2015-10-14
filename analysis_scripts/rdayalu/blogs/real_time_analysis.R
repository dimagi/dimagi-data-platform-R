# 5/22/15
# This script is for the real-time CommCare usage analysis for the Open Data Science Conference
# at the end of the month. 
# Outline of analysis here:
# https://docs.google.com/document/d/1udyW-YO5d2sdVDAKC-pE2b44tORsDjfLZShJJwlF9MI/edit

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# Get db connection
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))
db <- get_db_connection(system_conf)

#Import form table and form annotations
form_table <- tbl(db, "form")
form_table <- collect(form_table)
form_annotations <- read.csv(file = "form_annotations.csv", stringsAsFactors = F)

#Get domain table
domain <- get_domain_table(db)
names(domain)[names(domain) == "id"] = "domain_pk"
names(domain)[names(domain) == "name"] = "domain"
domain$subsector <- lapply(domain$subsector, as.character)
domains_mch_subsector <- seq_along(domain$subsector)[sapply(domain$subsector, FUN=function(X) "Maternal, Newborn, & Child Health" %in% X)]
domain$mch_subsector <- F
domain$mch_subsector[domains_mch_subsector] <- T

#We are only going to keep only travel forms because we only want to know which visits used travel forms 
#based on the form_annotation file.
test <- filter(form_annotations, Travel.visit == "Yes")
travel_domains <- filter(domain, domain %in% test$Domain.name)
forms_travel <- filter(form_table, domain_id %in% travel_domains$domain_pk)
#Import formdef table
formdef <- tbl(db, "formdef")
formdef <- collect(formdef)
formdef_travel <- filter(formdef, xmlns %in% test$Form.xmlns) #Not sure why this has > 200 domains... will look into this later
remove(formdef)
#Keep only travel forms
forms_travel <- filter(forms_travel, formdef_id %in% formdef_travel$id)
remove(form_table)

# Import visit table
visit <- tbl(db, "visit")
visit <- collect(visit)

# Format visit table
names(visit)[names(visit) == "id"] <- "visit_id"
names(visit)[names(visit) == "user_id"] <- "user_pk"
names(visit)[names(visit) == "domain_id"] <- "domain_pk"
visit <- select(visit, -c(time_end, time_start)) #These columns are missing the time element because of the size of the table

#Pull in visit time columns separately and merge into visit table
visit_times <- visit_pull_times(db)

#Merge in domain table
visit <- merge(visit, select(domain, domain_pk, domain, is_test, subsector), by = "domain_pk", all.x = T)

#Import user table and user_type table
#We are going to keep only mobile users
users <- tbl(db, "users") 
users <- collect(users)
names(users)[names(users) == "id"] = "user_pk"
users <- select(users, user_pk, user_id, username, email)

user_type <- get_user_type_table(db)
user_type <- collect(user_type)
user_type <- select(user_type, user_pk, user_type)

users <- merge(users, user_type, by = "user_pk", all.x = T)
visit <- merge(visit, users, by = "user_pk", all.x = T)



#------------------------------------------------------------------------#
# Create variables for real-time analysis
#------------------------------------------------------------------------#

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
visit <- arrange(visit, user_pk, visit_date_time_start)
users <- unique(visit$user_pk)

# Add previous visit times to each row by user_pk
visit <- visit %>% group_by(user_pk) %>% 
  mutate(previous_visit_end_time = lag(visit_date_time_end))

# Add next visit times to each row by user_pk
visit <- visit %>% group_by(user_pk) %>% 
  mutate(next_visit_start_time = lead(visit_date_time_start))

#Calculate time to next visit and time from previous visit
visit$time_to_next_visit <- round(as.numeric((visit$next_visit_start_time - visit$visit_date_time_end)/60), digits = 1)
visit$time_from_prev_visit <- round(as.numeric((visit$visit_date_time_start - visit$previous_visit_end_time)/60), digits = 1)

#Flag batch visits
visit$batch_visit <- visit$time_to_next_visit < 10 | visit$time_from_prev_visit < 10
#Recode batch_visit = NA as batch_visit = F. They only show up as NA because the time_to_next and/or time_from_prev
#are NA. If one of these times is <10, the visit will be flagged as batch even if the other is NA, so there's nothing to worry about here.
visit$batch_visit[is.na(visit$batch_visit)] <- F

#Note that 4.7% of visits are nested for some reason: they have time_to_next_visit < 0
#In other words, they have end times after the next visit start time. Should probably chuck out these visits because phone time is off. 
#Visit duration and time between visits for these nested visits is unreliable.
#But which one to throw out? The previous visit that ended after the next visit started or the next visit that started before the 
#previous visit ended? Maybe both.
#Let's flag both nested visits. Note that nested_visit = NA means that it is not a nested visit. We can keep these visits 
#They only show up as NA because the time_to_next and/or time_from_prev
#are NA. If one of these times is negative, the visit will be flagged as nested even if the other is NA, so there's nothing to worry about here.
#This gives us a total of 6.0% nested visits in either direction

visit$nested_visit <- visit$time_to_next_visit < 0 | visit$time_from_prev_visit < 0

#Some domains/users have more nested visits than others. Spoke with Amelia and confirmed that this is not necessarily a phone time problem.
#It can also be because the user can start a form, interrupt it and then start another form. 
domain_nested <- visit %>% group_by(domain) %>% 
  summarise(nvisits = length(unique(visit_id)), 
            nvisits_nested = sum(nested_visit, na.rm = T), 
            is_test = unique(is_test))
domain_nested$nested_per_domain <- round((domain_nested$nvisits_nested/domain_nested$nvisits)*100, digits = 1)
domain_nested$nested_per_overall <- round((domain_nested$nvisits_nested/nrow(filter(visit, nested_visit == T)))*100, digits = 1)
domain_nested <- arrange(domain_nested, desc(nested_per_overall))
write.csv(domain_nested, file = "domains_nested.csv", row.names = F)

#Exclude visits with duration < 0 mins or duration > 30 mins
visit_working <- filter(visit, visit_duration <= 30)
visit_working <- filter(visit_working, visit_duration >= 0)

#Exclude nested visits
visit_working <- filter(visit_working, nested_visit == F | is.na(nested_visit))

#Only keep test = F domains
visit_working <- filter(visit_working, is_test == "false")

#Only keep travel domain visits and forms
test <- filter(form_annotations, Travel.visit == "Yes")
travel_visits <- filter(visit_working, domain %in% test$Domain.name)
travel_visits <- filter(travel_visits, visit_id %in% forms_travel$visit_id)
visit_working <- travel_visits

#Only keep crs-remind and tulasalud
crs_visits <- filter(visit_working, domain == "crs-remind")
tula_visits <- filter(visit_working, domain == "tulasalud")
visit_working <- crs_visits

crs_forms_travel <- filter(forms_travel, domain_id == 40)
crs_travel_visits <- filter(travel_visits, visit_id %in% crs_forms_travel$visit_id)
visit_working <- crs_travel_visits
crs_travel_subset <- filter(crs_travel_visits, visit_date >= "2012-07-28" & visit_date <= "2014-04-09")

#Only keep travel domains without crs-remind
travel_visits_subset <- filter(travel_visits, domain != "crs-remind")
visit_working <- travel_visits_subset

#------------------------------------------------------------------------#
#TABLE 1
#------------------------------------------------------------------------#
visit_working$group_num <- 1

table_1 <- visit_working %>% group_by(group_num) %>% 
  summarise(A1 = sum(batch_visit == F & duration_ge_5min == T & daytime == T), 
            B1 = sum(batch_visit == F & duration_ge_1min_lt_5min == T & daytime == T), 
            C1 = sum(batch_visit == F & duration_lt_1min == T & daytime == T), 
            A2 = sum(batch_visit == F & duration_ge_5min == T & nighttime == T), 
            B2 = sum(batch_visit == F & duration_ge_1min_lt_5min == T & nighttime == T), 
            C2 = sum(batch_visit == F & duration_lt_1min == T & nighttime == T), 
            A3 = sum(batch_visit == T & duration_ge_5min == T & daytime == T), 
            B3 = sum(batch_visit == T & duration_ge_1min_lt_5min == T & daytime == T), 
            C3 = sum(batch_visit == T & duration_lt_1min == T & daytime == T), 
            A4 = sum(batch_visit == T & duration_ge_5min == T & nighttime == T), 
            B4 = sum(batch_visit == T & duration_ge_1min_lt_5min == T & nighttime == T), 
            C4 = sum(batch_visit == T & duration_lt_1min == T & nighttime == T))

#Add percentages
table_1 <- rbind(table_1, round((table_1/nrow(visit_working))*100, digits = 1))
table_1[2,] <- paste0("(", table_1[2,],")")

write.csv(table_1, file = "table_1.csv", row.names = F)

#------------------------------------------------------------------------#
#User exclusions
#------------------------------------------------------------------------#

#Exclude demo, admin, unknown, NA/NONE users
visit_working <- visit_working[!(visit_working$user_id == "demo_user"),]
visit_working <- visit_working[!(visit_working$user_id == "NONE"),]
visit_working <- visit_working[!(visit_working$user_id == "none"),]
visit_working <- visit_working[!is.na(visit_working$user_id),]

#Keep only users with user_type = mobile
visit_working <- filter(visit_working, user_type == "mobile")

#Exclude users who submit to multiple domains
users_multiple_domain <- visit_working %>% group_by(user_pk) %>% 
  summarise(n_domains = length(unique(domain)))
users_multiple_domain <- filter(users_multiple_domain, n_domains > 1)
visit_working <- visit_working[!(visit_working$user_pk %in% users_multiple_domain$user_pk),]

#Exclude dimagi users based on username and email
dimagi_users <- unique(c(visit_working$user_pk[grep("dimagi", visit_working$email, fixed=T)], 
                         visit_working$user_pk[grep("dimagi", visit_working$username, fixed=T)]))
visit_working$dimagi_user <- visit_working$user_pk %in% dimagi_users
visit_working <- filter(visit_working, dimagi_user == F)

#-----------------------------------------------------------------------------------------#

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
