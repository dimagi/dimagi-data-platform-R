library(dplyr)
library(lme4)

# load config files
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))

db <- get_db_connection(system_conf)

# load domain attributes and monthly aggregate table, but only keep domains where data use is permitted
source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
domain_table <- get_domain_table(db)
domain_table <- domain_table[domain_table$name %in% get_permitted_domains(domain_table),]
monthly_table <- collect(tbl(db, "aggregate_monthly_interactions"))
monthly_table <- monthly_table[monthly_table$domain %in% get_permitted_domains(domain_table),]

monthly_table$month_start<-as.Date(paste0('1 ',monthly_table$month.index), '%d %b %Y')

# remove demo users and NA/NONE users
monthly_table = monthly_table[!(monthly_table$user_id %in% c("demo_user","NONE","none")),]
monthly_table = monthly_table[!is.na(monthly_table$user_id),]
# remove web users
user_tbl <- get_user_table(db)
web_user_ids <- user_tbl[user_tbl$user_type=="web",]$user_id
monthly_table = monthly_table[!(monthly_table$user_id %in% web_user_ids),]

# specify date range
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2014-12-31")
monthly_table <- subset(monthly_table, date_first_visit >= start_date & date_last_visit <= end_date)

# add self-start data from domain table
self_starts <- domain_table[!is.na(domain_table$internal.self_started),c("name","internal.self_started")]
names(self_starts) <-c("domain","self_started")
monthly_table <- merge(monthly_table,self_starts,by="domain",all.x=T,all.y=F)

# only Android or Nokia users
monthly_table <- monthly_table %>% filter(summary_device_type %in% c('Android','Nokia'))

#----------------------------------------------------------------------------------------#
# get month 6 data only
m6_data <- monthly_table %>% filter(numeric_index == 6)
# get which users active in month 6 were active in month 9
m9_active_users <- monthly_table %>% filter(numeric_index == 9) %>% select(domain,user_id)
dropouts <- anti_join(m6_data,m9_active_users,by=c("domain","user_id")) %>% select(domain,user_id)
dropouts$active_9 <- F
m6_data <- merge(m6_data, dropouts, by=c("domain","user_id"), all.x=T, all.y=F)
m6_data[is.na(m6_data$active_9),]$active_9 <- T

# only domains with > n users in month 6
m6_users <- m6_data %>% group_by(domain) %>% summarise(total_users = length(unique(user_id))) %>%
  filter (total_users > 3)
m6_data <- m6_data %>% filter(domain %in% m6_users$domain)

m6_by_domain <- m6_data %>% group_by(domain) %>%
  summarise(first_active_month_start = min(month_start),
            total_users = length(unique(user_id)),
            percent_active_next_3 = (sum(active_9) / length(unique(user_id)))*100,
            percent_android = (length(unique(user_id[summary_device_type=="Android"])) / length(unique(user_id)))*100,
            most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))
m6_by_domain <- merge(m6_by_domain,self_starts,by="domain",all.x=T,all.y=F)

# take out domains that are less than 9 months old
m6_by_domain <- m6_by_domain %>% 
  filter(first_active_month_start<as.Date('2014-05-01'))
table(m6_by_domain$most_common_device, m6_by_domain$self_started,dnn=c("device","self-started"))

m6_by_domain$most_common_device <- as.factor(m6_by_domain$most_common_device)
kruskal.test(percent_active_next_3~most_common_device,data=m6_by_domain)
m6_by_domain$self_started <- as.factor(m6_by_domain$self_started)
kruskal.test(percent_active_next_3~self_started,data=m6_by_domain)


#----------------------------------------------------------------------------------------#
# get month 2 data only
m2_data <- monthly_table %>% filter(numeric_index == 2)
# get which users active in month 2 were active in month 5
m5_active_users <- monthly_table %>% filter(numeric_index == 5) %>% select(domain,user_id)
dropouts <- anti_join(m2_data,m5_active_users,by=c("domain","user_id")) %>% select(domain,user_id)
dropouts$active_5 <- F
m2_data <- merge(m2_data, dropouts, by=c("domain","user_id"), all.x=T, all.y=F)
m2_data[is.na(m2_data$active_5),]$active_5 <- T

# only domains with > n users in month 2
m2_users <- m2_data %>% group_by(domain) %>% summarise(total_users = length(unique(user_id))) %>%
  filter (total_users > 3)
m2_data <- m2_data %>% filter(domain %in% m2_users$domain)

m2_by_domain <- m2_data %>% group_by(domain) %>%
  summarise(first_active_month_start = min(month_start),
            total_users = length(unique(user_id)),
            percent_active_next_3 = (sum(active_5) / length(unique(user_id)))*100,
            percent_android = (length(unique(user_id[summary_device_type=="Android"])) / length(unique(user_id)))*100,
            most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))
m2_by_domain <- merge(m2_by_domain,self_starts,by="domain",all.x=T,all.y=F)

# take out domains that are less than 5 months old
m2_by_domain <- m2_by_domain %>% 
  filter(first_active_month_start<as.Date('2014-10-01'))
table(m2_by_domain$most_common_device, m2_by_domain$self_started,dnn=c("device","self-started"))

m2_by_domain$most_common_device <- as.factor(m2_by_domain$most_common_device)
kruskal.test(percent_active_next_3~most_common_device,data=m2_by_domain)
m2_by_domain$self_started <- as.factor(m2_by_domain$self_started)
kruskal.test(percent_active_next_3~self_started,data=m2_by_domain)



