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


#monthly_table <- collect(tbl(db, "aggregate_monthly_interactions"))
monthly_table <- read.csv("monthly.csv")
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

# tests whether various domain attributes effect retention percentage
# retention percent is defined as the number of users submitting data in month n who also submit data in month n+diff
test_retention_predictors <- function(monthly_table, n_month, diff, end_date) {
  # get month n data only
  month.n.data <- monthly_table %>% filter(numeric_index == n_month)
  # get which users active in month n were active in month n+diff
  mdiff_active_users <- monthly_table %>% filter(numeric_index == n_month+diff) %>% select(domain,user_id)
  dropouts <- anti_join(month.n.data,mdiff_active_users,by=c("domain","user_id")) %>% select(domain,user_id)
  dropouts$active_diff <- F
  month.n.data <- merge(month.n.data, dropouts, by=c("domain","user_id"), all.x=T, all.y=F)
  month.n.data[is.na(month.n.data$active_diff),]$active_diff <- T
  
  # only domains with > n users in month n
  month.n.domains <- month.n.data %>% group_by(domain) %>% summarise(total_users = length(unique(user_id))) %>%
    filter (total_users > 3)
  month.n.data <- month.n.data %>% filter(domain %in% month.n.domains$domain)
  
  month.n.by.domain <- month.n.data %>% group_by(domain) %>%
    summarise(first_active_month_start = min(month_start),
              total_users = length(unique(user_id)),
              percent_retained = (sum(active_diff) / length(unique(user_id)))*100,
              percent_android = (length(unique(user_id[summary_device_type=="Android"])) / length(unique(user_id)))*100,
              most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))
  
  month.n.by.domain <- merge(month.n.by.domain,self_starts,by="domain",all.x=T,all.y=F)
  
  # take out domains that are less than n+diff months old
  cutoff <- as.Date(as.yearmon(end_date) -((n_month+diff-1)/12), frac = 0)
  month.n.by.domain <- month.n.by.domain %>% 
    filter(first_active_month_start<=cutoff)
  table(month.n.by.domain$most_common_device, month.n.by.domain$self_started,dnn=c("device","self-started"))
  
  month.n.by.domain$most_common_device <- as.factor(month.n.by.domain$most_common_device)
  print(kruskal.test(percent_retained~most_common_device,data=month.n.by.domain))
  most_common_device.summary <- month.n.by.domain %>% group_by(most_common_device) %>% summarise(median(percent_retained), total_domains=n())
  print(most_common_device.summary)
  
  month.n.by.domain$self_started <- as.factor(month.n.by.domain$self_started)
  print(kruskal.test(percent_retained~self_started,data=month.n.by.domain))
  self_started.summary<-month.n.by.domain %>% group_by(self_started) %>% summarise(median(percent_retained), total_domains=n())
  print(self_started.summary)
  
  
}

test_retention_predictors(monthly_table, 2, 3, end_date)
test_retention_predictors(monthly_table, 6, 3, end_date)



