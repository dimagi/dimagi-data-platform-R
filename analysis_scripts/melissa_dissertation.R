library(dplyr)
library(zoo)
library(lme4)
library(influence.ME)

# load config files
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))

#db <- get_db_connection(system_conf)

# load domain attributes and monthly aggregate table, but only keep domains where data use is permitted
source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
domain_table <- read.csv("domain_table.csv")
domain_table <- domain_table[domain_table$name %in% get_permitted_domains(domain_table),]


#monthly_table <- collect(tbl(db, "aggregate_monthly_interactions"))
monthly_table <- read.csv("monthly.csv")
monthly_table <- monthly_table[monthly_table$domain %in% get_permitted_domains(domain_table),]

monthly_table$month_start<-as.Date(paste0('1 ',monthly_table$month.index), '%d %b %Y')

# remove test projects
not_test_projects <- domain_table %>% filter(test==F) %>% select(name)
monthly_table <- monthly_table %>% filter(domain %in% not_test_projects$name)

# remove demo users and NA/NONE users
monthly_table = monthly_table[!(monthly_table$user_id %in% c("demo_user","NONE","none")),]
monthly_table = monthly_table[!is.na(monthly_table$user_id),]
# remove web users
#user_tbl <- get_user_table(db)
user_tbl <- read.csv("user_table.csv")
web_user_ids <- user_tbl[user_tbl$user_type=="web",]$user_id
monthly_table = monthly_table[!(monthly_table$user_id %in% web_user_ids),]

# specify date range
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2014-12-31")
monthly_table <- subset(monthly_table, date_first_visit >= start_date & date_last_visit <= end_date)

# only Android or Nokia users
monthly_table <- monthly_table %>% filter(summary_device_type %in% c('Android','Nokia'))

# add self-start and creating user data from domain table
self_starts <- domain_table %>% select (name, internal.self_started, creating_user)
names(self_starts) <-c("domain","self_started","creating_user")
monthly_table <- merge(monthly_table,self_starts,by="domain",all.x=T,all.y=F)

# tests whether various domain attributes effect retention percentage
# retention percentage is the number of users submitting data in month n who also submit data in month n+diff
test_retention_predictors <- function(monthly_table, n_month, diff, end_date) {
  # get month n data only
  month.n.data <- monthly_table %>% filter(numeric_index == n_month)
  # get which users active in month n were active in month n+diff
  mdiff_active_users <- monthly_table %>% filter(numeric_index == n_month+diff) %>% select(domain,user_id)
  dropouts <- anti_join(month.n.data,mdiff_active_users,by=c("domain","user_id")) %>% select(domain,user_id)
  dropouts$active_diff <- F
  month.n.data <- merge(month.n.data, dropouts, by=c("domain","user_id"), all.x=T, all.y=F)
  month.n.data[is.na(month.n.data$active_diff),]$active_diff <- T
  
  # only domains with > 3 users in month n
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

get_visits_models <- function(monthly_table, n_month, exclude_names=list()){
  # get month n data only
  month.n.data <- monthly_table %>% filter(numeric_index == n_month)
  
  # filter out domains on the exclude list
  month.n.data <- month.n.data %>% filter(!(domain %in% exclude_names))
  
  # only domains with > 3 users in month n
  month.n.domains <- month.n.data %>% group_by(domain) %>% summarise(total_users = length(unique(user_id))) %>%
    filter (total_users > 3)
  month.n.data <- month.n.data %>% filter(domain %in% month.n.domains$domain)
  
  # summary tables
  month.n.by.domain <- month.n.data %>% group_by(domain, self_started) %>% summarise(total_users = length(unique(user_id)),
                                                                                     median_forms = median(nforms),
                                                                                     median_visits = median(nvisits),
                                                                                     percent_android = (length(unique(user_id[summary_device_type=="Android"])) / length(unique(user_id)))*100,
                                                                                     most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))
  month.n.by.domain$self_started <- as.factor(month.n.by.domain$self_started)
  month.n.by.domain$most_common_device <- as.factor(month.n.by.domain$most_common_device)
  most_common_device.summary <- month.n.by.domain %>% group_by(most_common_device) %>% 
    summarise(median(median_visits), median(median_forms), total_domains=n())
  self_started.summary <- month.n.by.domain %>% group_by(self_started) %>% 
    summarise(median(median_visits), median(median_forms), total_domains=n())
  print(self_started.summary)
  print(most_common_device.summary)
  
  # models for nvisits
  month.n.data$log_visits = log(month.n.data$nvisits)
  
  m.visits.null <- lmer(log_visits ~ 1+ (1|domain), data = month.n.data,REML=FALSE)
  m.visits.self_started <- lmer(log_visits ~ self_started + (1|domain), data = month.n.data,
                                REML=FALSE)
  m.visits.device <- lmer(log_visits ~ summary_device_type + (1|domain), data = month.n.data,
                          REML=FALSE)
  m.visits.full <- lmer(log_visits ~ summary_device_type + self_started+ (1|domain), data = month.n.data,
                        REML=FALSE)
  
  ret <- c(m.visits.full, m.visits.self_started,m.visits.device,m.visits.null)
  names(ret) <- c("full","no_device","no_self_started","null")
  return (ret)
  
}

influential_domains <- function (model){
  estex.m.visits <- influence(model, "domain")
  dfb <- dfbetas(estex.m.visits)
  cutoff <- 2 / sqrt(length(unique(slot(model, "frame")$domain)))
  plot(estex.m.visits,
       which="dfbetas",
       parameters=c(2,3),
       xlab="DFbetaS",
       ylab="Domain")
  
  # based on DFBETAS, which groups should we remove?
  dfb <- as.data.frame(dfb)
  dfb$remove2 <- abs(dfb[,2]) > cutoff
  print(sprintf("cutoff is %s",cutoff))
  if (length(dfb$remove2) > 0) { print(dfb[dfb$remove2==T,])}
  dfb$remove3 <- abs(dfb[,3]) > cutoff
  if (length(dfb$remove3) > 0) {print(dfb[dfb$remove3==T,])}
}

#test_retention_predictors(monthly_table, 2, 3, end_date)
#test_retention_predictors(monthly_table, 6, 3, end_date)


# visits models for month 6
m6.models <- get_visits_models(monthly_table,6)

anova(m6.models$full,m6.models$no_device)
anova(m6.models$full,m6.models$no_self_started)
summary(m6.models$full)

influential_domains(m6.models$full)

m6.models.rev <- get_visits_models(monthly_table,6,c("crs-mip","m4change","project"))
anova(m6.models.rev$full,m6.models.rev$no_device)
anova(m6.models.rev$full,m6.models.rev$no_self_started)
summary(m6.models.rev$full)

influential_domains(m6.models.rev$full)

# visits models for month 12
m12.models <- get_visits_models(monthly_table,12)

anova(m12.models$full,m12.models$no_device)
anova(m12.models$full,m12.models$no_self_started)
summary(m12.models$full)

influential_domains(m12.models$full)

m12.models.rev <- get_visits_models(monthly_table,12,c("crs-mip","m4change","project","opm","rmf"))
anova(m12.models.rev$full,m12.models.rev$no_device)
anova(m12.models.rev$full,m12.models.rev$no_self_started)
summary(m12.models.rev$full)

influential_domains(m12.models.rev$full)

# visits models for month 18

m18.models <- get_visits_models(monthly_table,18)

anova(m18.models$full,m18.models$no_device)
anova(m18.models$full,m18.models$no_self_started)
summary(m18.models$full)

influential_domains(m18.models$full)
m18.models.rev <- get_visits_models(monthly_table,18,c("crs-mip","project"))
anova(m18.models.rev$full,m18.models.rev$no_device)
anova(m18.models.rev$full,m18.models.rev$no_self_started)
summary(m18.models.rev$full)



