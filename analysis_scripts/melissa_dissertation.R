library(dplyr)
library(zoo)
library(lme4)
library(influence.ME)
library(lmerTest)
source("mer-utils.R")

library(ggplot2)

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2014-12-31")

add_domain_month_index<-function(monthly_table){
  domain_start_dates <- monthly_table %>% group_by (domain) %>% summarise(domain_start_date = min(month_start) )
  monthly_table <- merge(monthly_table,domain_start_dates,by=c("domain"))
  
  # domain's first, second, third... etc. month on CC
  domain_month_index <- function (x) {
    first_possible_visit_date <- as.Date(as.POSIXct(strptime("2010-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")))
    this_month <- min(x$month_start)
    if (this_month < first_possible_visit_date) { return (1) }
    
    start_month <- min(x$domain_start_date)
    if (start_month < first_possible_visit_date) {start_month <- first_possible_visit_date}
    
    total_months <- length(seq(from=start_month, to=this_month, by='month'))
    return (total_months)
  }
  
  domain_indexes <- monthly_table %>% group_by (domain, month.index) %>% do(domain_month_index = domain_month_index(.) )
  monthly_table <- merge(monthly_table,domain_indexes,by=c("domain","month.index"))
  monthly_table$domain_month_index<-monthly_table$domain_month_index[[1]]
  return(monthly_table)
  
  
}

clean_monthly_table <- function(monthly_table,domain_table,user_table) {
  # only keep domains where data use is permitted
  source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
  domain_table <- domain_table[domain_table$name %in% get_permitted_domains(domain_table),]
  monthly_table <- monthly_table[monthly_table$domain %in% get_permitted_domains(domain_table),]
  
  # add user month start
  monthly_table$month_start<-as.Date(paste0('1 ',monthly_table$month.index), '%d %b %Y')
  # add domain month index
  monthly_table<-add_domain_month_index(monthly_table)
  
  # remove test projects
  not_test_projects <- domain_table %>% filter(test==F) %>% select(name)
  monthly_table <- monthly_table %>% filter(domain %in% not_test_projects$name)
  
  # remove demo users and NA/NONE users
  monthly_table = monthly_table[!(monthly_table$user_id %in% c("demo_user","NONE","none")),]
  monthly_table = monthly_table[!is.na(monthly_table$user_id),]
  
  # remove web users
  web_user_ids <- user_table[user_table$user_type=="web",]$user_id
  monthly_table = monthly_table[!(monthly_table$user_id %in% web_user_ids),]
  
  # specify date range
  monthly_table <- subset(monthly_table, date_first_visit >= start_date & date_last_visit <= end_date)
  
  # only Android or Nokia users
  monthly_table <- monthly_table %>% filter(summary_device_type %in% c('Android','Nokia'))
  
  # add self-start and creating user data from domain table
  self_starts <- domain_table %>% select (name, internal.self_started, creating_user)
  names(self_starts) <-c("domain","self_started","creating_user")
  monthly_table <- merge(monthly_table,self_starts,by="domain",all.x=T,all.y=F)
  
  return(monthly_table)
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

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
  
  ggplot(month.n.data ,aes(x=domain,y=nvisits,color=summary_device_type)) +
    geom_point(size=2) 
  ggplot(month.n.data ,aes(x=domain,y=nvisits,color=self_started)) +
    geom_point(size=2) 
  
  # summary tables
  month.n.by.domain <- month.n.data %>% group_by(domain, self_started) %>% summarise(total_users = length(unique(user_id)),
                                                                                     median_forms = median(nforms),
                                                                                     median_visits = median(nvisits),
                                                                                     mean_visits = mean(nvisits),
                                                                                     gm_mean_visits = gm_mean(nvisits),
                                                                                     percent_android = (length(unique(user_id[summary_device_type=="Android"])) / length(unique(user_id)))*100,
                                                                                     most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))
  
  #ggplot(month.n.by.domain,aes(x=domain,y=mean_visits,color=self_started,shape=most_common_device)) +
    geom_point(size=5) 
  
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
  
  m.visits.null <- lmer(log_visits ~ 1+ (1|domain), data = month.n.data, REML=T)
  m.visits.self_started <- lmer(log_visits ~ self_started + (1|domain), data = month.n.data, REML=T)
  m.visits.device <- lmer(log_visits ~ summary_device_type + (1|domain), data = month.n.data,
                          REML=T)
  m.visits.full <- lmer(log_visits ~ summary_device_type + self_started+ (1|domain), data = month.n.data,
                        REML=T)
  
  ret <- c(m.visits.full, m.visits.self_started,m.visits.device,m.visits.null)
  names(ret) <- c("full","no_device","no_self_started","null")
  return (ret)
  
}

get_retention_models <- function(monthly_table, n_month, diff, exclude_names = list()){
  # get month n data only
  month.n.data <- monthly_table %>% filter(numeric_index == n_month)
  
  # get which users active in month n were active in month n+diff
  mdiff_active_users <- monthly_table %>% filter(numeric_index == n_month+diff) %>% select(domain,user_id)
  dropouts <- anti_join(month.n.data,mdiff_active_users,by=c("domain","user_id")) %>% select(domain,user_id)
  dropouts$active_diff <- 0
  month.n.data <- merge(month.n.data, dropouts, by=c("domain","user_id"), all.x=T, all.y=F)
  month.n.data[is.na(month.n.data$active_diff),]$active_diff <- 1
  
  # take out users that are less than n+diff months old
  cutoff <- as.Date(as.yearmon(end_date) -((n_month+diff-1)/12), frac = 0)
  user_start_dates <- month.n.data %>% group_by(user_id,domain) %>% 
    summarise(first_month_start = min(month_start)) %>%
    select(user_id,domain,first_month_start)    
  month.n.data <- merge(month.n.data, user_start_dates,by=c("domain","user_id"),all.x=T,all.y=F)
  month.n.data <- month.n.data %>% filter(first_month_start <= cutoff)
  
  # models for retention
  m.retention.null <- glmer(active_diff ~ 1+ (1|domain), data = month.n.data, family=binomial)
  m.retention.self_started <- glmer(active_diff ~ self_started + (1|domain), data = month.n.data, 
                                    family=binomial)
  m.retention.device <- glmer(active_diff ~ summary_device_type + (1|domain), data = month.n.data,
                             family=binomial)
  m.retention.full <- glmer(active_diff ~ summary_device_type + self_started+ (1|domain), data = month.n.data,
                           family=binomial)
  
  ret <- c(m.retention.full, m.retention.self_started,m.retention.device,m.retention.null)
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

source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
domain_table <- read.csv("domain_table.csv")
monthly_table <- read.csv("monthly.csv")
user_table <- read.csv("user_table.csv")

monthly_table <- clean_monthly_table(monthly_table,domain_table,user_table)


# visits models for month 6
m6.models <- get_visits_models(monthly_table,6)

anova(m6.models$full,m6.models$no_device)
anova(m6.models$full,m6.models$no_self_started)
summary(m6.models$full)
anova(m6.models$full)

influential_domains(m6.models$full)

m6.models.rev <- get_visits_models(monthly_table,6,c("crs-mip","m4change","project"))
anova(m6.models.rev$full,m6.models.rev$no_device)
anova(m6.models.rev$no_self_started,m6.models.rev$full)
anova(m6.models.rev$full, ddf = "Kenward-Roger")

summary(m6.models.rev$full)
exp(fixef(m6.models.rev$full))
exp(confint(m6.models.rev$full, level = 0.95, method="profile"))
vif.mer(m6.models.rev$full)

influential_domains(m6.models.rev$full)

# visits models for month 12
m12.models <- get_visits_models(monthly_table,12)

anova(m12.models$full,m12.models$no_device)
anova(m12.models$full,m12.models$no_self_started)
summary(m12.models$full)

influential_domains(m12.models$full)

m12.models.rev <- get_visits_models(monthly_table,12,c("crs-mip","rmf","m4change","project","opm"))
anova(m12.models.rev$full,m12.models.rev$no_device)
anova(m12.models.rev$full,m12.models.rev$no_self_started)
summary(m12.models.rev$full)
exp(fixef(m12.models.rev$full))
exp(confint(m12.models.rev$full, level = 0.95, method="profile"))

influential_domains(m12.models.rev$full)
vif.mer(m12.models.rev$full)
anova(m12.models.rev$full, ddf = "Kenward-Roger")

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
exp(fixef(m18.models.rev$full))
exp(confint(m18.models.rev$full, level = 0.95, method="profile"))

vif.mer(m18.models.rev$full)

anova(m18.models.rev$full, ddf = "Kenward-Roger")

# retention for month 6 (users still active at month 9)
m6.ret.models <- get_retention_models(monthly_table,6,9)
anova(m6.ret.models$full,m6.ret.models$no_device)
anova(m6.ret.models$full,m6.ret.models$no_self_started)
influential_domains(m6.ret.models$full)

m6.ret.models.rev <- get_retention_models(monthly_table,6,9,c("m4change","project"))
anova(m6.ret.models.rev$full,m6.ret.models.rev$no_device)
anova(m6.ret.models.rev$full,m6.ret.models.rev$no_self_started)

summary(m6.ret.models.rev$no_device)
confint(m6.ret.models.rev$no_device,method="boot")


