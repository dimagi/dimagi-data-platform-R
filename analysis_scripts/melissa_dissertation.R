library(dplyr)
library(zoo)
library(lme4)
library(influence.ME)
library(lmerTest)
source("mer-utils.R")
library(MuMIn)

library(ggplot2)

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2014-12-31")

# probabilities from log-odds
prob <- function(x) {return (exp(x)/(1+exp(x)))}

# geometric mean
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# add an index for how many months the domain has been on CC
add_domain_month_index<-function(monthly_table) {
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
  
  # add organization
  get_org <- function (domain_name) {
    if (grepl('-',domain_name)) return (unlist(strsplit(domain_name,'-'))[1]) else return (domain_name)
  }
  monthly_table$org <- sapply(as.character(monthly_table$domain), get_org)
  
  return(monthly_table)
}

get_visits_data <- function(monthly_table, n_month, exclude_names=list(),exclude_na = T){
  # get month n data only
  month.n.data <- monthly_table %>% filter(numeric_index == n_month)%>% 
    select(domain,org,user_id,nvisits,self_started,summary_device_type)
  
  # remove rows where any value is NA
  if (exclude_na==T) {month.n.data <- na.omit(month.n.data)}
  # filter out domains on the exclude list
  month.n.data <- month.n.data %>% filter(!(domain %in% exclude_names))
  
  # log transform
  month.n.data$log_visits = log(month.n.data$nvisits)
  
  return(month.n.data)
}

get_visits_models <- function(visits_data) {
  
  month.n.data <- visits_data

  m.visits.null <- lmer(log_visits ~ 1+ (1|domain) + (1|org), data = month.n.data, REML=T)
  m.visits.self_started <- lmer(log_visits ~ self_started + (1|domain) + (1|org), data = month.n.data, REML=T)
  m.visits.device <- lmer(log_visits ~ summary_device_type + (1|domain) + (1|org), data = month.n.data,
                          REML=T)
  m.visits.full <- lmer(log_visits ~ summary_device_type + self_started+ (1|domain) + (1|org), data = month.n.data,
                        REML=T)
  
  ret <- c(m.visits.full, m.visits.self_started,m.visits.device,m.visits.null)
  names(ret) <- c("full","no_device","no_self_started","null")
  return (ret)
  
}

get_retention_data <- function(monthly_table, n_month, diff, exclude_names = list(),exclude_na = T){
  # get month n data only
  month.n.data <- monthly_table %>% filter(numeric_index == n_month) %>% 
    select(domain,org,user_id,month.index, month_start,self_started,summary_device_type)
  
  # remove rows where any value is NA
  if (exclude_na==T) {month.n.data <- na.omit(month.n.data)}
  
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
  
  return(month.n.data)
}

get_retention_models <- function(retention_data){
  
  month.n.data <- retention_data
  
  # models for retention
  m.retention.null <- glmer(active_diff ~ 1+ (1|domain)+ (1|org), data = month.n.data, family=binomial(link=logit),
                            glmerControl(optimizer = "bobyqa"))
  m.retention.self_started <- glmer(active_diff ~ self_started + (1|domain)+ (1|org), data = month.n.data, 
                                    family=binomial(link=logit),glmerControl(optimizer = "bobyqa"))
  m.retention.device <- glmer(active_diff ~ summary_device_type + (1|domain)+ (1|org), data = month.n.data,
                             family=binomial(link=logit),glmerControl(optimizer = "bobyqa"))
  m.retention.full <- glmer(active_diff ~ summary_device_type + self_started+ (1|domain)+ (1|org), data = month.n.data,
                           family=binomial(link=logit),glmerControl(optimizer = "bobyqa"))
  
  ret <- c(m.retention.full, m.retention.self_started,m.retention.device,m.retention.null)
  names(ret) <- c("full","no_device","no_self_started","null")
  return (ret)
}

influential_domains <- function (model, parameters=c(2,3)){
  estex.m.visits <- influence(model, "domain")
  dfb <- dfbetas(estex.m.visits)
  cutoff <- 2 / sqrt(length(unique(slot(model, "frame")$domain)))
  plot(estex.m.visits,
       which="dfbetas",
       parameters=parameters,
       xlab="DFbetaS",
       ylab="Domain")
  
  # based on DFBETAS, which groups should we remove?
  dfb <- as.data.frame(dfb)
  for (i in parameters) {
    dfb$remove <- abs(dfb[,i]) > cutoff
    print(sprintf("cutoff is %s",cutoff))
    if (length(dfb$remove) > 0) { print(dfb[dfb$remove2==T,])}
  }
}

source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
domain_table <- read.csv("domain_table.csv")
monthly_table <- read.csv("monthly.csv")
user_table <- read.csv("user_table.csv")

monthly_table <- clean_monthly_table(monthly_table,domain_table,user_table)

# visits models for month 6
m6.data <- get_visits_data(monthly_table,6)
m6.models <- get_visits_models(m6.data)

anova(m6.models$full,m6.models$no_device)
anova(m6.models$full,m6.models$no_self_started)

influential_domains(m6.models$no_self_started,parameters=c(2))

summary(m6.models$no_self_started)
exp(fixef(m6.models$no_self_started))
exp(confint(m6.models$no_self_started, level = 0.95, method="profile"))
r.squaredGLMM(m6.models$no_self_started)

# visits models for month 12
m12.data <- get_visits_data(monthly_table,12)
m12.models <- get_visits_models(m12.data)

anova(m12.models$full,m12.models$no_device)
anova(m12.models$full,m12.models$no_self_started)

influential_domains(m12.models$no_self_started, parameters=c(2))

summary(m12.models$no_self_started)
exp(fixef(m12.models$no_self_started))
exp(confint(m12.models.rev$no_self_started, level = 0.95, method="profile"))
r.squaredGLMM(m12.models$no_self_started)

# visits models for month 18
m18.data <- get_visits_data(monthly_table,18)
m18.models <- get_visits_models(m18.data)

anova(m18.models$full,m18.models$no_device)
anova(m18.models$full,m18.models$no_self_started)
anova(m18.models$full,m18.models$null)

summary(m18.models$null)
r.squaredGLMM(m18.models$null)

# retention for month 6 (users still active at month 12)
m6.ret.data <- get_retention_data(monthly_table,6,6)
m6.ret.models <- get_retention_models(m6.ret.data)
anova(m6.ret.models$full,m6.ret.models$no_device)
anova(m6.ret.models$full,m6.ret.models$no_self_started)
anova(m6.ret.models$full,m6.ret.models$null)

summary(m6.ret.models$null)
r.squaredGLMM(m6.ret.models$null)

# retention for month 2 (users still active at month 5)
m2.ret.data <- get_retention_data(monthly_table,2,3)
m2.ret.models <- get_retention_models(m2.ret.data)
anova(m2.ret.models$full,m2.ret.models$no_device)
anova(m2.ret.models$full,m2.ret.models$no_self_started)
influential_domains(m2.ret.models$full, parameters=c(2))

summary(m2.ret.models$full)
exp(confint(m2.ret.models$full, level = 0.95, method="boot"))
exp(fixef(m2.ret.models$full))
r.squaredGLMM(m2.ret.models$full)

# plots
cbPalette <- c("#56B4E9", "#D55E00")
cbPalette2 <- c("#0072B2", "#F0E442")
m2.ret.by.domain <- m2.ret.data %>% group_by(domain, self_started) %>% 
  summarise(retention_percent = (sum(active_diff)/n()*100),
            nusers = n(),
            most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))
ggplot(m2.ret.by.domain,aes(x=domain,y=retention_percent,color=most_common_device)) +
  geom_point(aes(size=nusers)) + labs(x = "Project", y = "Percent of users retained", 
                                      size="Total users", color = "Device type") +
  scale_size(trans="log10") +
  scale_colour_manual(values=cbPalette) +
  theme(axis.text.x = element_blank())

m6.ret.by.domain <- m6.ret.data %>% group_by(domain, self_started) %>% 
  summarise(retention_percent = (sum(active_diff)/n()*100),
            nusers = n(),
            most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))
ggplot(m6.ret.by.domain,aes(x=domain,y=retention_percent,color=self_started)) +
  geom_point(aes(size=nusers)) + labs(x = "Project", y = "Percent of users retained", 
                                      size="Total users", color = "Self-started?") +
  scale_size(trans="log10") +
  scale_colour_manual(values=cbPalette2) +
  theme(axis.text.x = element_blank())

ggplot(m6.data,aes(x=domain,y=nvisits,color=summary_device_type)) +
  geom_point() +
  labs(x = "Project", y = "Number of visits", color="Device type") +
  scale_colour_manual(values=cbPalette) +
  theme(axis.text.x = element_blank())

m6.by.domain<- m6.data.rev %>% group_by(domain, self_started) %>% 
  summarise(total_users = length(unique(user_id)),
            median_visits = median(nvisits),
            mean_visits = mean(nvisits),
            gm_mean_visits = gm_mean(nvisits),
            percent_android = (length(unique(user_id[summary_device_type=="Android"])) / length(unique(user_id)))*100,
            most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))

m6.plot<-ggplot(m6.by.domain,aes(x=domain,y=median_visits,color=most_common_device)) +
  geom_point(aes(size=total_users)) + labs(x = "Project", y = "Median visits", 
                                      size="Total users", color = "Device type") +
  scale_size(trans="log10") +
  scale_colour_manual(values=cbPalette2) +
  theme(axis.text.x = element_blank())

m12.by.domain<- m12.data %>% group_by(domain, self_started) %>% 
  summarise(total_users = length(unique(user_id)),
            median_visits = median(nvisits),
            mean_visits = mean(nvisits),
            gm_mean_visits = gm_mean(nvisits),
            percent_android = (length(unique(user_id[summary_device_type=="Android"])) / length(unique(user_id)))*100,
            most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))

m12.plot<-ggplot(m12.by.domain,aes(x=domain,y=median_visits,color=most_common_device)) +
  geom_point(aes(size=total_users)) + labs(x = "Project", y = "Median visits", 
                                           size="Total users", color = "Device type") +
  scale_size(trans="log10") +
  scale_colour_manual(values=cbPalette2) +
  theme(axis.text.x = element_blank())

m18.by.domain<- m18.data %>% group_by(domain, self_started) %>% 
  summarise(total_users = length(unique(user_id)),
            median_visits = median(nvisits),
            mean_visits = mean(nvisits),
            gm_mean_visits = gm_mean(nvisits),
            percent_android = (length(unique(user_id[summary_device_type=="Android"])) / length(unique(user_id)))*100,
            most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))

m18.plot<-ggplot(m18.by.domain,aes(x=domain,y=median_visits,color=most_common_device)) +
  geom_point(aes(size=total_users)) + labs(x = "Project", y = "Median visits", 
                                           size="Total users", color = "Device type") +
  scale_size(trans="log10") +
  scale_colour_manual(values=cbPalette2) +
  theme(axis.text.x = element_blank())


# org mapping debug
org_mapping <- unique(monthly_table[monthly_table$domain != monthly_table$org,c('domain','org')])
org_mapping$multi <- org_mapping$org %in% org_mapping$org[duplicated(org_mapping$org)]
m6.orgs<-m6.data[m6.data$org %in% org_mapping[org_mapping$multi==T,c('org')],]

ggplot(m6.orgs,aes(x=domain,y=nvisits,color=org,shape=summary_device_type)) +
  geom_point() +
  labs(x = "Project", y = "Number of visits", color="Organisation", shape="Device Type") +
  theme(axis.text.x = element_blank())

m6.orgs.summary <- m6.orgs %>% group_by(org,domain) %>% summarize(median_visits = median(nvisits))

ggplot(m6.orgs.summary,aes(x=domain,y=median_visits,fill=org)) +
  geom_bar(stat="identity") +
  labs(x = "Project", y = "Median visits", color="Organisation") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



