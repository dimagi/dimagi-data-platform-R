#Data for Dag Holmboe
#unique_domains <- unique(all_monthly$domain)
#sample_domains <- sample(unique_domains, 9)
sample_domains <- c("crs-senegal", "cahabon", "teba-hbc", 
"nyu-corrections-study", "matts-sandbox", "ucla-dermatology", "m4change", 
"adra", "mvp-mayange")

sample_monthly <- all_monthly[all_monthly$domain %in% sample_domains,]

month_count <- sample_monthly %.%
    group_by(domain, user_pk) %.%
    summarise(months_on_cc = length(unique(calendar_month)))
month_count <- filter(month_count, months_on_cc > 1)

#Keep user with >1 month on CC
sample_monthly <- 
    sample_monthly[sample_monthly$user_pk %in% month_count$user_pk,]

#Domains with >= 5 users with > 1 month on CC
cc_tenure <- month_count %.%
    group_by(domain) %.%
    summarise(nusers = length(unique(user_pk)))
cc_tenure <- filter(cc_tenure, nusers >= 5)

#Create dataset only from these domains for Dag
sample_monthly <- 
    sample_monthly[sample_monthly$domain %in% cc_tenure$domain,]

#Inititalize sample_users vector
sample_users <- c()

#Sample for 5 users from each of these domains
single_domain <- 
    sample_monthly[sample_monthly$domain == "ucla-dermatology",]
users <- sample(unique(single_domain$user_pk), 5)
sample_users <- append(sample_users, users)

#Convert to factor vector
#sample_users <- as.factor(sample_users)

#Create final dataset for Dag
sample_monthly <- 
    sample_monthly[sample_monthly$user_pk %in% sample_users,]

drops <- c("row.names", "active_days_percent", 
           "median_time_elapsed_btw_visits", "batch_entry_visit", 
           "batch_entry_percent", "user_numeric", "sample_undefined", 
           "sample_normal", "active", "domain", "register_followup", 
           "case_register_followup_rate", "morning", "afternoon", 
           "night", "after_midnight", "ncases_opened", "total_logs", 
           "audio_plays", "network_warnings", "diff_days")

sample_monthly <- sample_monthly[,!(names(sample_monthly) %in% drops)]
#names(sample_monthly)[names(sample_monthly) == "obsnum"] = "month_index"

#Calculate differences between month_index to calculate next_month_active
#previous_month_active
sample_monthly <- arrange(sample_monthly, domain_numeric, user_id, 
                          calendar_month)
df <- data.table(sample_monthly)
#Can we setkey by domain and user_id since some user_ids might be the same?
setkey(df,user_id)
df[,diff:=c(NA,diff(calendar_month)),by=user_id]  
dag_data <- as.data.frame(df)
dag_data$previous_month_active <- dag_data$diff <= 31

users <- unique(dag_data$user_id)
next_month_active <- c()
for (i in users) {
    single_user <- dag_data[dag_data$user_id == i,]
    next_active <- c()
    next_active <- append(single_user$previous_month_active[-1], F)
    next_month_active <- append(next_month_active, next_active)
}
dag_data$next_month_active <- next_month_active

#If calendar_month = 8/1/14 then next_month_active = NA
#because we don't know if the user will be active in the following month
is.na(dag_data$next_month_active) <- dag_data$calendar_month == "2014-08-01"

write.csv(sample_monthly, file="sample_monthly.csv")

#------------------------------------------------------------------------#
#Create previous/next month active for all_monthly (full) dataset
#------------------------------------------------------------------------#

#Calculate differences between month_index to calculate next_month_active
#previous_month_active
all_monthly <- arrange(all_monthly, domain_numeric, user_pk, calendar_month)
df <- data.table(all_monthly)
#Can we setkey by domain and user_id since some user_ids might be the same?
setkey(df,user_id)
df[,diff:=c(NA,diff(calendar_month)),by=user_id]  
all_monthly <- as.data.frame(df)
all_monthly$previous_month_active <- all_monthly$diff <= 31

users <- unique(all_monthly$user_id)
next_month_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_id == i,]
  next_active <- c()
  next_active <- append(single_user$previous_month_active[-1], F)
  next_month_active <- append(next_month_active, next_active)
}
all_monthly$next_month_active <- next_month_active

#If calendar_month = 8/1/14 then next_month_active = NA
#because we don't know if the user will be active in the following month
is.na(all_monthly$next_month_active) <- all_monthly$calendar_month == "2014-08-01"

#------------------------------------------------------------------------#
# 4/28/15: Research for domain with domain_numeric = 60
#------------------------------------------------------------------------#
#Get user_pks for list of user_ids supplied by Jolani
#These user_ids are nurse users who Dag will look at further

jolani_user_ids <- c("d7f5386a997c4f8396ca56f93189a4b2",
                     "d7f5386a997c4f8396ca56f931899c83",
                     "d7f5386a997c4f8396ca56f93189a7f9",
                     "d7f5386a997c4f8396ca56f93189d6a4",
                     "d7f5386a997c4f8396ca56f93189cfa7",
                     "d7f5386a997c4f8396ca56f93189c923",
                     "d7f5386a997c4f8396ca56f93189b113",
                     "add6e1de5a30188238f8a6dd1a74e4da",
                     "d7f5386a997c4f8396ca56f93189c401",
                     "d7f5386a997c4f8396ca56f93189bd44")

jolani_user_pk <- data.frame(filter(user_type, user_id %in% jolani_user_ids)$user_pk)
write.csv(jolani_user_pk, file = "nurse_user_pk_domain_60.csv")

