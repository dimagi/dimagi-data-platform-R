library(dplyr)
library(googleVis)
library(rCharts)

# load config files
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))
source(file.path("config_setup.R", fsep = .Platform$file.sep))
run_conf <-get_run_config(config_run_path)

#Get db connection
db <- get_db_connection(system_conf)

# get domain table from db
source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
domain_table <- get_domain_table(db)

# get the domains to run on based on the filters/names in run conf
domains_for_run <- get_domains_for_run(domain_table,run_conf)

source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
monthly_table <- get_aggregate_table (db, "aggregate_monthly_interactions", domains_for_run)

# remove demo users and NA/NONE users
all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
all_monthly = all_monthly[!(all_monthly$user_id =="NONE"),]
all_monthly = all_monthly[!(all_monthly$user_id =="none"),]
all_monthly = all_monthly[!is.na(all_monthly$user_id),]

# use date range from config_run
start_date <- as.Date(run_conf$reports$start_date)
end_date <- as.Date(run_conf$reports$end_date)
monthly_table <- subset(monthly_table, date_first_visit >= start_date & date_last_visit <= end_date)

monthly_table$month_start<-as.Date(paste0('1 ',monthly_table$month.index), '%d %b %Y')
monthly_table$month_start_numeric <- as.numeric(as.POSIXct(monthly_table$month_start))
source(file.path("aggregate_tables","monthly_table_functions.R", fsep = .Platform$file.sep))
monthly_table <- add_next_previous_active(monthly_table)


# summary by device by month
summary_table_by_device <- monthly_table %>% 
  group_by(month_start_numeric, summary_device_type) %>% 
  summarise(total_forms = sum(nforms), total_visits=sum(nvisits), total_users=length(unique(user_id)),)

write.csv(summary_table_by_device,'device_summary_table.csv')

# Rickshaw chart by user
all_months <- unique(summary_table_by_device$month_start_numeric)
all_dev_types <- unique(summary_table_by_device$summary_device_type)
full_set <- merge(all_months,all_dev_types, all=T)
names(full_set) <- c('month_start_numeric','summary_device_type')
full_set <- full_set[order(full_set$month_start_numeric,full_set$summary_device_type),]
full_set$value <- 0
full_set$variable <- 'total_users'
dev_long = reshape2::melt(summary_table_by_device[c('summary_device_type','month_start_numeric','total_users')],
                          id.vars=c('summary_device_type','month_start_numeric'))
dev_long<-replace.df(full_set, dev_long,by=c('month_start_numeric','summary_device_type','variable'))

chart <- Rickshaw$new()

chart$layer(value ~ month_start_numeric, group = "summary_device_type", 
            data = dev_long, type = "area", width = 560)
chart$set(slider = TRUE)
chart$save('rickshaw_chart.html',  standalone = TRUE)

# 6-month comparison between nokia and android users
six_month_comparison_data <- monthly_table %>% filter(numeric_index == 6, summary_device_type %in% c('Android','Nokia'))
six_month_summary <- six_month_comparison_data %>% group_by(summary_device_type) %>%
  summarise(total_users = length(unique(user_id)),
            total_domains = length(unique(domain)),
            mean_forms = mean(nforms), 
            mean_visits = mean(nvisits), 
            mean_active_days = mean(active_days),
            avg_time_using_cc = mean(time_using_cc), 
            number_active_next_3 = sum(next_three_months_active))

# anova for users - not really valid though, observations are not independent within a project
six_month_aov = aov(nforms~summary_device_type,data=six_month_comparison_data)
summary(six_month_aov)
print(model.tables(six_month_aov,"means"),digits=3)   

# summary by domain
six_month_by_domain <- six_month_comparison_data %>% group_by(domain) %>%
  summarise(total_users = length(unique(user_id)),
            total_forms = sum(nforms),
            mean_forms_per_user = mean(nforms),
            median_forms_per_user = median(nforms),
            total_visits = sum(nvisits),
            mean_visits_per_user = mean(nvisits),
            median_visits_per_user = median(nvisits),
            most_common_device = names(sort(table(summary_device_type),decreasing=TRUE)[1]))
six_month_by_domain <- six_month_by_domain %>% filter(total_users >= 5)

# kruskal-wallis non-parametric test for domains
six_month_by_domain$most_common_device <- as.factor (six_month_by_domain$most_common_device)
kruskal.test(mean_forms_per_user~most_common_device,data=six_month_by_domain) 


# summary by domain

monthly_table$month_start<-as.Date(paste0('1 ',monthly_table$month.index), '%d %b %Y')
summary_by_domain <- as.data.frame (monthly_table %>% 
  group_by(domain, month_start, month_start_numeric) %>% 
  summarise(total_forms = sum(nforms), total_visits=sum(nvisits), total_users=length(unique(user_id)), 
            most_common_device=names(sort(table(summary_device_type),decreasing=TRUE)[1]), 
            median_active_days = median(active_days), median_time_using_cc = median(time_using_cc),
            android_users = length(unique(user_id[summary_device_type=="Android"])), 
            nokia_users = length(unique(user_id[summary_device_type=="Nokia"]))))
summary_by_domain$percent_android = 
  summary_by_domain$android_users / (summary_by_domain$total_users) * 100
summary_by_domain$percent_nokia = 
  summary_by_domain$nokia_users / (summary_by_domain$total_users) * 100

mixed_domains <- subset(summary_by_domain, percent_android > 25 & percent_nokia > 25 & total_users > 5)
domains_by_month <- summary_by_domain %>% 
  group_by (month_start_numeric, most_common_device) %>%
  summarise(domains_total=length(domain))

# Rickshaw chart by domain
all_months <- unique(domains_by_month$month_start_numeric)
all_dev_types <- unique(domains_by_month$most_common_device)
full_set <- merge(all_months,all_dev_types, all=T)
names(full_set) <- c('month_start_numeric','most_common_device')
full_set <- full_set[order(full_set$month_start_numeric,full_set$most_common_device),]
full_set$value <- 0
full_set$variable <- 'domains_total'
dev_long = reshape2::melt(domains_by_month,
                          id.vars=c('most_common_device','month_start_numeric'))
dev_long<-replace.df(full_set, dev_long,by=c('month_start_numeric','most_common_device','variable'))

chart_by_domain <- Rickshaw$new()

chart_by_domain$layer(value ~ month_start_numeric, group = "most_common_device", 
            data = dev_long, type = "area", width = 560)
chart_by_domain$set(slider = TRUE)
chart_by_domain$save('rickshaw_chart_by_domain.html',  standalone = TRUE)

# % Android users by country
monthly_country <- merge(monthly_table, domain_table[c('name','deployment.country')], by.x='domain', by.y='name')
users_by_country_by_month <- monthly_country %>% 
  group_by(month_start, month_start_numeric, deployment.country) %>% 
  summarise(total_projects = length(unique(domain)),
            total_forms = sum(nforms), total_visits=sum(nvisits), total_users=length(unique(user_id)), 
            android_users =length(unique(user_id[summary_device_type=="Android"])))
users_by_country_by_month$percent_android = 
  users_by_country_by_month$android_users / (users_by_country_by_month$total_users) * 100

cleaned <- users_by_country_by_month[!(users_by_country_by_month$deployment.country == 'None'),]
cleaned$hovervar <- sprintf('%s: %d projects, %d users (%d Android)',cleaned$deployment.country,
                            cleaned$total_projects, cleaned$total_users, cleaned$android_users)
Sept_2012 <-  cleaned[cleaned$month_start==as.Date(as.POSIXct('2012-09-01')),]
Sept_2013 <-  cleaned[cleaned$month_start==as.Date(as.POSIXct('2013-09-01')),]
Sept_2014 <-  cleaned[cleaned$month_start==as.Date(as.POSIXct('2014-09-01')),]


Sept_2012_chart <- gvisGeoChart(Sept_2012, 
                               locationvar='deployment.country', 
                               colorvar='percent_android', hovervar="hovervar",
                               options=list(width=800, height=600,
                                            colorAxis="{colors: ['#e7711c', '#4374e0']}"))
plot(Sept_2012_chart)
cat(Sept_2012_chart$html$chart, file="Sept_2012_chart.html")

Sept_2014_chart <- gvisGeoChart(Sept_2014, 
                               locationvar='deployment.country', 
                               colorvar='percent_android', hovervar="hovervar",
                               options=list(width=800, height=600,
                                            colorAxis="{colors: ['#e7711c', '#4374e0']}"))
plot(Sept_2014_chart)
cat(Sept_2014_chart$html$chart, file="Sept_2014_chart.html")
