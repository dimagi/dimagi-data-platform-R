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

#monthly_table$month.index <- as.yearmon(monthly_table$month.index, "%b %Y")
start_date <- as.Date(run_conf$reports$start_date)
end_date <- as.Date(run_conf$reports$end_date)
monthly_table <- subset(monthly_table, date_first_visit >= start_date & date_last_visit <= end_date)

monthly_table$calendar_month_start<-as.Date(paste0('1 ',monthly_table$month.index), '%d %b %Y')
monthly_table$month_start_numeric <- as.numeric(as.POSIXct(monthly_table$calendar_month_start))
summary_table_by_device <- monthly_table %>% 
  group_by(month_start_numeric, summary_device_type) %>% 
  summarise(total_forms = sum(nforms), total_visits=sum(nvisits), total_users=length(unique(user_id)))

write.csv(summary_table_by_device,'device_summary_table.csv')

# Rickshaw chart
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


monthly_table$calendar_month_start<-as.Date(paste0('1 ',monthly_table$month.index), '%d %b %Y')
summary_by_domain <- as.data.frame (monthly_table %>% 
  group_by(domain, calendar_month_start) %>% 
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

# GoogleViz Motion Chart
state_settings <-'
{"orderedByY":false,"xZoomedDataMax":612,
"duration":{"timeUnit":"D","multiplier":1},
"iconType":"BUBBLE","iconKeySettings":[],
"nonSelectedAlpha":0.4,"yZoomedDataMin":1,
"dimensions":{"iconDimensions":["dim0"]},
"yZoomedDataMax":49302,"orderedByX":false,"showTrails":true,
"xLambda":0,"yLambda":0,"xZoomedIn":false,"uniColorForNonSelected":false,
"time":"2012","playDuration":15000,"xAxisOption":"4",
"yAxisOption":"2","sizeOption":"6","yZoomedIn":false,"xZoomedDataMin":1,"colorOption":"5"}'

motion_chart <-gvisMotionChart(summary_by_domain, idvar = "domain", timevar = "calendar_month_start",
                xvar="total_users", yvar="total_forms", colorvar="most_common_device", sizevar="median_active_days",
                date.format = "%Y/%m", 
                options = list(width=1200, height=800, state=state_settings,
                               showSelectListComponent = F, showChartButtons = F))

cat(motion_chart$html$chart, file="motion_chart.html")
plot(motion_chart)

# % Android users by country
monthly_country <- merge(monthly_table, domain_table[c('name','deployment.country')], by.x='domain', by.y='name')
users_by_country_by_month <- monthly_country %>% 
  group_by(calendar_month_start, month_start_numeric, deployment.country) %>% 
  summarise(total_projects = length(unique(domain)),
            total_forms = sum(nforms), total_visits=sum(nvisits), total_users=length(unique(user_id)), 
            android_users =length(unique(user_id[summary_device_type=="Android"])))
users_by_country_by_month$percent_android = 
  users_by_country_by_month$android_users / (users_by_country_by_month$total_users) * 100

cleaned <- users_by_country_by_month[!(users_by_country_by_month$deployment.country == 'None'),]
cleaned$hovervar <- sprintf('%s: %d projects, %d users (%d Android)',cleaned$deployment.country,
                            cleaned$total_projects, cleaned$total_users, cleaned$android_users)
Oct_2012 <-  cleaned[cleaned$calendar_month_start==as.Date(as.POSIXct('2012-10-01')),]
Oct_2013 <-  cleaned[cleaned$calendar_month_start==as.Date(as.POSIXct('2013-10-01')),]
Oct_2014 <-  cleaned[cleaned$calendar_month_start==as.Date(as.POSIXct('2014-10-01')),]


Oct_2012_chart <- gvisGeoChart(Oct_2012, 
                               locationvar='deployment.country', 
                               colorvar='percent_android', hovervar="hovervar",
                               options=list(width=800, height=600,
                                            colorAxis="{colors: ['#e7711c', '#4374e0']}"))
plot(Oct_2012_chart)
cat(Oct_2012_chart$html$chart, file="Oct_2012_chart.html")

Oct_2014_chart <- gvisGeoChart(Oct_2014, 
                               locationvar='deployment.country', 
                               colorvar='percent_android', hovervar="hovervar",
                               options=list(width=800, height=600,
                                            colorAxis="{colors: ['#e7711c', '#4374e0']}"))
plot(Oct_2014_chart)
cat(Oct_2014_chart$html$chart, file="Oct_2014_chart.html")
