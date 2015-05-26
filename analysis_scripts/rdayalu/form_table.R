#How many users have forms in DP for a particular month
#but no monthly rows in the aggregate table? We should monitor
#this list on a regular basis till we figure out the issues
#with the aggregate table build.
form_table$form_date <- as.Date(substr(form_table$time_start, 1, 10))
form_table$form_month <- floor_date(form_table$form_date, "month")
form_months <- unique(form_table$form_month)
aggregate_months <- unique(all_monthly$calendar_month)


#Forms with negative form durations
#Calculate form duration (in minutes)
form_table$form_duration <- as.numeric(form_table$time_end - form_table$time_start)/60

#Flag forms with negative form_duration
form_table$negative_duration <- form_table$form_duration < 0
neg_forms <- filter(form_table, negative_duration == T)
pos_forms <- filter(form_table, negative_duration == F)
pos_forms_le30 <- filter(pos_forms, form_duration <= 30)
neg_forms$absolute_duration_reasonable <- neg_forms
summary(neg_forms$form_duration)
#Histogram
myhist <- ggplot(pos_forms_le30, aes(x=form_duration)) + 
  geom_histogram(binwidth=5, colour="black", fill="lightblue")

#Flag forms with >30 min form duration
form_table$gt_30mins <- form_table$form_duration > 30
gt30_forms <- filter(form_table, gt_30mins == T & form_duration <= 2500)
summary(gt30_forms$form_duration)
#Boxplot
myhist <- ggplot(gt30_forms, aes(x=form_duration)) + 
  geom_histogram(binwidth=50, colour="black", fill="lightblue")

