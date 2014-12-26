library(data.table)
library(lubridate)

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
add_next_previous_active <- function(all_monthly) {
  all_monthly$domain_numeric = as.numeric(as.factor(all_monthly$domain))
  all_monthly$calendar_month <- all_monthly$month_start
  all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)
  
  all_monthly <- arrange(all_monthly, domain_numeric, user_pk, calendar_month)
  df <- data.table(all_monthly)
  setkey(df,user_pk)
  df[,diff_days:=c(NA,diff(calendar_month)),by=user_pk]
  all_monthly <- as.data.frame(df)
  all_monthly$previous_three_months_active <- all_monthly$diff_days <= 93
  
  users <- unique(all_monthly$user_pk)

  next_three_months_active <- c()
  for (i in users) {
    single_user <- all_monthly[all_monthly$user_pk == i,]
    next_active <- c()
    next_active <- append(single_user$previous_three_months_active[-1], F)
    next_three_months_active <- append(next_three_months_active, next_active)
  }
  all_monthly$next_three_months_active <- next_three_months_active
  
  #If calendar_month = 10/1/14 then next_month_active = NA
  #because we don't know if the user will be active in the following month
  is.na(all_monthly$next_three_months_active) <- all_monthly$calendar_month >= "2014-08-01"
  return(all_monthly)
}
