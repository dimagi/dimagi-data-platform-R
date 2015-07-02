#Adds month_index for each active months - ignores skipped months
nwando <- arrange(nwando, user_pk, calendar_month)
nwando <- nwando %>% group_by(user_pk) %>% 
  mutate(month_index_no_skips = seq_along(calendar_month))

#Use cumsum as above to create cumulative active_days by the end of each month
