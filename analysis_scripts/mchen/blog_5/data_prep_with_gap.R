###################################
# ADD GAP MONTHS BACK TO THE GAME #
###################################

library(Hmisc) # monthDays() to get the exact number of days in a specific month

# for each user, generate a vector of months according to the distance variable
expandMonths = function(data) {
  months = seq(from = data$first_active_month, to = data$last_active_month,
               by = "months") # length.out is adding extra month for some reason
  noGap = data.frame(rep(data$user_pk,length(months)), months)
  return(noGap)
}

# for each user, get distance between first and last active month
gapBack = function(data){
  data = arrange(data, domain_numeric, user_pk, calendar_month)
  uf = tbl_df(ddply(data, .(domain_numeric, user_pk), function(x)head(x,n=1)))
  ul = tbl_df(ddply(data, .(domain_numeric, user_pk), function(x)tail(x,n=1)))   
  colnames(uf)[5] = c("first_active_month")
  colnames(ul)[5] = c("last_active_month")
  uf = select(uf, domain_numeric, user_pk, first_active_month)
  ul = select(ul, domain_numeric, user_pk, last_active_month)
  ufl = inner_join(uf, ul)
  ufl$distance = 12*as.numeric(as.yearmon(ufl$last_active_month) - 
                                 as.yearmon(ufl$first_active_month)) + 1
  expanded = tbl_df(ddply(ufl, .(domain_numeric, user_pk), function(x) expandMonths(x)))
  expanded = select(expanded, domain_numeric, user_pk, months)
  names(expanded) = c("domain_numeric", "user_pk", "calendar_month")  
  return(expanded)
}

# join the expanded data frame with all monthly data
all_expanded = gapBack(all)
all_expanded = left_join(expanded, all)
all_expanded[is.na(all_expanded)] <- 0

# for all data excluding 3 months, add back gap months
p1$percent_active_days = p1$active_days/monthDays(p1$calendar_month)
p1_expanded = gapBack(p1)
p1_expanded = left_join(p1_expanded, p1)
p1_expanded[is.na(p1_expanded)] <- 0

# for all data excluding 6 months, add back gap months
p2$percent_active_days = p2$active_days/monthDays(p2$calendar_month)
p2_expanded = gapBack(p2)
p2_expanded = left_join(p2_expanded, p2)
p2_expanded[is.na(p2_expanded)] <- 0

# for 12 big projects, add back gap months
bb_data$percent_active_days = bb_data$active_days/monthDays(bb_data$calendar_month)
bb_list = split(bb_data, bb_data$domain_numeric)
bb_data_expanded = gapBack(bb_data)
bb_data_expanded = left_join(bb_data_expanded, bb_data)
bb_data_expanded[is.na(bb_data_expanded)] <- 0

# for quarterly data, add back gap months
# adding back gap months means user_quarter_index needs to be recomputed
qdata$percent_active_days = qdata$active_days/monthDays(qdata$calendar_month)
qdata_expanded = gapBack(qdata)
qdata_expanded = left_join(qdata_expanded, qdata)
qdata_expanded[is.na(qdata_expanded)] <- 0

# recompute quarterly index after adding gap months
# reindex user_month_index
qdata_expanded = 
  qdata_expanded %>%
  group_by(domain_numeric, user_pk) %>%
  mutate(umi = seq_len(n()))
qdata_expanded$user_month_index <- NULL                    

# Bin user months to custom quarters
bins = c(1+(3*(0:ceiling(max(qdata_expanded$umi)/3))))
labs = paste("Month ", 3*seq(length(bins)-1)-2, "-", 3*seq(length(bins)-1), sep = "")

qdata_expanded = qdata_expanded %>% 
  group_by(domain_numeric, user_pk) %>%
  mutate(uqi = cut(umi, breaks = bins, labels = labs, right = FALSE))
