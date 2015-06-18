library(plyr)
library(dplyr)
library(zoo)

# load data
data = tbl_df(read.csv("blog.csv", stringsAsFactors=FALSE))
data$calendar_month = as.Date(data$calendar_month, "%m/%d/%y") 

# subset columns of interest
all = select(data, domain_numeric, user_pk, active_days, ncases_touched, calendar_month)
all = filter(all, calendar_month >= as.Date("2010-01-01")) %>% 
  filter(., calendar_month <= as.Date("2014-12-01")) # reformat date

# flagship project in India (pre-rebalancing)
crs = filter(all, domain_numeric == 40)

# total active months per user 
m = all %>%
  group_by(domain_numeric, user_pk) %>% # duplicated user_id exists across domains
  summarise(nmonths = n_distinct(calendar_month))

# excluding first N months of data (depends on reporting needs)
excdMonth = function(data, n) {
  data = data %>%
    group_by(domain_numeric, user_pk) %>%
    filter(., row_number() > n) 
  return(data)
}


# Rebalancing data: no domain contribute more than 10% to the data
# total active months per domain
totalMonths = function(x) {
  y = tbl_df(as.data.frame(table(x$domain_numeric)))
  y = arrange(y, desc(Freq))
  names(y) = c("domain_numeric", "months")
  y$pct = y$months/sum(y$months)
  return(y)
}
# min months to be dropped (or added) on each domain to get to 10% contribution 
drop = function(x, N) {
  x$bal = (x$months - sum(x$months)*N)/(1-N)
  return(x)
}

rebalanceData = function(data, N) {
  ov_month_data = totalMonths(data)
  K = drop(ov_month_data, N)
  dms = filter(K, bal > 0)
  keepVal = dms$months - ceiling(dms$bal)
  rebal_dm = as.numeric(as.character(dms$domain_numeric))
  sdata = filter(data, domain_numeric %in% rebal_dm) 
  ndata = filter(data, !(domain_numeric %in% rebal_dm)) 
  sp = split(sdata, sdata$domain_numeric)
  for (i in seq_len(length(sp))) {
    sp[[i]] = sp[[i]][sample(1:nrow(sp[[i]]), keepVal[i], FALSE),]
  }
  out = tbl_df(do.call(rbind, sp))
  rebalanced_data = tbl_df(rbind(out, ndata))  
  return(rebalanced_data)
}

# subset 1: all data rebalanced
rbl_all = rebalanceData(all, 0.10)
p1 = excdMonth(rbl_all, 3) 

# subset 2: all data excluding first 6 months 
p2 = excdMonth(all, 6) 
# reblanaced
rbl_p2 = rebalanceData(p2, 0.10) 

# subset 3: all data (unbalanced) including only users that have at least 18 months of usage
# rebalanced
u = select(filter(m, nmonths >= 18), user_pk)
qdata = filter(all, user_pk %in% u$user_pk) # exclude users active for < 18 months

# indexing each user-month
qdata = arrange(qdata, domain_numeric, user_pk, calendar_month)
qdata = qdata %>%
  group_by(domain_numeric, user_pk) %>%  
  mutate(user_month_index = seq(n()))  

# Bin user months to custom quarters
qdata$user_quarter_index = round_any(qdata$user_month_index, 3, ceiling)/3
# Rebalance data from quarterly data (only for Q1-Q6)
qdata_sub = filter(qdata, user_quarter_index <= 6)
qdata_split = split(qdata_sub, qdata_sub$user_quarter_index, drop=TRUE) 
rbl_qdata_split = lapply(qdata_split, function(x) rebalanceData(x, 0.10))
rbl_qdata = do.call(rbind, rbl_qdata_split)

# subset 4: 12 most active domains in terms of total user-months
bb = m %>%
  group_by(domain_numeric) %>%
  summarise(um = sum(nmonths)) %>%
  arrange(., desc(um)) %>%
  top_n(12)

bb_data = filter(rbl_all, domain_numeric %in% bb$domain_numeric)
