# IMPORT DATA #
library(plyr)
library(dplyr)
data = read.csv("blog.csv", stringsAsFactors=FALSE)
data = tbl_df(data)

# data format conversion 
library(zoo)
data$calendar_month = as.Date(data$calendar_month)

# keep columns of interest
hq = filter(data, typical_flw == "TRUE") %>%  # keeping typical FLW only 
  select(., domain_numeric, domain, user_id, calendar_month)
hq = arrange(hq, domain_numeric, user_id, calendar_month)

agg = hq %>%
  group_by(domain_numeric) %>%
  summarise(nusers=n_distinct(user_id),
            nmonths = n_distinct(calendar_month))

# test subset for viz
t = filter(hq, calendar_month <= as.Date("2015-04-01"))
t = filter(t, calendar_month >= as.Date("2010-01-01"))

# X dimension: calendar month
getMonthlyUser = function(t) {
  t_agg_1 = t %>%
    group_by(domain, calendar_month) %>%
    summarise(nusers = n_distinct(user_id))
  #  x_range = seq(min(t_agg_1$calendar_month),max(t_agg_1$calendar_month),by="month")
  #  t_agg_1$domain = factor(t_agg_1$domain)
  #  t_expanded = expand.grid(unique(t_agg_1$domain),x_range) # this expansion is actually unnecessary
  #  names(t_expanded) = c("domain", "calendar_month")
  #  t_agg_1 = left_join(t_expanded, t_agg_1, by = c("domain","calendar_month"))
  #  t_agg_1[is.na(t_agg_1)] <- 0
  #  t_agg_1 = tbl_df(t_agg_1)
}


t_agg_1 = arrange(t_agg_1, domain, calendar_month)

# Y dimension: number of user blocks in each domain
getUserBlock = function(t_agg_1) {
  
  bins = c(5*(0:ceiling(max(t_agg_1$nusers)/5)))
  labs = c(1:ceiling(max(t_agg_1$nusers)/5))
  t_agg_1$blocks = cut(t_agg_1$nusers,
                       breaks = bins,
                       labels = labs)
  t_agg_1$blocks = as.numeric(t_agg_1$blocks)
  #  t_agg_1 = na.omit(t_agg_1) # let's not worry about adding gap months back for now
  #  t_agg_1$blocks[is.na(t_agg_1$blocks)] <- 0
}


# Replicate each row of the data and set the number of replications to the number of blocks
expanded = as.numeric(rep(row.names(t_agg_1), t_agg_1$blocks))
texp = t_agg_1[expanded,] 

texp = texp %>%
  group_by(domain, calendar_month) %>%
  mutate(cum_blocks = seq(n())) 

# for every single value of cum_blocks, return the sum of cum_blocks and the max value of the domain below
piles = texp %>%
  group_by(domain) %>%
  summarise(max_blocks = max(cum_blocks))
piles$max_blocks = c(0, cumsum(piles$max_blocks[-nrow(piles)]))

texp = arrange(texp, domain)
texp = left_join(texp, piles) 
texp$piled_blocks = texp$cum_blocks + texp$max_blocks

# using geom_tile to plot it 
library(ggplot2)
ggplot(texp, aes(x=calendar_month,y=piled_blocks,fill=domain)) + geom_tile()

