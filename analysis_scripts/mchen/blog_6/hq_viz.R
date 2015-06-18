# IMPORT DATA #
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(zoo))
suppressMessages(library(lubridate))

data = tbl_df(read.csv("blog_data_6_10_15.csv", stringsAsFactors=FALSE))
data$calendar_month = as.Date(data$calendar_month, "%Y-%m-%d") # time data in DP is stored as numeric distance from 1970-01-01

hq = select(data, domain_numeric, user_pk, calendar_month) # keep columns of interest
hq = arrange(hq, domain_numeric, user_pk, calendar_month)
t = filter(hq, calendar_month >= as.Date("2012-01-01")) %>% 
  filter(., calendar_month <= as.Date("2014-12-01")) # keeping data from 2012 onward


# for each domain, get total number of users and number of active months 
agg = t %>%
  group_by(domain_numeric) %>% # change from domain_numeric to domain
  summarise(nusers=n_distinct(user_pk),
            nmonths = n_distinct(calendar_month))

# for each domain in each month, get the number of active users. TBF'ED
t_agg_1 = t %>%
  group_by(domain_numeric, calendar_month) %>%
  summarise(nusers = n_distinct(user_pk)) %>%
  arrange(., domain_numeric, calendar_month)

# for each domain, get total user blocks
t_agg_1$blocks = round_any(t_agg_1$nusers, 25, ceiling)/25

# rank domains by max user blocks in a given month (big domains go to the bottom)
d_rank = t_agg_1 %>% 
  group_by(domain_numeric) %>%
  summarise(b = max(blocks)) %>%
  arrange(., desc(b)) %>%
  mutate(b_rank = seq(b)) %>%
  select(., domain_numeric, b_rank)

# Replicate each row; set the number of replications to the number of blocks
expanded = as.numeric(rep(row.names(t_agg_1), t_agg_1$blocks))
texp = t_agg_1[expanded,] 
texp = texp[order(match(texp$domain_numeric, d_rank$domain_numeric)),] # order domains by max active month
texp = left_join(texp, d_rank)
texp = texp %>%
  group_by(b_rank, calendar_month) %>%
  mutate(incre_blocks = seq(n())) # Index user bins incrementally

# for each cum_blocks value, sum up cum_blocks and the max value of the domain that's sitting below
piles = texp %>%
  group_by(b_rank) %>%
  summarise(max_blocks = max(incre_blocks))
piles$max_blocks = c(0, cumsum(piles$max_blocks[-nrow(piles)])) 
texp = left_join(texp, piles) 
texp$piled_blocks = texp$incre_blocks + texp$max_blocks # building blocks into mounds for each domain


# subset HQ domains (just run codes above and you will get the data)
# needs to be functioned
# subs = c(11, 67, 87, 127, 138)
t = filter(t, domain_numeric %in% subs)
