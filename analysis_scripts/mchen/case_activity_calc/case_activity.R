# correlation between case activity rate and number of active days
library(plyr)
library(dplyr)

data = tbl_df(read.csv("blog.csv", stringsAsFactors = FALSE))
jharks = filter(data, domain == "jharkhand-mch")
jharks$calendar_month = as.Date(jharks$calendar_month)

# select time period
jks = filter(jharks, calendar_month >= as.Date("2014-09-01"))
jks = filter(jks, calendar_month < as.Date("2015-01-01"))
jks = filter(jks, !(user_id %in% "demo_user"))

# remove test user_id
users = tbl_df(read.csv("users.csv", stringsAsFactors=FALSE))
users = select(users, user_id, username)
jks_user_indice = grep("@jharkhand-mch.commcarehq.org", users$username)
jks_user = users[jks_user_indice,]
grep("test", jks_user$username) -> testPos # test users indices
jks_user = jks_user[-testPos,]

library(stringr)
jks_user$user = str_split_fixed(jks_user$username, "@", 2)[,1]
user_indice_2 = grep("s[1-9]", jks_user$user) 
jks_user = jks_user[user_indice_2,] # 222 distinct users 

# Import case activity data
ca = tbl_df(read.csv("ca_data.csv", stringsAsFactors = FALSE))
names(ca)[1] = c("user")
ca2 = left_join(ca, jks_user)
dimnames(ca2)[[1]] = ca2$user_id
ca3 = select(ca2, Sep, Oct, Nov, Dec) # user_id were added to each row as rownames 
ca3 = as.data.frame(ca3) # without this step there will be misleading error message about row.names() having unequal length

long_ca3 = reshape(ca3, varying = list(names(ca3)),
                   idvar = "user_id", ids = row.names(ca3),
                   times = names(ca3), timevar = "month",
                   direction = "long")

names(long_ca3) = c("month.index", "case_activity_rate", "user_id")
long_ca3$month.index = ifelse(long_ca3$month.index == "Sep", "2014-09-01", 
                              ifelse(long_ca3$month.index == "Oct", "2014-10-01", 
                                     ifelse(long_ca3$month.index == "Nov", "2014-11-01", "2014-12-01")))
long_ca3$month.index = as.Date(long_ca3$month.index)
long_ca3 = tbl_df(long_ca3)

# Join case activity data from HQ and active days data from DP
jks_2 = select(jks, user_id, active_days, calendar_month)
long_ca3 = rename(long_ca3, calendar_month = month.index)
jks_3 = left_join(jks_2, long_ca3, by = c("user_id", "calendar_month"))
jks_3[is.na(jks_3)] <- 0 

# Correlation 1: case_activity_rate vs. active days
library(Hmisc)
c1 = rcorr(jks_3$active_days, jks_3$case_activity_rate, type="pearson")

# Correlation 2: rank_car vs. rank_ad
# rank by case activity rate
jk_4 = jks_3 %>%
  group_by(user_id) %>%
  summarise(a=median(case_activity_rate), b=median(active_days))
jk_4 = arrange(jk_4, desc(a)) # order by case activity rate
jk_4$car_rank = seq_along(jk_4$a)
jk_4 = arrange(jk_4, desc(b)) # order by active days per month
jk_4$ad_rank = seq_along(jk_4$b)

c2 = rcorr(jk_4$ad_rank, jk_4$car_rank, type="pearson")
