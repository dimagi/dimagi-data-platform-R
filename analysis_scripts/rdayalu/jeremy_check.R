#583 users
d_238 <- filter(blog, domain_numeric == 238)
length(unique(d_238$user_pk))

#Keep only users that have a month_index = 1 in this dataset
#Want to exclude users that started outside of this dataset
#Now have 573 users
d_238 <- d_238[d_238$user_pk %in% d_238$user_pk[d_238$month_index == 1],]

#Keep users with at least 12 months on CC
#Now have 128 users
#d_238 <- filter(d_238, months_on_cc >= 12)
#summary(d_238$months_on_cc)

#Keep only the first 12 months for each user (month_index = 1 through 12)
#So exclude month_index > 12
#Now have 128 users
#unique(d_238$user_pk[!(d_238$user_pk %in% with_mi_1)])
#test <- d_238[d_238$user_pk %in% unique(d_238$user_pk[!(d_238$user_pk %in% with_mi_1)]),]
#test <- arrange(test, user_pk, month_index)
d_238 <- filter(d_238, month_index <= 12)

#Keep only users that have a max(month_index) = 12 and have num_obs = 12
#97 users
users_for_12 <- d_238 %>% group_by(user_pk) %>% summarise(max_index = max(month_index),
                                                          num_obs = length(unique(calendar_month)))
users_for_12 <- filter(users_for_12, max_index == 12 & num_obs == 12)
d_238 <- d_238[d_238$user_pk %in% users_for_12$user_pk,]

#Keep only users that have been active for the 