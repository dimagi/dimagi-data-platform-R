test <- all_monthly

test$has_index_1 <- test$month_index == 1
user_index_1 <- test %.%
  group_by(user_pk) %.%
  summarise(keep_user = sum(has_index_1))
user_index_1 <- filter(user_index_1, keep_user != 0)
test <- test[test$user_pk %in% user_index_1$user_pk, ]

test <- filter(test, month_index <= 6)

#How many users have nmos = 6? 
active_6_mos <- test %>% group_by(user_pk) %>% 
  summarise(nmos = length(unique(calendar_month)))