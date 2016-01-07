#Can duplicate user_pks across domains be user_type = mobile?
#This is question in based on an email sent by Amelia on 4/30/15
#where she says that "it's impossible to have the same mobile worker on two project spaces"

check_dup_user_type <- monthly_table

#Merge in user table
#Note that this user table has already been merged to the 
#user_type table, which we need
check_dup_user_type <- merge(check_dup_user_type, users, 
                             by = "user_pk", all.x = T)

summary_dup <- check_dup_user_type %>% group_by(domain, user_pk) %>% 
  summarise(user_id = unique(user_id), 
            user_type = unique(user_type))

summary_dup <- filter(summary_dup, !is.na(user_pk))
summary_dup <- filter(summary_dup, user_id != "demo_user")

summary_dup$dup_user_for <- duplicated(summary_dup$user_pk)
summary_dup$dup_user_back <- duplicated(summary_dup$user_pk, fromLast = T)
summary_dup$dup_user_pk <- summary_dup$dup_user_for == T | 
  summary_dup$dup_user_back == T

summary_dup <- filter(summary_dup, user_type == "mobile" & dup_user_pk == T)
