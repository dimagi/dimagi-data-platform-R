#This code creates a table from the blog dataset 
#that aggregates # active users, # max users, 
#and #users with 3 mo attrition by domain-calendar_month
#combination

#Flag each new user in the blog dataset 
#We are flagging the first calendar month of each user
blog <- blog %>% group_by(domain_numeric, user_pk) %>% 
  mutate(new_user = calendar_month == min(calendar_month))

#Sort by domain and calendar_month before calculating the cumulative sum 
#of new users in the next step
blog <- arrange(blog, domain_numeric, calendar_month)

#Calculate cumulative sum of new users by domain
blog <- blog %>% group_by(domain_numeric) %>% 
  mutate(max_users = cumsum(new_user))

# Create aggregate table for #active users, #max users, 
#users with 3 mo attrition by domain/mont
nwando <- blog %>% group_by(domain_numeric, calendar_month) %>% 
  summarise(nusers_active = length(unique(user_pk)), 
            nusers_3mo_attrition = sum(next_three_months_active == F, na.rm=T), 
            total_users_to_date = max(max_users))