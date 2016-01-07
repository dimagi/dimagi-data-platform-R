# Google analytics - worker activity reports (WAR)
# This scirpt is to integrate csv downloads from GA. Based on user_id,
# WAR usage statistics will be linked to the the users table and eventually to 
# the monthly_table. Only non-Dimagi users will be considered. 
# 2/9/15

#Import GA csv
ga <- read.csv(file = "GA_aug_2014.csv")
domain_name <- sub(".*/a/", "", ga$page)
ga$domain <- sub("/reports/.*", "", domain_name)

#Import user table and user_type table
users <- tbl(db, "users") 
users <- collect(users)
user_type <- get_user_type_table(db)
user_type <- collect(user_type)

#Merge user tables
users <- merge(users, user_type, by.x = "id", by.y = "user_pk", all.x = T)
users <- select(users, -c(user_id.y, username.y))
names(users)[names(users) == "user_id.x"] = "user_id"
names(users)[names(users) == "username.x"] = "username"

#Exclude dimagi users and superusers from GA stats
dimagi <- grep("dimagi", users$email, fixed=T)
dimagi_users <- users[dimagi,]
non_dimagi_users <- users[!(users$id %in% dimagi_users$id),]
non_dimagi_users <- filter(non_dimagi_users, is_superuser == F | is.na(is_superuser))

#Merge ga values with user values
ga_users <- merge(ga, non_dimagi_users, by = "user_id", all.x = T)
names(ga_users)[names(ga_users) == "id"] = "user_pk"

#Exclude users with user_type = NA.
#These users were not in the "non_dimagi_users" list, so we don't want to include them.
ga_users <- filter(ga_users, !is.na(user_type))

#Merge with monthly table of interest
#all_monthly <- filter(monthly_table, month.index == "Aug 2014")
#ga_users <- merge(ga_users, all_monthly, by = "user_pk", all.x = T)

