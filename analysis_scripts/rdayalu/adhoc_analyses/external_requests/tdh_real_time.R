rec_forms <- filter(form_table, domain_id == 4515)
names(rec_forms)[names(rec_forms) == "user_id"] = "user_pk"
names(rec_forms)[names(rec_forms) == "id"] = "form_pk"
names(rec_forms)[names(rec_forms) == "domain_id"] = "domain_pk"
names(rec_forms)[names(rec_forms) == "application_id"] = "application_pk"
names(rec_forms)[names(rec_forms) == "visit_id"] = "visit_pk"

users <- tbl(db, "users") 
users <- collect(users)
user_type <- get_user_type_table(db)
user_type <- collect(user_type)
#Merge user tables
users <- merge(users, user_type, by.x = "id", by.y = "user_pk", all.x = T)
users <- select(users, -c(user_id.y, username.y, first_name, last_name, default_phone_number, 
                          groups, phone_numbers, deactivated, deleted, is_superuser))
names(users)[names(users) == "username.x"] = "username"
names(users)[names(users) == "id"] = "user_pk"
names(users)[names(users) == "user_id.x"] = "user_id"

rec_forms <- merge(rec_forms, users, by = "user_pk", all.x = T)

#Create form_date and form_month based on server time
rec_forms$form_start_date <- as.Date(substr(rec_forms$time_start, 1, 10))
rec_forms$form_start_month <- floor_date(rec_forms$form_start_date, "month")

formdef <- tbl(db, "formdef")
formdef <- collect(formdef)
formdef <- filter(formdef, domain_id == 4515)
formdef <- select(formdef, id, )