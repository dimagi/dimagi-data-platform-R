#Run lines 1-73

# Get db connection
db <- get_db_connection(system_conf)

# Get MALT table 
malt <- tbl(db, "malt_table")
malt <- collect(malt)
malt <- filter(malt, month.index == "May 2015")
malt$nforms <- as.numeric(malt$nforms)
#Change column names as needed
names(malt)[names(malt) == "month.index"] = "calendar_month"
#Convert calendar month to actual date
malt$calendar_month <- parse_date_time(paste('01', malt$calendar_month), '%d %b %Y!')
malt$calendar_month <- as.Date(malt$calendar_month)
#Concat user and month in monthly table
malt$user_month_concat <- paste(malt$user_pk, malt$calendar_month, sep = "_")
#Add unique integer column
malt$unique_int <- 1:nrow(malt)

# Filter form table
forms_single_month <- filter(form_table, form_start_date >= "2015-05-01" & form_start_date <= "2015-05-31")
active_days <- forms_single_month %>% group_by(domain_id, user_pk) %>% 
  summarise(active_days = length(unique(form_start_date)))

#Merge domain_numeric to malt
malt <- select(malt, -active_days)
domain_id <- select(domain, name, id)
names(domain_id)[names(domain_id) == "id"] = "domain_numeric"
malt <- merge(malt, domain_id, by.x = "domain", by.y = "name", all.x = T)

#Merge active_days to malt based on domain_numeric and user_pk
malt <- merge(malt, active_days, by.x = c("domain_numeric", "user_pk"), by.y = c("domain_id", "user_pk"), all.x = T)
malt <- select(malt, -domain_numeric)

#Keep only malt names in all_monthly
all_monthly <- all_monthly[,names(malt)]

#Remove all malt month data from all_monthly so as not to have duplicate rows
all_monthly <- all_monthly[all_monthly$calendar_month != "2015-05-01",]

#Rbind malt to all_monthly
all_monthly <- rbind(all_monthly, malt)

#Run lines 75 onwards













