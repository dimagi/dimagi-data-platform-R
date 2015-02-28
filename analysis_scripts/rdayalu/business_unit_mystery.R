#BU mystery
#2/24/15

#How many country_final = India domains were active in Q4 of 2014?
#First load monthly table for all test = F and permitted_data = T domains
all_monthly <- monthly_table

#Consolidate country information
domain_facets <- domain_table
is.na(domain_facets$deployment.country) <- domain_facets$deployment.country == ""
is.na(domain_facets$country) <- domain_facets$country == ""
domain_facets$country_final <- domain_facets$deployment.country
keep_country <- which(is.na(domain_facets$deployment.country) & !is.na(domain_facets$country))
domain_facets$country_final[keep_country] <- domain_facets$country[keep_country]
is.na(domain_facets$internal.self_started) <- domain_facets$internal.self_started == ""

#Using just deployment.country
#domain_facets <- domain_table
#is.na(domain_facets$deployment.country) <- domain_facets$deployment.country == ""
#domain_facets$country_final <- domain_facets$deployment.country

#Keep only columns of interest from domain_table
names(domain_facets)[names(domain_facets) == "id"] = "domain_id"
facets_to_merge <- select(domain_facets, name, domain_id,is_test, country_final, 
                          internal.self_started)

#Merge domain facets from domain table into all_monthly table
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#Keep only India domains
all_monthly <- filter(all_monthly, country_final=="India")

#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
names(all_monthly)[names(all_monthly) == "domain_id"] = "domain_numeric"

#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

# Table of Q4 2014 and Jan 2015
q4 <- filter(all_monthly, calendar_month >= "2014-10-01" & calendar_month <= "2014-12-31")
test <- filter(all_monthly, calendar_month == "2014-12-01")
jan_2015 <- filter(all_monthly, calendar_month == "2015-01-01")

#Number of active domains per month/quarter
length(unique(q4$domain))
length(unique(test$domain))
length(unique(jan_2015$domain))
