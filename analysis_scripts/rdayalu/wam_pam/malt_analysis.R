#This code is to analyze the MALT (monthly aggregate lite table) which 
#we will use to generate quick WAM reports without waiting for the full
#monthly aggregate build
#MALT description here: 
#https://docs.google.com/document/d/1QQ3tzFPs6TWiPiah6YUBCrFILKih6OcJV7444i50o1U/edit

library(dplyr)

# Load system config file
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))

# Get db connection
db <- get_db_connection(system_conf)

# Get MALT table 
malt <- tbl(db, "malt_table")
malt <- collect(malt)

# ilsgateway-test3 has over 12K users in April 2015!
# Will email Yedi about this domain
# This domain is completely absent from the current monthly_table
# malt <- filter(malt, domain != "ilsgateway-test3")

# Active users per domain in April 2015 and May 2015
apr_malt <- filter(malt, month.index == "Apr 2015")
may_malt <- filter(malt, month.index == "May 2015")
apr_monthly <- filter(monthly_table, month.index == "Apr 2015")

# Domain summaries for MALT
apr_malt_summary <- apr_malt %>% group_by(domain) %>% 
  summarise(nusers_malt_apr_2015 = length(unique(user_pk)))
may_malt_summary <- may_malt %>% group_by(domain) %>% 
  summarise(calendar_month = unique(month.index), 
            nusers_malt_may_2015 = length(unique(user_pk)))

# Domain summaries for monthly_table
apr_monthly_summary <- apr_monthly %>% group_by(domain) %>% 
  summarise(nusers_monthly_apr_2015 = length(unique(user_pk)))

# Merge domain summaries by domain
domain_summaries <- merge(apr_monthly_summary, apr_malt_summary, by = "domain", all = T)
domain_summaries$nusers_apr_malt_monthly_diff <- domain_summaries$nusers_malt_apr_2015 - 
  domain_summaries$nusers_monthly_apr_2015
domain_summaries$domain_missing_apr_malt <- is.na(domain_summaries$nusers_malt_apr_2015)
domain_summaries$domain_added_apr_malt <- is.na(domain_summaries$nusers_monthly_apr_2015)

domain_summaries <- merge(domain_summaries, may_malt_summary, by = "domain", all = T)
domain_summaries$nusers_may_apr_malt_diff <- domain_summaries$nusers_malt_may_2015 - 
  domain_summaries$nusers_malt_apr_2015
domain_summaries$domain_missing_may_malt <- is.na(domain_summaries$nusers_malt_may_2015)
domain_summaries$domain_added_may_malt <- is.na(domain_summaries$nusers_monthly_apr_2015) | 
  is.na(domain_summaries$nusers_malt_apr_2015)

#Convert NA to zeros in dummy vectors to recalculate differences
domain_summaries$dum_nusers_malt_apr_2015 <- domain_summaries$nusers_malt_apr_2015
domain_summaries$dum_nusers_monthly_apr_2015 <- domain_summaries$nusers_monthly_apr_2015
domain_summaries$dum_nusers_malt_apr_2015[is.na(domain_summaries$dum_nusers_malt_apr_2015)] <- 0
domain_summaries$dum_nusers_monthly_apr_2015[is.na(domain_summaries$dum_nusers_monthly_apr_2015)] <- 0
domain_summaries$nusers_apr_malt_monthly_diff <- domain_summaries$dum_nusers_malt_apr_2015 - 
  domain_summaries$dum_nusers_monthly_apr_2015

domain_summaries$dum_nusers_malt_may_2015 <- domain_summaries$nusers_malt_may_2015
domain_summaries$dum_nusers_malt_may_2015[is.na(domain_summaries$dum_nusers_malt_may_2015)] <- 0
domain_summaries$nusers_may_apr_malt_diff <- domain_summaries$dum_nusers_malt_may_2015 - 
  domain_summaries$dum_nusers_malt_apr_2015

domain_summaries <- select(domain_summaries, -c(dum_nusers_malt_apr_2015, 
                                                dum_nusers_monthly_apr_2015, 
                                                dum_nusers_malt_may_2015))

#Pull the app table from the db
app <- tbl(db, "application")
app <- collect(app)
app_amplifies <- app
#Remove all double quotes from inside attributes string. Replace with underscores
app_amplifies$attributes <- gsub('"', "_", app_amplifies$attributes)
#Parse amplifies_workers values from app table
app_amplifies$amplifies_workers <- NA
app_amplifies$amplifies_workers[grep("_amplifies_workers_=>_yes_", app_amplifies$attributes, 
                                     fixed=T)] <- T
app_amplifies$amplifies_workers[grep("_amplifies_workers_=>_no_", app_amplifies$attributes, 
                                     fixed=T)] <- F
#Manually tag domains
app_amplifies$amplifies_workers[app_amplifies$id == 10724] <- F #pradan-mis "CDC"
app_amplifies$amplifies_workers[app_amplifies$id == 339] <- T #tulasalud "Kawok Vigilancia Comunitario 3.0"
app_amplifies$amplifies_workers[app_amplifies$id == 1140] <- T #tulasalud "Kawok Vigilancia Comunitario 2.0"
app_amplifies$amplifies_workers[app_amplifies$id == 1567] <- T #tulasalud "Kawok Vigilancia Comunitario 2.1"
app_amplifies$amplifies_workers[app_amplifies$id == 109] <- T #opm "Bihar Child Support Programme"
app_amplifies$amplifies_workers[app_amplifies$id == 290] <- T #ssqh-cs "mSante"
app_amplifies$amplifies_workers[app_amplifies$id == 2014] <- F #ssqh-cs "Referans Klinik"
#Get domain table
domains <- get_domain_table(db)
domains <- select(domains, name, id)
#Merge with app table
app_amplifies <- merge(app_amplifies, domains, by.x = "domain_id", by.y = "id", all.x = T)
names(app_amplifies)[names(app_amplifies) == "name"] = "domain"
app_summary <- app_amplifies %>% group_by(domain) %>% 
  summarise(napps_amplifies = sum(amplifies_workers, na.rm=T))
app_summary$has_amplifies_workers <- app_summary$napps_amplifies > 0
app_summary <- select(app_summary, domain, has_amplifies_workers)

#Merge app summary with domain_summaries
domain_summaries <- merge(domain_summaries, app_summary, by="domain", all.x = T)

write.csv(domain_summaries, file = "domain_summaries_malt.csv")

