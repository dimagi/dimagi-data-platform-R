#This code is to create the WAM table based on the DP MALT and HQ MALT that live in the 
#dropbox folder here: Dropbox (Dimagi)\Dimagi\CommCare\Data Platform\WAM-PAM tables\Working Files\Ingredients for July WAM
#We are only using the files in this folder - they are kept up to date by files downloaded from HQ
#so we are not dependent on the DP.

library(dplyr)
library(lubridate)

#------------------------------------------------------------------------#
#User configurable settings
#Import files from dropbox
#------------------------------------------------------------------------#

setwd("/Users/rdayalu/dimagi-data-platform-R")

#Define start_month for WAM tables
#WAM table 1 will start at this month
start_month <- as.Date("2013-01-01")

#Define current_month for WAM tables
#WAM table 2 will be generated for this month
current_month <- as.Date("2015-06-01")

#Import HQ malt files by name
#I need to come up with a way to import multiple HQ MALTs by name
#without listing them all here, but this will work for now.
hq_malt <- read.csv(file = "malt_hq_main_jun_2015.csv")
india_malt <- read.csv(file = "malt_hq_india_jan_jun_2015.csv")

#------------------------------------------------------------------------#
#Import static files from dropbox
#------------------------------------------------------------------------#

#These file names shouldn't change
domain <- read.csv(file = "project_spaces_hq.csv")
domain_india <- read.csv(file = "project_spaces_india.csv")
dp_malt <- read.csv(file = "malt_dp.csv")
country_bu_mapping <- read.csv(file = "country_bu_mapping.csv")

#Delete is_app_deleted, id row
#This was added to the HQ MALT and represents apps that were deleted 
#some time after forms were submitted using the app. We don't treat these rows differently
#while calculating WAMs. Per Neal, we don't need to worry about this right now.
hq_malt <- select(hq_malt, -c(id, is_app_deleted))
india_malt <- select(india_malt, -c(id, is_app_deleted))

#Rename columns for objects as needed
names(domain)[names(domain) == "Project"] = "domain"
names(domain)[names(domain) == "Deployment.Country"] = "countries"
names(domain)[names(domain) == "First.Form.Submission"] = "cp_first_form"
names(domain)[names(domain) == "Sector"] = "sector_final"
names(domain)[names(domain) == "Sub.Sector"] = "subsector_final"
names(domain)[names(domain) == "Self.Starter."] = "self_start"
names(domain)[names(domain) == "Business.Unit"] = "new_business_unit"
names(domain)[names(domain) == "Test.Project."] = "is_test"

names(domain_india)[names(domain_india) == "Project"] = "domain"
names(domain_india)[names(domain_india) == "Deployment.Country"] = "countries"
names(domain_india)[names(domain_india) == "First.Form.Submission"] = "cp_first_form"
names(domain_india)[names(domain_india) == "Sector"] = "sector_final"
names(domain_india)[names(domain_india) == "Sub.Sector"] = "subsector_final"
names(domain_india)[names(domain_india) == "Self.Starter."] = "self_start"
names(domain_india)[names(domain_india) == "Business.Unit"] = "new_business_unit"
names(domain_india)[names(domain_india) == "Test.Project."] = "is_test"

names(dp_malt)[names(dp_malt) == "month"] = "form_month"
names(dp_malt)[names(dp_malt) == "domain_name"] = "domain"
names(dp_malt)[names(dp_malt) == "num_of_forms"] = "nforms"
names(dp_malt)[names(dp_malt) == "wam"] = "amplifies_workers"
names(dp_malt)[names(dp_malt) == "pam"] = "amplifies_project"

names(hq_malt)[names(hq_malt) == "month"] = "form_month"
names(hq_malt)[names(hq_malt) == "domain_name"] = "domain"
names(hq_malt)[names(hq_malt) == "num_of_forms"] = "nforms"
names(hq_malt)[names(hq_malt) == "wam"] = "amplifies_workers"
names(hq_malt)[names(hq_malt) == "pam"] = "amplifies_project"

names(india_malt)[names(india_malt) == "month"] = "form_month"
names(india_malt)[names(india_malt) == "domain_name"] = "domain"
names(india_malt)[names(india_malt) == "num_of_forms"] = "nforms"
names(india_malt)[names(india_malt) == "wam"] = "amplifies_workers"
names(india_malt)[names(india_malt) == "pam"] = "amplifies_project"

#Format domain table as needed
domain <- select(domain, domain, countries, cp_first_form, sector_final, subsector_final, self_start, new_business_unit, is_test)
#Remove last three rows which contain only the summary for the project spaces
domain <- head(domain,-3)
#Domain names
domain$domain <- as.character(domain$domain)
#Countries
domain$countries <- as.character(domain$countries)
domain$countries[domain$countries == "No countries"] <- NA
domain$countries <- gsub("u'", "", domain$countries)
domain$countries <- gsub("'", "", domain$countries)
domain$countries <- gsub('u"', "", domain$countries)
domain$countries <- gsub('"', "", domain$countries)
domain$countries <- gsub('[[]', "", domain$countries)
domain$countries <- gsub('[]]', "", domain$countries)
#First form submission date
domain$cp_first_form <- as.character(domain$cp_first_form)
domain$cp_first_form[domain$cp_first_form == "No forms"] <- NA
domain$domain_start_date <- as.Date(substr(domain$cp_first_form, 1, 10), format = "%Y/%m/%d")
domain$sector_final <- as.character(domain$sector_final)
domain$subsector_final <- as.character(domain$subsector_final)
domain$self_start <- as.character(domain$self_start)
domain$new_business_unit <- as.character(domain$new_business_unit)
domain$is_test <- as.character(domain$is_test)

#Format domain_india table as needed
domain_india <- select(domain_india, domain, countries, cp_first_form, sector_final, subsector_final, self_start, new_business_unit, is_test)
#Remove last three rows which contain only the summary for the project spaces
domain_india <- head(domain_india,-3)
#Domain names
domain_india$domain <- as.character(domain_india$domain)
#Countries
domain_india$countries <- as.character(domain_india$countries)
domain_india$countries[domain_india$countries == "No countries"] <- NA
domain_india$countries <- gsub("u'", "", domain_india$countries)
domain_india$countries <- gsub("'", "", domain_india$countries)
domain_india$countries <- gsub('u"', "", domain_india$countries)
domain_india$countries <- gsub('"', "", domain_india$countries)
domain_india$countries <- gsub('[[]', "", domain_india$countries)
domain_india$countries <- gsub('[]]', "", domain_india$countries)
#First form submission date
domain_india$cp_first_form <- as.character(domain_india$cp_first_form)
domain_india$cp_first_form[domain_india$cp_first_form == "No forms"] <- NA
domain_india$domain_start_date <- as.Date(substr(domain_india$cp_first_form, 1, 10), format = "%Y/%m/%d")
domain_india$sector_final <- as.character(domain_india$sector_final)
domain_india$subsector_final <- as.character(domain_india$subsector_final)
domain_india$self_start <- as.character(domain_india$self_start)
domain_india$new_business_unit <- as.character(domain_india$new_business_unit)
domain_india$is_test <- as.character(domain_india$is_test)
#Most of the India projects have country = NA, but Stella confirmed that all these 
#are India project, so we will manually set that here till India HQ is updated.
domain_india$countries[is.na(domain_india$countries)] <- "INDIA"
#A few India server domains have identical names as the main server domains
#This will cause a problem in the WAM table 2 creation, so we will append those
#duplicate India server domains with "india_server" for now
domain_india$dup_domain_main_hq <- domain_india$domain %in% domain$domain == T
domain_india$domain[domain_india$dup_domain_main_hq == T] <- paste(domain_india$domain[domain_india$dup_domain_main_hq == T], 
                                                                   "india_server", sep = "_")
domain_india <- select(domain_india, -dup_domain_main_hq)

#Format dp_malt as needed
dp_malt$form_month <- as.character(dp_malt$form_month)
dp_malt$form_month <- as.Date(dp_malt$form_month)
dp_malt$user_id <- as.character(dp_malt$user_id) 
dp_malt$username <- as.character(dp_malt$username)
dp_malt$email <- as.character(dp_malt$email)
dp_malt$domain <- as.character(dp_malt$domain)
dp_malt$app_id <- as.character(dp_malt$app_id)
dp_malt$amplifies_workers <- as.character(dp_malt$amplifies_workers)
dp_malt$amplifies_project <- as.character(dp_malt$amplifies_project)
dp_malt$user_type <- as.character(dp_malt$user_type)

#Format hq_malt as needed
hq_malt$form_month <- as.character(hq_malt$form_month)
hq_malt$form_month <- as.Date(hq_malt$form_month, format="%m/%d/%Y")
hq_malt$user_id <- as.character(hq_malt$user_id) 
hq_malt$username <- as.character(hq_malt$username)
hq_malt$email <- as.character(hq_malt$email)
hq_malt$domain <- as.character(hq_malt$domain)
hq_malt$app_id <- as.character(hq_malt$app_id)
hq_malt$amplifies_workers <- as.character(hq_malt$amplifies_workers)
hq_malt$amplifies_project <- as.character(hq_malt$amplifies_project)
hq_malt$user_type <- as.character(hq_malt$user_type)

#Format india_malt as needed
india_malt$form_month <- as.character(india_malt$form_month)
india_malt$form_month <- as.Date(india_malt$form_month, format="%m/%d/%Y")
india_malt$user_id <- as.character(india_malt$user_id) 
india_malt$username <- as.character(india_malt$username)
india_malt$email <- as.character(india_malt$email)
india_malt$domain <- as.character(india_malt$domain)
india_malt$app_id <- as.character(india_malt$app_id)
india_malt$amplifies_workers <- as.character(india_malt$amplifies_workers)
india_malt$amplifies_project <- as.character(india_malt$amplifies_project)
india_malt$user_type <- as.character(india_malt$user_type)
#A few India MALT domains have identical names as the main MALT domains
#This will cause a problem in the WAM table 2 creation, so we will append those
#duplicate India server domains with "india_server" for now
india_malt$dup_domain_main_hq <- india_malt$domain %in% dp_malt$domain | 
    india_malt$domain %in% hq_malt$domain
india_malt$domain[india_malt$dup_domain_main_hq == T] <- paste(india_malt$domain[india_malt$dup_domain_main_hq == T], 
                                                                   "india_server", sep = "_")
india_malt <- select(india_malt, -dup_domain_main_hq)

#Format country_bu_mapping as needed
country_bu_mapping$country <- as.character(country_bu_mapping$country)
country_bu_mapping$business_unit <- as.character(country_bu_mapping$business_unit)

#Convert relevant variables to logical class
#amplifies_workers
dp_malt$amplifies_workers2 <- NA
dp_malt$amplifies_workers2[dp_malt$amplifies_workers == "no"] <- F
dp_malt$amplifies_workers2[dp_malt$amplifies_workers == "yes"] <- T
dp_malt <- select(dp_malt, -amplifies_workers)
names(dp_malt)[names(dp_malt) == "amplifies_workers2"] = "amplifies_workers"

hq_malt$amplifies_workers2 <- NA
hq_malt$amplifies_workers2[hq_malt$amplifies_workers == "no"] <- F
hq_malt$amplifies_workers2[hq_malt$amplifies_workers == "yes"] <- T
hq_malt <- select(hq_malt, -amplifies_workers)
names(hq_malt)[names(hq_malt) == "amplifies_workers2"] = "amplifies_workers"

india_malt$amplifies_workers2 <- NA
india_malt$amplifies_workers2[india_malt$amplifies_workers == "f"] <- F
india_malt$amplifies_workers2[india_malt$amplifies_workers == "t"] <- T
india_malt <- select(india_malt, -amplifies_workers)
names(india_malt)[names(india_malt) == "amplifies_workers2"] = "amplifies_workers"

#amplifies_project
dp_malt$amplifies_project2 <- NA
dp_malt$amplifies_project2[dp_malt$amplifies_project == "no"] <- F
dp_malt$amplifies_project2[dp_malt$amplifies_project == "yes"] <- T
dp_malt <- select(dp_malt, -amplifies_project)
names(dp_malt)[names(dp_malt) == "amplifies_project2"] = "amplifies_project"

hq_malt$amplifies_project2 <- NA
hq_malt$amplifies_project2[hq_malt$amplifies_project == "no"] <- F
hq_malt$amplifies_project2[hq_malt$amplifies_project == "yes"] <- T
hq_malt <- select(hq_malt, -amplifies_project)
names(hq_malt)[names(hq_malt) == "amplifies_project2"] = "amplifies_project"

india_malt$amplifies_project2 <- NA
india_malt$amplifies_project2[india_malt$amplifies_project == "f"] <- F
india_malt$amplifies_project2[india_malt$amplifies_project == "t"] <- T
india_malt <- select(india_malt, -amplifies_project)
names(india_malt)[names(india_malt) == "amplifies_project2"] = "amplifies_project"

#Check dp_malt and hq_malt tables before appending together
#summary(names(dp_malt) %in% names(hq_malt))

#Append together
all_malt <- rbind(dp_malt, hq_malt, india_malt)

#Add unique integer column
all_malt$unique_int <- 1:nrow(all_malt)

#------------------------------------------------------------------------#
#User exclusions
#------------------------------------------------------------------------#

#Exclude demo users and NA/NONE users
#First count these users per domain per month
user_exclusion <- all_malt %>% group_by(domain) %>% 
    summarise(users_initial = length(unique(user_id)),
              user_app_months_initial = length(unique(unique_int)),
              user_app_months_user_id_demo_na = length(unique(unique_int[user_id =="demo_user" | 
                                                                             user_id =="NONE" | 
                                                                             user_id == "none" | 
                                                                             is.na(user_id)])))


all_malt = all_malt[!(all_malt$user_id =="demo_user"),]
all_malt = all_malt[!(all_malt$user_id =="NONE"),]
all_malt = all_malt[!(all_malt$user_id =="none"),]
all_malt = all_malt[!is.na(all_malt$user_id),]

#Keep only users with user_type = CommCareUser
#First count these users per domain per month
all_malt$user_type_web <- all_malt$user_type == "WebUser"
all_malt <- all_malt %>% group_by(domain) %>% 
    mutate(user_app_months_web = sum(user_type_web, na.rm=T))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_app_months_web))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_type_web"
all_malt$keep_user <- all_malt$user_type_web == F | is.na(all_malt$user_type_web)
all_malt <- filter(all_malt, keep_user == T)

#Exclude users who submit to multiple domains
#First count these users per domain per month
chw_single_domain <- all_malt %>% group_by(user_id) %>% 
    summarise(n_domains = length(unique(domain)))
chw_single_domain <- filter(chw_single_domain, n_domains == 1)
all_malt$user_multiple_domain <- !(all_malt$user_id %in% chw_single_domain$user_id)
all_malt <- all_malt %>% group_by(domain) %>% 
    mutate(user_months_user_pk_multiple_domains = sum(user_multiple_domain))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_months_user_pk_multiple_domains))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_user_id_multiple_domains"
all_malt <- all_malt[all_malt$user_id %in% chw_single_domain$user_id,]

#Exclude dimagi users based on username and email
#Used to exclude dimagi users and superusers, but excluding dimagi users only 
#is fine.
#First count these users per domain per month
dimagi_users <- c(all_malt$user_id[grep("dimagi", all_malt$email, fixed=T)], 
                  all_malt$user_id[grep("dimagi", all_malt$username, fixed=T)])
all_malt$dimagi_user <- all_malt$user_id %in% dimagi_users
all_malt <- all_malt %>% group_by(domain) %>% 
    mutate(user_app_months_dimagi_user = sum(dimagi_user, na.rm=T))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_app_months_dimagi_user))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_dimagi_user"
all_malt <- filter(all_malt, dimagi_user == F)

#Add final columns to user_exclusion table
all_malt <- all_malt %>% group_by(domain) %>% 
    mutate(user_app_months_final = length(unique(unique_int)), 
           users_final = length(unique(user_id)))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_app_months_final), 
                                                    check2=unique(users_final))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_final"
names(user_exclusion)[names(user_exclusion) == "check2"] = "users_final"
#Convert all NA values to 0
user_exclusion[is.na(user_exclusion)] <- 0

write.csv(user_exclusion, file = "user_exclusions.csv", row.names = F)

#------------------------------------------------------------------------#
#Consolidate WAM categories for output tables so that each user has only
#one wam_eligible, wam_experienced and wam_using value. We don't want a 
#user to count in more than one of our 12 WAM categories
#------------------------------------------------------------------------#

all_malt$used_wam_elig_true <- all_malt$amplifies_workers == T
all_malt$used_wam_elig_true[is.na(all_malt$used_wam_elig_true)] <- F
all_malt$used_wam_elig_false <- all_malt$amplifies_workers == F
all_malt$used_wam_elig_false[is.na(all_malt$used_wam_elig_false)] <- F
all_malt$used_wam_elig_na <- is.na(all_malt$amplifies_workers)

#Create all_malt_user_month table with just one row per user per month
all_malt_user_month <- all_malt %>% group_by(user_id, form_month) %>% 
    summarise(domain = unique(domain),
              n_apps = length(unique(app_id)), 
              used_wam_elig_true = sum(used_wam_elig_true) >= 1,
              used_wam_elig_false = sum(used_wam_elig_false) >= 1,
              used_wam_elig_na = sum(used_wam_elig_na) >= 1, 
              nforms = sum(nforms)) 

#------------------------------------------------------------------------#
#wam_eligible
#------------------------------------------------------------------------#

all_malt_user_month$wam_eligible <- NA
all_malt_user_month$wam_eligible[all_malt_user_month$used_wam_elig_true == T] <- T
all_malt_user_month$wam_eligible[all_malt_user_month$used_wam_elig_true == F &
                                     all_malt_user_month$used_wam_elig_false == T] <- F

#------------------------------------------------------------------------#
#wam_experienced, pam_experienced
#------------------------------------------------------------------------#

#Add rolling # active months to each user-app row
all_malt_user_month <- arrange(all_malt_user_month, user_id, form_month)
all_malt_user_month <- all_malt_user_month %>% group_by(user_id) %>% 
    mutate(previous_active_months_rolling = seq_along(form_month)-1)

#wam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_malt_user_month$wam_experienced <- all_malt_user_month$previous_active_months_rolling >= 3


#------------------------------------------------------------------------#
#wam_using, pam_using
#------------------------------------------------------------------------#

#wam_using == T or pam_using == T
#if the user met our criteria this month for sufficient usage of the app. 
#Specifically, they submitted >= 15 forms during the user-app-month in question
all_malt_user_month$wam_using <- all_malt_user_month$nforms >= 15

#------------------------------------------------------------------------#
#WAM categories for output tables
#------------------------------------------------------------------------#

#We have 12 combinations for of wam_eligible(T/F/NA), wam_experienced(T/F) 
#and wam_using(T/F)
all_malt_user_month$elig_exp_using <- all_malt_user_month$wam_eligible == T & all_malt_user_month$wam_experienced == T & 
    all_malt_user_month$wam_using == T
all_malt_user_month$elig_exp_notusing <- all_malt_user_month$wam_eligible == T & all_malt_user_month$wam_experienced == T & 
    all_malt_user_month$wam_using == F
all_malt_user_month$elig_notexp_using <- all_malt_user_month$wam_eligible == T & all_malt_user_month$wam_experienced == F & 
    all_malt_user_month$wam_using == T
all_malt_user_month$elig_notexp_notusing <- all_malt_user_month$wam_eligible == T & all_malt_user_month$wam_experienced == F & 
    all_malt_user_month$wam_using == F

all_malt_user_month$notelig_exp_using <- all_malt_user_month$wam_eligible == F & all_malt_user_month$wam_experienced == T & 
    all_malt_user_month$wam_using == T
all_malt_user_month$notelig_exp_notusing <- all_malt_user_month$wam_eligible == F & all_malt_user_month$wam_experienced == T & 
    all_malt_user_month$wam_using == F
all_malt_user_month$notelig_notexp_using <- all_malt_user_month$wam_eligible == F & all_malt_user_month$wam_experienced == F & 
    all_malt_user_month$wam_using == T
all_malt_user_month$notelig_notexp_notusing <- all_malt_user_month$wam_eligible == F & all_malt_user_month$wam_experienced == F & 
    all_malt_user_month$wam_using == F

all_malt_user_month$na_exp_using <- is.na(all_malt_user_month$wam_eligible)& all_malt_user_month$wam_experienced == T & 
    all_malt_user_month$wam_using == T
all_malt_user_month$na_exp_notusing <- is.na(all_malt_user_month$wam_eligible) & all_malt_user_month$wam_experienced == T & 
    all_malt_user_month$wam_using == F
all_malt_user_month$na_notexp_using <- is.na(all_malt_user_month$wam_eligible)& all_malt_user_month$wam_experienced == F & 
    all_malt_user_month$wam_using == T
all_malt_user_month$na_notexp_notusing <- is.na(all_malt_user_month$wam_eligible) & all_malt_user_month$wam_experienced == F & 
    all_malt_user_month$wam_using == F

#We have 4 combinations for wam_experienced(T/F) and wam_using(T/F)
all_malt_user_month$using_exp <- all_malt_user_month$wam_experienced == T & all_malt_user_month$wam_using == T
all_malt_user_month$notusing_exp <- all_malt_user_month$wam_experienced == T & all_malt_user_month$wam_using == F
all_malt_user_month$using_notexp <- all_malt_user_month$wam_experienced == F & all_malt_user_month$wam_using == T
all_malt_user_month$notusing_notexp <- all_malt_user_month$wam_experienced == F & all_malt_user_month$wam_using == F

#------------------------------------------------------------------------#
#Add amplifies_workers/project to domain table based on values from all_malt
#------------------------------------------------------------------------#

domain <- rbind(domain, domain_india)

#Create vectors of domain_ids by value of amplifies_workers and amplifies_project
#These vectors are not mutually exclusive, meaning that some domains can be in more 
#than one vector
domain_has_amp_w_true <- unique(filter(all_malt, amplifies_workers == T)$domain)
domain_has_amp_w_false <- unique(filter(all_malt, amplifies_workers == F)$domain)
domain_has_amp_w_na <- unique(filter(all_malt, is.na(amplifies_workers))$domain) 

domain_has_amp_p_true <- unique(filter(all_malt, amplifies_project == T)$domain)
domain_has_amp_p_false <- unique(filter(all_malt, amplifies_project == F)$domain)
domain_has_amp_p_na <- unique(filter(all_malt, is.na(amplifies_project))$domain)

#Create mutually exclusive vectors, meaning that no domains overlap between vectors 
#Keep domain_has_amp_w_true as is
#Remove domain_has_amp_w_true from the other two vectors
domain_has_amp_w_false <- domain_has_amp_w_false[!(domain_has_amp_w_false %in% domain_has_amp_w_true)]
domain_has_amp_w_na <- domain_has_amp_w_na[!(domain_has_amp_w_na %in% domain_has_amp_w_true)]
#Remove domain_has_amp_w_false from domain_has_amp_w_na
domain_has_amp_w_na <- domain_has_amp_w_na[!(domain_has_amp_w_na %in% domain_has_amp_w_false)]

#Keep domain_has_amp_p_true as is
#Remove domain_has_amp_p_true from the other two vectors
domain_has_amp_p_false <- domain_has_amp_p_false[!(domain_has_amp_p_false %in% domain_has_amp_p_true)]
domain_has_amp_p_na <- domain_has_amp_p_na[!(domain_has_amp_p_na %in% domain_has_amp_p_true)]
#Remove domain_has_amp_p_false from domain_has_amp_p_na
domain_has_amp_p_na <- domain_has_amp_p_na[!(domain_has_amp_p_na %in% domain_has_amp_p_false)]

domain$domain_has_amplifies_workers <- NA
domain$domain_has_amplifies_workers[domain$domain %in% domain_has_amp_w_true] <- T
domain$domain_has_amplifies_workers[domain$domain %in% domain_has_amp_w_false] <- F

domain$domain_has_amplifies_project <- NA
domain$domain_has_amplifies_project[domain$domain %in% domain_has_amp_p_true] <- T
domain$domain_has_amplifies_project[domain$domain %in% domain_has_amp_p_false] <- F

#------------------------------------------------------------------------#
#Map domain table with missing BU values to master BU file
#------------------------------------------------------------------------#

#Convert countries variable to a corresponsing list, separating out the comma
#separated names
country_list <- as.list(strsplit(domain$countries, ", "))

#Extract first country from country_list
#Ideally, we want to pick the first country that has a BU match 
#for domains with multiple countries, but we will figure that out later
#Just pick the first country in the list for now
domain$country_bu <- unlist(lapply(country_list, function(x) x[1]))

#For domains with missing BU, look for BU in the master file
domain$new_business_unit[domain$new_business_unit == "No info"] <- NA
domain <- merge(domain, country_bu_mapping, by.x = "country_bu", by.y = "country", all.x = T)
domain$new_business_unit[is.na(domain$new_business_unit)] <- domain$business_unit[is.na(domain$new_business_unit)]  

#Merge domain facets to all_malt_user_month
facets_to_merge <- select(domain, domain, countries, sector_final, 
                          subsector_final, self_start, domain_has_amplifies_workers, 
                          domain_has_amplifies_project, is_test, domain_start_date, new_business_unit)
all_malt_user_month <- merge(all_malt_user_month, facets_to_merge, by = "domain", all.x = T)
all_malt_user_month$new_business_unit[is.na(all_malt_user_month$new_business_unit)] <- "None"

#------------------------------------------------------------------------#
#Table 1: WAM OVERVIEW
#------------------------------------------------------------------------#

#Create denominators for Table 1

#d2: all users who submitted forms at least 4+ months ago, regardless of whether or not they 
#are experienced as of the month in question (i.e., these users could theoretically be 
#experienced by the month in question. For example, a user who submitted just one form 
#6 months ago. This is just whether the user’s first form was more than 3 months ago)
all_malt_user_month <- all_malt_user_month %>% group_by(domain, user_id) %>% 
    mutate(new_user = form_month == min(form_month))
agg_d2 <- all_malt_user_month %>% group_by(form_month) %>% 
    summarise(nusers_new = sum(new_user))
agg_d2$month_prev_3 <- agg_d2$form_month - months(3)
d2_months <- agg_d2$month_prev_3[agg_d2$month_prev_3 %in% agg_d2$form_month]
vector_d2 <- c()
for(i in d2_months) {
    subset_d2 <- agg_d2[agg_d2$form_month <= i,]
    sum_d2 <- sum(subset_d2$nusers_new)
    vector_d2 <- c(vector_d2, sum_d2)
}
vector_d2 <- c(rep(0, nrow(agg_d2) - length(vector_d2)), vector_d2)

#d3: all experienced users (all users who are actually experienced by the time they reach 
#the month in question, regardless of whether or not they are active during the month 
#in question)
#We are flagging the calendar month in which each user actually becomes experienced
all_malt_user_month$new_exp_user <- all_malt_user_month$previous_active_months_rolling == 3
#Sort by calendar_month before calculating the cumulative sum 
#of new users in the next step
all_malt_user_month <- ungroup(all_malt_user_month)
all_malt_user_month <- arrange(all_malt_user_month, form_month)
#Calculate cumulative sum of new experienced users
all_malt_user_month$max_exp_users <- cumsum(all_malt_user_month$new_exp_user)
# Create row for total experienced users to date 
all_malt_user_month <- all_malt_user_month %>% group_by(form_month) %>% 
    mutate(total_exp_users_to_date = max(max_exp_users))

#Create Table 1
table_1_wam <- all_malt_user_month %>% group_by(form_month) %>% 
    summarise(nusers = length(unique(user_id)),
              eligible = sum(wam_eligible, na.rm = T), 
              using = sum(wam_using, na.rm = T), 
              experienced = sum(wam_experienced, na.rm=T), 
              d3_exp_real = unique(total_exp_users_to_date),
              
              elig_using_exp = sum(elig_exp_using, na.rm=T),
              elig_notusing_exp = sum(elig_exp_notusing, na.rm=T),
              elig_using_notexp = sum(elig_notexp_using, na.rm=T), 
              elig_notusing_notexp = sum(elig_notexp_notusing, na.rm=T),
              
              notelig_using_exp = sum(notelig_exp_using, na.rm=T),
              notelig_notusing_exp = sum(notelig_exp_notusing, na.rm=T),
              notelig_using_notexp = sum(notelig_notexp_using, na.rm=T), 
              notelig_notusing_notexp = sum(notelig_notexp_notusing, na.rm=T), 
              
              naelig_using_exp = sum(na_exp_using, na.rm=T),
              naelig_notusing_exp = sum(na_exp_notusing, na.rm=T), 
              naelig_using_notexp = sum(na_notexp_using, na.rm=T),
              naelig_notusing_notexp = sum(na_notexp_notusing, na.rm=T))

table_1_wam$per_wams_d4 <- round((table_1_wam$elig_using_exp/table_1_wam$experienced)*100, digits = 1)
table_1_wam$d2_exp_theory <- vector_d2

#Filter table_1
table_1_wam <- filter(table_1_wam, form_month >= start_month, form_month <= current_month)
names(table_1_wam)[names(table_1_wam) == "form_month"] = "Calendar Month"
names(table_1_wam)[names(table_1_wam) == "nusers"] = "Active Users"
names(table_1_wam)[names(table_1_wam) == "eligible"] = "Eligible Users"
names(table_1_wam)[names(table_1_wam) == "using"] = "Sufficient Users"
names(table_1_wam)[names(table_1_wam) == "experienced"] = "Experienced Users"
names(table_1_wam)[names(table_1_wam) == "d2_exp_theory"] = "D2 All Possibly Exp Users"
names(table_1_wam)[names(table_1_wam) == "d3_exp_real"] = "D3 All Ever Exp Users"
write.csv(table_1_wam, file = "table_1_wam.csv", row.names=F)

#Create table 1 for each business unit
business_unit <- unique(all_malt_user_month$new_business_unit)

for (j in business_unit) {
    single_bu <- all_malt_user_month[all_malt_user_month$new_business_unit == j,]
    
    agg_d2 <- single_bu %>% group_by(form_month) %>% 
        summarise(nusers_new = sum(new_user))
    agg_d2$month_prev_3 <- agg_d2$form_month - months(3)
    d2_months <- agg_d2$month_prev_3[agg_d2$month_prev_3 %in% agg_d2$form_month]
    vector_d2 <- c()
    for(i in d2_months) {
        subset_d2 <- agg_d2[agg_d2$form_month <= i,]
        sum_d2 <- sum(subset_d2$nusers_new)
        vector_d2 <- c(vector_d2, sum_d2)
    }
    vector_d2 <- c(rep(0, nrow(agg_d2) - length(vector_d2)), vector_d2)
    
    #d3: all experienced users (all users who are actually experienced by the time they reach 
    #the month in question, regardless of whether or not they are active during the month 
    #in question)
    #We are flagging the calendar month in which each user actually becomes experienced
    single_bu$new_exp_user <- single_bu$previous_active_months_rolling == 3
    #Sort by calendar_month before calculating the cumulative sum 
    #of new users in the next step
    single_bu <- ungroup(single_bu)
    single_bu <- arrange(single_bu, form_month)
    #Calculate cumulative sum of new experienced users
    single_bu$max_exp_users <- cumsum(single_bu$new_exp_user)
    # Create row for total experienced users to date 
    single_bu <- single_bu %>% group_by(form_month) %>% 
        mutate(total_exp_users_to_date = max(max_exp_users))
    
    #Create Table 1
    table_1_wam_bu <- single_bu %>% group_by(form_month) %>% 
        summarise(nusers = length(unique(user_id)),
                  eligible = sum(wam_eligible, na.rm = T), 
                  using = sum(wam_using, na.rm = T), 
                  experienced = sum(wam_experienced, na.rm=T), 
                  d3_exp_real = unique(total_exp_users_to_date),
                  
                  elig_using_exp = sum(elig_exp_using, na.rm=T),
                  elig_notusing_exp = sum(elig_exp_notusing, na.rm=T),
                  elig_using_notexp = sum(elig_notexp_using, na.rm=T), 
                  elig_notusing_notexp = sum(elig_notexp_notusing, na.rm=T),
                  
                  notelig_using_exp = sum(notelig_exp_using, na.rm=T),
                  notelig_notusing_exp = sum(notelig_exp_notusing, na.rm=T),
                  notelig_using_notexp = sum(notelig_notexp_using, na.rm=T), 
                  notelig_notusing_notexp = sum(notelig_notexp_notusing, na.rm=T), 
                  
                  naelig_using_exp = sum(na_exp_using, na.rm=T),
                  naelig_notusing_exp = sum(na_exp_notusing, na.rm=T), 
                  naelig_using_notexp = sum(na_notexp_using, na.rm=T),
                  naelig_notusing_notexp = sum(na_notexp_notusing, na.rm=T))
    
    table_1_wam_bu$per_wams_d4 <- round((table_1_wam_bu$elig_using_exp/table_1_wam_bu$experienced)*100, digits = 1)
    table_1_wam_bu$d2_exp_theory <- vector_d2
    
    #Filter table_1
    table_1_wam_bu <- filter(table_1_wam_bu, form_month >= start_month, form_month <= current_month)
    names(table_1_wam_bu)[names(table_1_wam_bu) == "form_month"] = "Calendar Month"
    names(table_1_wam_bu)[names(table_1_wam_bu) == "nusers"] = "Active Users"
    names(table_1_wam_bu)[names(table_1_wam_bu) == "eligible"] = "Eligible Users"
    names(table_1_wam_bu)[names(table_1_wam_bu) == "using"] = "Sufficient Users"
    names(table_1_wam_bu)[names(table_1_wam_bu) == "experienced"] = "Experienced Users"
    names(table_1_wam_bu)[names(table_1_wam_bu) == "d2_exp_theory"] = "D2 All Possibly Exp Users"
    names(table_1_wam_bu)[names(table_1_wam_bu) == "d3_exp_real"] = "D3 All Ever Exp Users"
    
    #Rename table specific to BU
    assign(paste("table_1_wam", j, sep="_"), table_1_wam_bu)
}

write.csv(table_1_wam_None, file = "table_1_wam_None.csv", row.names=F)
write.csv(table_1_wam_INC, file = "table_1_wam_INC.csv", row.names=F)
write.csv(table_1_wam_DSI, file = "table_1_wam_DSI.csv", row.names=F)
write.csv(table_1_wam_DWA, file = "table_1_wam_DWA.csv", row.names=F)
write.csv(table_1_wam_DSA, file = "table_1_wam_DSA.csv", row.names=F)
write.csv(table_1_wam_DLAC, file = "table_1_wam_DLAC.csv", row.names=F)
write.csv(table_1_wam_DMOZ, file = "table_1_wam_DMOZ.csv", row.names=F)

#------------------------------------------------------------------------#
#Table 2: WAM DATA (complete)
#------------------------------------------------------------------------#

#Set table parameters
all_malt_user_month$current_month <- current_month

all_malt_table2 <- filter(all_malt_user_month, form_month >= current_month, 
                          form_month <= current_month)

#Create Table 2
table_2_wam <- all_malt_table2 %>% group_by(domain) %>% 
    summarise(calendar_month = unique(current_month),
              nusers = length(unique(user_id)),
              elig_using_exp = sum(elig_exp_using, na.rm=T),
              elig_notusing_exp = sum(elig_exp_notusing, na.rm=T),
              elig_using_notexp = sum(elig_notexp_using, na.rm=T), 
              elig_notusing_notexp = sum(elig_notexp_notusing, na.rm=T),
              
              notelig_using_exp = sum(notelig_exp_using, na.rm=T),
              notelig_notusing_exp = sum(notelig_exp_notusing, na.rm=T),
              notelig_using_notexp = sum(notelig_notexp_using, na.rm=T), 
              notelig_notusing_notexp = sum(notelig_notexp_notusing, na.rm=T), 
              
              naelig_using_exp = sum(na_exp_using, na.rm=T),
              naelig_notusing_exp = sum(na_exp_notusing, na.rm=T), 
              naelig_using_notexp = sum(na_notexp_using, na.rm=T),
              naelig_notusing_notexp = sum(na_notexp_notusing, na.rm=T))

write.csv(table_2_wam, file = "table_2_wam.csv", row.names=F)

#------------------------------------------------------------------------#
#Table 2_1: WAM DATA (modified from Table 2)
#------------------------------------------------------------------------#

#Set table parameters for current_month
all_malt_user_month$current_month <- current_month
all_malt_user_month$prior_1_month <- floor_date(rollback(all_malt_user_month$current_month), "month")
all_malt_user_month$prior_2_months <- floor_date(rollback(all_malt_user_month$prior_1_month), "month")

#Create denominators for Table 2_1_4

#d1: all users who ever submitted forms as of the month in question
#We are flagging the first calendar month of each user
all_malt_user_month <- all_malt_user_month %>% group_by(domain, user_id) %>% 
    mutate(new_user = form_month == min(form_month))
#Sort by domain and calendar_month before calculating the cumulative sum 
#of new users in the next step
all_malt_user_month <- ungroup(all_malt_user_month)
all_malt_user_month <- arrange(all_malt_user_month, domain, form_month)
#Calculate cumulative sum of new users by domain
all_malt_user_month <- all_malt_user_month %>% group_by(domain) %>% 
    mutate(max_users = cumsum(new_user))
# Create variable for total users to date 
all_malt_user_month <- all_malt_user_month %>% group_by(domain, form_month) %>% 
    mutate(total_users_to_date = max(max_users))

#d2: all users who submitted forms at least 4+ months ago, regardless of whether or not they 
#are experienced as of the month in question (i.e., these users could theoretically be 
#experienced by the month in question. For example, a user who submitted just one form 
#6 months ago. This is just whether the user’s first form was more than 3 months ago)
agg_d2 <- all_malt_user_month %>% group_by(domain, form_month) %>% 
    summarise(nusers_new = sum(new_user))
agg_d2$month_prev_3 <- agg_d2$form_month - months(3)
agg_d2 <- filter(agg_d2, form_month <= current_month - months(3))
agg_d2 <- agg_d2 %>% group_by(domain) %>% mutate(d2_exp_theory = sum(nusers_new))
agg_d2 <- agg_d2 %>% group_by(domain) %>% summarise(d2_exp_theory = unique(d2_exp_theory))
all_malt_user_month <- merge(all_malt_user_month, agg_d2, by = "domain", all.x = T)

#d3: all experienced users (all users who are actually experienced by the time they reach 
#the month in question, regardless of whether or not they are active during the month 
#in question)
#We are flagging the calendar month in which each user actually becomes experienced
all_malt_user_month$new_exp_user <- all_malt_user_month$previous_active_months_rolling == 3
#Sort by domain and calendar_month before calculating the cumulative sum 
#of new users in the next step
all_malt_user_month <- ungroup(all_malt_user_month)
all_malt_user_month <- arrange(all_malt_user_month, domain, form_month)
#Calculate cumulative sum of new experienced users by domain
all_malt_user_month <- all_malt_user_month %>% group_by(domain) %>% 
    mutate(max_exp_users = cumsum(new_exp_user))
# Create row for total experienced users to date 
all_malt_user_month <- all_malt_user_month %>% group_by(domain, form_month) %>% 
    mutate(total_exp_users_to_date = max(max_exp_users))

#Create subset for time range parameters
all_monthly_table2 <- filter(all_malt_user_month, form_month >= prior_2_months, 
                             form_month <= current_month) 

#d6: total # users active over the past 3 months (current month and past 2 months) 
all_monthly_table2 <- all_monthly_table2 %>% group_by(domain) %>% 
    mutate(total_users_past_3_mos = length(unique(user_id)))

#Create Table 2_1_1
table_2_1_1 <- all_monthly_table2 %>% group_by(domain) %>% 
    summarise(Country = unique(countries),
              Sector = unique(sector_final),
              Subsector = unique(subsector_final),
              business_unit = unique(new_business_unit), 
              self_service = unique(self_start), 
              test = unique(is_test), 
              domain_start_date = unique(domain_start_date, na.rm=T), 
              nusers = length(unique(user_id[form_month == current_month])), 
              domain_has_amplifies_workers = unique(domain_has_amplifies_workers),
              domain_has_amplifies_project = unique(domain_has_amplifies_project))

names(table_2_1_1)[names(table_2_1_1) == "domain"] = "Domain"
names(table_2_1_1)[names(table_2_1_1) == "business_unit"] = "Business Unit"
names(table_2_1_1)[names(table_2_1_1) == "self_service"] = "Self Service"
names(table_2_1_1)[names(table_2_1_1) == "test"] = "Test Domain"
names(table_2_1_1)[names(table_2_1_1) == "domain_start_date"] = "Domain Start Date"
names(table_2_1_1)[names(table_2_1_1) == "nusers"] = "Active Users"
names(table_2_1_1)[names(table_2_1_1) == "domain_has_amplifies_workers"] = "Eligible for WAMs"
names(table_2_1_1)[names(table_2_1_1) == "domain_has_amplifies_project"] = "Eligible for PAMs"

#write.csv(table_2_1_1, file = "table_2_1_1.csv", row.names = F)

table_2_1_2 <- all_monthly_table2 %>% group_by(domain) %>% 
    summarise(prior_2_months = unique(prior_2_months), 
              prior_1_month = unique(prior_1_month),
              current_month = unique(current_month), 
              wams_2_mos_prior = sum(elig_exp_using[form_month == prior_2_months], na.rm=T), 
              wams_1_mo_prior = sum(elig_exp_using[form_month == prior_1_month], na.rm=T), 
              wams_current_mo = sum(elig_exp_using[form_month == current_month], na.rm=T))
names(table_2_1_2)[names(table_2_1_2) == "domain"] = "Domain"
names(table_2_1_2)[names(table_2_1_2) == "prior_2_months"] = "Prior 2 Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "prior_1_month"] = "Prior 1 Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "current_month"] = "Current Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "wams_2_mos_prior"] = "WAMs 2 months prior"
names(table_2_1_2)[names(table_2_1_2) == "wams_1_mo_prior"] = "WAMs 1 month prior"
names(table_2_1_2)[names(table_2_1_2) == "wams_current_mo"] = "WAMs current month"

#write.csv(table_2_1_2, file = "table_2_1_2.csv", row.names = F)

table_2_1_3 <- all_monthly_table2 %>% group_by(domain) %>%
    summarise(using_exp = sum(using_exp[form_month == current_month], na.rm=T),
              inelig_exp_using = sum(notelig_exp_using[form_month == current_month] == T | 
                                         na_exp_using[form_month == current_month] == T, na.rm = T),
              notusing_exp = sum(notusing_exp[form_month == current_month], na.rm=T),
              using_notexp = sum(using_notexp[form_month == current_month], na.rm=T), 
              notusing_notexp = sum(notusing_notexp[form_month == current_month], na.rm=T))
names(table_2_1_3)[names(table_2_1_3) == "domain"] = "Domain"
names(table_2_1_3)[names(table_2_1_3) == "using_exp"] = "Sufficient Use and Experience"
names(table_2_1_3)[names(table_2_1_3) == "inelig_exp_using"] = "Ineligible - Sufficient Use and Experience"
names(table_2_1_3)[names(table_2_1_3) == "notusing_exp"] = "Low Use"
names(table_2_1_3)[names(table_2_1_3) == "using_notexp"] = "Not Experienced"
names(table_2_1_3)[names(table_2_1_3) == "notusing_notexp"] = "Low Use + Not Experienced"

#write.csv(table_2_1_3, file = "table_2_1_3.csv", row.names = F)

table_2_1_4 <- filter(all_monthly_table2, form_month == current_month) %>% 
    group_by(domain) %>% 
    summarise(d1_total_users_to_date = unique(total_users_to_date), 
              d2_exp_theory = unique(d2_exp_theory), 
              d3_exp_real = unique(total_exp_users_to_date), 
              d4_exp_active_current = sum(wam_experienced, na.rm=T),
              d5_active_current = length(unique(user_id)),
              d6_active_3_mos = unique(total_users_past_3_mos), 
              wams_current_mo = sum(elig_exp_using, na.rm=T))
table_2_1_4$per_wams_current <- round((table_2_1_4$wams_current_mo/table_2_1_4$d4_exp_active_current)*100, digits = 1)
table_2_1_4$per_wams_current[table_2_1_4$d4_exp_active_current == 0] <- 0

names(table_2_1_4)[names(table_2_1_4) == "domain"] = "Domain"
names(table_2_1_4)[names(table_2_1_4) == "d1_total_users_to_date"] = "D1 All Users Ever Active"
names(table_2_1_4)[names(table_2_1_4) == "d2_exp_theory"] = "D2 All Possibly Exp Users"
names(table_2_1_4)[names(table_2_1_4) == "d3_exp_real"] = "D3 All Ever Exp Users"
names(table_2_1_4)[names(table_2_1_4) == "d4_exp_active_current"] = "D4 All Experienced + Active Users"
names(table_2_1_4)[names(table_2_1_4) == "d5_active_current"] = "D5 All Active Users"
names(table_2_1_4)[names(table_2_1_4) == "d6_active_3_mos"] = "D6 All Active Users Current + Prior 2 Mos"
names(table_2_1_4)[names(table_2_1_4) == "wams_current_mo"] = "WAMs Current"
names(table_2_1_4)[names(table_2_1_4) == "per_wams_current"] = "Per WAMs Current"

#Here is the code if we want to add back in denomiantors from prior 1 month or
#prior 2 months
#dplyr code
#exp_prior_2 = sum(wam_experienced[calendar_month == prior_2_months], na.rm=T), 
#exp_prior_1 = sum(wam_experienced[calendar_month == prior_1_month], na.rm=T),
#wams_2_mos_prior = sum(elig_exp_using[calendar_month == prior_2_months], na.rm=T), 
#wams_1_mo_prior = sum(elig_exp_using[calendar_month == prior_1_month], na.rm=T),
#regular code after table has been created through dplyr
#table_2_1_4$per_wams_prior_2 <- round((table_2_1_4$wams_2_mos_prior/table_2_1_4$exp_prior_2)*100, digits = 1)
#table_2_1_4$per_wams_prior_2[table_2_1_4$exp_prior_2 == 0] <- 0
#table_2_1_4$per_wams_prior_1 <- round((table_2_1_4$wams_1_mo_prior/table_2_1_4$exp_prior_1)*100, digits = 1)
#table_2_1_4$per_wams_prior_1[table_2_1_4$exp_prior_1 == 0] <- 0
#table_2_1_4$slope_prior_2_to_current <- (table_2_1_4$per_wams_current - table_2_1_4$per_wams_prior_2) / 2
#table_2_1_4$slope_prior_1_to_current <- (table_2_1_4$per_wams_current - table_2_1_4$per_wams_prior_1) / 1
#names(table_2_1_4)[names(table_2_1_4) == "exp_prior_2"] = "Experienced + Active Users Prior 2"
#names(table_2_1_4)[names(table_2_1_4) == "exp_prior_1"] = "Experienced + Active Users Prior 1"
#names(table_2_1_4)[names(table_2_1_4) == "wams_2_mos_prior"] = "WAMs Prior 2"
#names(table_2_1_4)[names(table_2_1_4) == "wams_1_mo_prior"] = "WAMs Prior 1"
#names(table_2_1_4)[names(table_2_1_4) == "per_wams_prior_2"] = "Per WAMs Prior 2"
#names(table_2_1_4)[names(table_2_1_4) == "per_wams_prior_1"] = "Per WAMs Prior 1"
#names(table_2_1_4)[names(table_2_1_4) == "slope_prior_2_to_current"] = "Slope Prior 2 to Current"
#names(table_2_1_4)[names(table_2_1_4) == "slope_prior_1_to_current"] = "Slope Prior 2 to Current"
#write.csv(table_2_1_4, file = "table_2_1_4.csv", row.names = F)

#Merge all sections to make overall table_2_1
table_2_1 <- merge(table_2_1_1, table_2_1_2, by = "Domain", all = T)
table_2_1 <- merge(table_2_1, table_2_1_3, by = "Domain", all = T)
table_2_1 <- merge(table_2_1, table_2_1_4, by = "Domain", all = T)

write.csv(table_2_1, file = "table_2_1_update.csv", row.names = F)

