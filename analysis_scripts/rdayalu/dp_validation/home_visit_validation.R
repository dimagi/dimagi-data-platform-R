#This script is to manually use the form annotations file
#to make sure that the home_visit flag in the visit_detail table
#has been correctly set
#Based on this script, everything looks good!
#Note:
#1) I only ran this for MCH domains because this was in the middle of my MCH analysis. Other sector domains should be fine too, 
#but you should double check if you want to be absolutely sure
#2) ceasefire domain is spelled wrong in the annotation file, so I don't think ceasefire visits have been correctly annotated
#in the visit_detail table. Might want to do that manually if you are working with that domain. 

#Get domain table
domain <- get_domain_table(db)
names(domain)[names(domain) == "id"] = "domain_pk"
names(domain)[names(domain) == "name"] = "domain"
domain$subsector <- lapply(domain$subsector, as.character)
domains_mch_subsector <- seq_along(domain$subsector)[sapply(domain$subsector, FUN=function(X) "Maternal, Newborn, & Child Health" %in% X)]
domain$mch_subsector <- F
domain$mch_subsector[domains_mch_subsector] <- T
domain$countries <- gsub('[{]', "", domain$countries)
domain$countries <- gsub('[}]', "", domain$countries)
domain$countries <- gsub('"', "", domain$countries)
domain$countries[domain$countries == "No country"] <- NA
domain$countries[domain$countries == ""] <- NA
domain$self_start <- NA
domain$self_start[domain$internal.self_started == "False"] <- F
domain$self_start[domain$internal.self_started == "True"] <- T
domain$domain_start_date <- as.Date(substr(domain$cp_first_form, 1, 10))
domain$domain_last_form_date <- as.Date(substr(domain$cp_last_form, 1, 10))

#Get visit_detail data source
#Takes ~50 minutes to load
#Visit detail has additional information such as nforms, visit duration, home_visit annotations that the main visit table
#doesn't have
visit <- get_visit_detail(db)
names(visit)[names(visit) == "id"] <- "visit_pk"
names(visit)[names(visit) == "num_forms"] <- "nforms"
#Don't use total_forms because it looks like it carries over nforms from other visits (e.g. demo_user has total_forms ~6K)
#Have confirmed that num_forms is fine to use for the overwhelming majority of visits that don't have duplicate visit_pk
visit$visit_duration <- round(as.numeric(difftime(visit$time_end, visit$time_start, units = "mins")), digits = 2)
#Merge in domain table and keep only MCH domains with is_test = F
visit <- merge(visit, select(domain, domain_pk, domain, is_test, mch_subsector, countries, self_start, domain_start_date, 
                             domain_last_form_date), by = "domain", all.x = T)
visit <- filter(visit, mch_subsector == T)
visit <- filter(visit, is_test == "false")

#Get form table
#Importing the form table takes almost 1 hour.
form <- tbl(db, "form")
form <- collect(form)
names(form)[names(form) == "id"] = "form_pk"
names(form)[names(form) == "user_id"] = "user_pk"
names(form)[names(form) == "visit_id"] = "visit_pk"
names(form)[names(form) == "domain_id"] = "domain_pk"
names(form)[names(form) == "formdef_id"] = "formdef_pk"
names(form)[names(form) == "application_id"] = "app_pk"
#Keep only MCH domains
form <- filter(form, domain_pk %in% visit$domain_pk)
#Calculate days to server per form
form$days_to_server <- as.numeric(difftime(form$received_on, form$time_end, units="days"))

#Get application table
#This takes < 1 minute to collect
#Need this table in the form_annotations steps
app <- tbl(db, "application")
app <- collect(app)
names(app)[names(app) == "id"] = "app_pk"
names(app)[names(app) == "domain_id"] = "domain_pk"
#Keep only MCH domains
app <- filter(app, domain_pk %in% visit$domain_pk)

#Import form annotations file
form_annotations <- read.csv(file = "form_annotations.csv", stringsAsFactors = F)
form_annotations <- form_annotations[,1:34]
names(form_annotations)[names(form_annotations) == "Domain.name"] = "domain"
names(form_annotations)[names(form_annotations) == "Form.xmlns"] = "xmlns"
names(form_annotations)[names(form_annotations) == "Application.ID"] = "app_id"
#Correct spelling errors in annotation file
form_annotations$domain[form_annotations$domain == "Ceasefire"] <- "ceasefire"
table(form_annotations$Travel.visit, useNA = "always")
form_annotations$travel_form <- NA 
form_annotations$travel_form[form_annotations$Travel.visit == "Yes"] <- T
form_annotations$travel_form[form_annotations$Travel.visit == "yes"] <- T
form_annotations$travel_form[form_annotations$Travel.visit == "No"] <- F
form_annotations$travel_form[form_annotations$Travel.visit == "no"] <- F
#Merge in domain_pk
form_annotations <- merge(form_annotations, select(domain, domain, domain_pk), by = "domain", all.x = T)
#Keep only MCH domains in domain table and form annotation table 
domain <- filter(domain, mch_subsector == T & is_test == "false")
form_annotations <- filter(form_annotations, domain_pk %in% visit$domain_pk)
#Remove forms annotation rows with xmlns == "None. There are no forms with xmlns = NA
form_annotations <- filter(form_annotations, xmlns != "None")
#Merge in app_pk since xmlns is not unique and can be duplicated across apps/domains
#We need app_pk to correctly merge the travel annotation to the correct app in the correct domain later on
#App IDs are not duplicated across domains in the list of MCH apps we have here. 
#All form_annotation app_ids are present in the app list, so we are good to go.
#test <- app %>% group_by(app_id) %>% summarise(ndomains = length(unique(domain_pk)))
#Problem is that we can have multiple app_pks for a single app_id, so this will create duplicate
#rows in the form_annotations table. We will go with this for now. 
form_annotations <- merge(form_annotations, select(app, app_pk, app_id), by = "app_id", all.x = T)

#Get formdef table
#Takes ~1 minute to collect
formdef <- tbl(db, "formdef")
formdef <- collect(formdef)
names(formdef)[names(formdef) == "id"] = "formdef_pk"
names(formdef)[names(formdef) == "domain_id"] = "domain_pk"
names(formdef)[names(formdef) == "application_id"] = "app_pk"
#Keep only MCH domains
formdef <- filter(formdef, domain_pk %in% visit$domain_pk)
#Merge in travel_form from form_annotations file based on domain_pk, app_pk and xmlns
formdef <- merge(formdef, select(form_annotations, app_pk, xmlns, travel_form), by = c("app_pk", "xmlns"), all.x = T)
#Merge travel_form into form table
form <- merge(form, select(formdef, formdef_pk, travel_form), by = "formdef_pk", all.x = T)

#Does the form annotation file match with the home_visit column in the visit table?
travel_visits <- filter(form, !is.na(visit_pk)) %>% group_by(visit_pk) %>% 
  summarise(travel_visit = sum(travel_form == T, na.rm = T) > 0)
travel_visits$is_home_visit <- travel_visits$visit_pk %in% filter(visit, home_visit == T)$visit_pk 
travel_visits$correct_flag <- travel_visits$travel_visit == travel_visits$is_home_visit

summary(travel_visits$correct_flag)

#Mode      FALSE     TRUE     NA's 
#logical       7   4939301       0 

test <- filter(travel_visits, correct_flag == F)
test2 <- filter(visit, visit_pk %in% test$visit_pk)

#home_visit flag in DB seems to be correct, so we can go with that!
#The 7 visit_pks that aren't matching up above are from demo_users across domains with duplicated visit_pks, 
#so there's nothing to really worry about for our purposes 