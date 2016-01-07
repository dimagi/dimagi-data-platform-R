#------------------------------------------------------------------------#
#Create general and specific case tables as described here:
#https://docs.google.com/document/d/1GJA85jrlNd9VYbcHP00zn9G4e6jH-e-IBGBaT_ijLWk/edit#bookmark=id.cqyhlmfcvdwy
#------------------------------------------------------------------------#

#THIS DOMAIN IS NOT IN THE DP CASE TABLE BECAUSE IT WAS AUTOMATICALLY FILTERED OUT oF MCH DOMAINS
#HAS SUBSECTOR = OTHER

#Use msante app for ssqh-cs
ssqh <- filter(cases, domain == "ssqh-cs")

#Read in form data
registration <- read.csv(file = "registration.csv", stringsAsFactors = F)
prenatal <- read.csv(file = "prenatal.csv", stringsAsFactors = F)
postpartum <- read.csv(file = "postpartum.csv", stringsAsFactors = F)

#Format wvug form tables
#Registration
names(registration)[names(registration) == "info.username"] <- "username"
names(registration)[names(registration) == "case_open_pregnant_mother_0.case..case_id"] <- "case_id"
names(registration)[names(registration) == "info.started_time"] <- "time_start"
names(registration)[names(registration) == "mother_age"] <- "age_woman_response"
names(registration)[names(registration) == "whether_previous_preg"] <- "previously_preg"
names(registration)[names(registration) == "num_children_for_reg"] <- "num_children"
registration <- select(registration, username, case_id, time_start, edd, age_woman_response, 
                       previously_preg, num_children, is_pregnant)

#Prenatal
names(prenatal)[names(prenatal) == "info.username"] <- "username"
names(prenatal)[names(prenatal) == "info.caseid"] <- "case_id"
names(prenatal)[names(prenatal) == "info.started_time"] <- "time_start"
names(prenatal)[names(prenatal) == "pregnancy_questions.takes_iron_folic"] <- "iron"
names(prenatal)[names(prenatal) == "pregnancy_questions.tetanus_1"] <- "tet_1"
names(prenatal)[names(prenatal) == "pregnancy_questions.tetanus_2"] <- "tet_2"
names(prenatal)[names(prenatal) == "pregnancy_questions.hiv_test"] <- "hiv_tested"
#names(prenatal)[names(prenatal) == "delivery_questions.repeat_for_baby.birth_delivery_conducted_by"] <- "birth_attendant"
prenatal <- select(prenatal, username, case_id, time_start, iron, tet_1, tet_2, hiv_tested)


#Postpartum
names(postpartum)[names(postpartum) == "info.username"] <- "username"
names(postpartum)[names(postpartum) == "info.caseid"] <- "case_id"
names(postpartum)[names(postpartum) == "info.started_time"] <- "time_start"
names(postpartum)[names(postpartum) == "show_birth_date"] <- "date_delivery"
names(postpartum)[names(postpartum) == "breastfeeding_problems_group.exclusive_breastfeeding"] <- "problems_breastfeeding_1"
names(postpartum)[names(postpartum) == "breastfeeding_problems_group.pain"] <- "problems_breastfeeding_2"
names(postpartum)[names(postpartum) == "breastfeeding_problems_group.cracked_nipples"] <- "problems_breastfeeding_3"
names(postpartum)[names(postpartum) == "breastfeeding_problems_group.breast_infection"] <- "problems_breastfeeding_4"
names(postpartum)[names(postpartum) == "visit_questions.family_planning"] <- "using_fp_pp"
postpartum <- select(postpartum, username, case_id, time_start, date_delivery, mother_alive, place_birth, 
                     problems_breastfeeding_1, problems_breastfeeding_2, problems_breastfeeding_3, 
                     problems_breastfeeding_4, using_fp_pp)

#Prepare registration table for case table
#This should be a one-to-one mapping
nrow(registration)
length(unique(registration$case_id))
#There are just over 100 rows with duplicate case_ids. Remove case_id == "---"
registration <- filter(registration, case_id != "---")
registration$row_num <- 1:nrow(registration)

#registration$reg_date <- as.Date(substr(registration$time_start, 1, 10))
#registration$dob_woman <- as.Date(registration$dob_woman)
#registration$age_woman_calc <- round(as.numeric(registration$reg_date - registration$dob_woman)/365, digits = 0)

registration$previously_preg[registration$previously_preg == "yes"] <- T
registration$previously_preg[registration$previously_preg == "no"] <- F
registration$previously_preg[registration$previously_preg == "---"] <- NA
registration$previously_preg <- as.logical(registration$previously_preg)

#registration$num_prev_preg <- as.numeric(registration$num_prev_preg)

registration$is_pregnant[registration$is_pregnant == "yes"] <- T
registration$is_pregnant[registration$is_pregnant == "no"] <- F
registration$is_pregnant <- as.logical(registration$is_pregnant)

#registration$using_fp[registration$using_fp == "yes"] <- T
#registration$using_fp[registration$using_fp == "no"] <- F
#registration$using_fp[registration$using_fp == "---"] <- NA
#registration$using_fp <- as.logical(registration$using_fp)

registration$edd[registration$edd == "---"] <- NA
registration$edd[registration$edd == ""] <- NA
registration$edd <- as.Date(registration$edd, format = "%m/%d/%Y")

registration <- select(registration, -c(username, time_start, row_num))
registration$has_registration_form <- T
jhark <- merge(jhark, registration, by = "case_id", all.x = T)
jhark$edd_before_dataset_end <- jhark$edd <= jhark$dataset_end_date   
jhark$edd_before_domain_end <- jhark$edd <= jhark$domain_last_form_date

#Prepare prenatal table for case table
#This will not necessarily have one-to-one mapping, so we need to summarise over case_id
#Number of prenatal forms
prenatal$row_num <- 1:nrow(prenatal)
prenatal$prenatal_date <- as.Date(substr(prenatal$time_start, 1, 10))

prenatal$iron[prenatal$iron == "---"] <- NA
prenatal$iron[prenatal$iron == "yes"] <- T
prenatal$iron[prenatal$iron == "no"] <- F
prenatal$iron <- as.logical(prenatal$iron)

prenatal$tet_1[prenatal$tet_1 == "---"] <- NA
prenatal$tet_1[prenatal$tet_1 == "yes"] <- T
prenatal$tet_1[prenatal$tet_1 == "no"] <- F
prenatal$tet_1 <- as.logical(prenatal$tet_1)

prenatal$tet_2[prenatal$tet_2 == "---"] <- NA
prenatal$tet_2[prenatal$tet_2 == "yes"] <- T
prenatal$tet_2[prenatal$tet_2 == "no"] <- F
prenatal$tet_2 <- as.logical(prenatal$tet_2)

prenatal$hiv_tested[prenatal$hiv_tested == "---"] <- NA
prenatal$hiv_tested[prenatal$hiv_tested == "yes"] <- T
prenatal$hiv_tested[prenatal$hiv_tested == "no"] <- F
prenatal$hiv_tested <- as.logical(prenatal$hiv_tested)

prenatal <- prenatal %>% group_by(case_id) %>% 
  mutate(most_recent_prenatal = prenatal_date == max(prenatal_date), 
         iron_ever = sum(iron, na.rm = T) > 0, 
         tet_1_ever = sum(tet_1, na.rm = T) > 0, 
         tet_2_ever = sum(tet_2, na.rm = T) > 0, 
         hiv_tested_ever = sum(hiv_tested, na.rm = T) > 0)

prenatal_summary <- prenatal %>% group_by(case_id) %>% 
  summarise(num_prenatal_forms = length(unique(row_num)), 
            iron_ever = unique(iron_ever), 
            tet_1_ever = unique(tet_1_ever), 
            tet_2_ever = unique(tet_2_ever), 
            hiv_tested_ever = unique(hiv_tested_ever))
prenatal_summary$has_prenatal_form <- T
jhark <- merge(jhark, prenatal_summary, by = "case_id", all.x = T)
jhark$num_reg_visits <- 0
jhark$num_reg_visits[jhark$has_registration_form == T] <- 1
jhark$num_prenatal_forms[is.na(jhark$num_prenatal_forms)] <- 0
jhark$num_anc_visits <- jhark$num_reg_visits + jhark$num_prenatal_forms

#Prepare postpartum table for case table
#This will not necessarily have one-to-one mapping, so we need to summarise over case_id
#Number of postpartum forms
postpartum$row_num <- 1:nrow(postpartum)
postpartum$postpartum_date <- as.Date(substr(postpartum$time_start, 1, 10))
postpartum$date_delivery <- as.Date(postpartum$date_delivery, format = "%m/%d/%Y")

postpartum$mother_alive[postpartum$mother_alive == ""] <- NA
postpartum$mother_alive[postpartum$mother_alive == "yes"] <- T
postpartum$mother_alive[postpartum$mother_alive == "no"] <- F
postpartum$mother_alive <- as.logical(postpartum$mother_alive)

#postpartum$birth_attendant[postpartum$birth_attendant == "---"] <- NA

postpartum$place_birth[postpartum$place_birth == "---"] <- NA
postpartum$place_birth[postpartum$place_birth == ""] <- NA

postpartum$using_fp_pp[postpartum$using_fp_pp == "---"] <- NA

postpartum$problems_breastfeeding_1[postpartum$problems_breastfeeding_1 == "---"] <- NA
postpartum$problems_breastfeeding_1[postpartum$problems_breastfeeding_1 == "yes"] <- T
postpartum$problems_breastfeeding_1[postpartum$problems_breastfeeding_1 == "no"] <- F
postpartum$problems_breastfeeding_1 <- as.logical(postpartum$problems_breastfeeding_1)

postpartum$problems_breastfeeding_2[postpartum$problems_breastfeeding_2 == "---"] <- NA
postpartum$problems_breastfeeding_2[postpartum$problems_breastfeeding_2 == "yes"] <- T
postpartum$problems_breastfeeding_2[postpartum$problems_breastfeeding_2 == "no"] <- F
postpartum$problems_breastfeeding_2 <- as.logical(postpartum$problems_breastfeeding_2)

postpartum$problems_breastfeeding_3[postpartum$problems_breastfeeding_3 == "---"] <- NA
postpartum$problems_breastfeeding_3[postpartum$problems_breastfeeding_3 == "yes"] <- T
postpartum$problems_breastfeeding_3[postpartum$problems_breastfeeding_3 == "no"] <- F
postpartum$problems_breastfeeding_3 <- as.logical(postpartum$problems_breastfeeding_3)

postpartum$problems_breastfeeding_4[postpartum$problems_breastfeeding_4 == "---"] <- NA
postpartum$problems_breastfeeding_4[postpartum$problems_breastfeeding_4 == "yes"] <- T
postpartum$problems_breastfeeding_4[postpartum$problems_breastfeeding_4 == "no"] <- F
postpartum$problems_breastfeeding_4 <- as.logical(postpartum$problems_breastfeeding_4)

postpartum$problems_breastfeeding <- NA
postpartum$problems_breastfeeding[postpartum$problems_breastfeeding_1 == T | postpartum$problems_breastfeeding_2 == T |
                                    postpartum$problems_breastfeeding_3 == T | postpartum$problems_breastfeeding_4 == T] <- T
postpartum$problems_breastfeeding[postpartum$problems_breastfeeding_1 == F & postpartum$problems_breastfeeding_2 == F &
                                    postpartum$problems_breastfeeding_3 == F & postpartum$problems_breastfeeding_4 == F] <- F

postpartum$mother_deceased <- NA
postpartum$mother_deceased[postpartum$mother_alive == F] <- T
postpartum$mother_deceased[postpartum$mother_alive == T] <- F

postpartum <- postpartum %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date), 
         mother_alive_ever = sum(mother_alive, na.rm = T) > 0,
         mother_deceased_ever = sum(mother_deceased, na.rm = T) > 0,
         prob_bf_ever = sum(problems_breastfeeding, na.rm = T) > 0)

pp_case <- postpartum %>% group_by(case_id) %>% 
  summarise(mother_alive_ever = unique(mother_alive_ever), 
            mother_deceased_ever = unique(mother_deceased_ever), 
            prob_bf_ever = unique(prob_bf_ever),
            num_pp_forms = length(unique(row_num)))
jhark <- merge(jhark, pp_case, by = "case_id", all.x = T)

dd_clean <- filter(postpartum, !is.na(date_delivery))
dd_clean <- dd_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
dd_clean <- filter(dd_clean, most_recent_postpartum == T)
dd_clean_case <- dd_clean %>% group_by(case_id) %>% 
  summarise(dd_new = unique(date_delivery))
jhark <- merge(jhark, dd_clean_case, by = "case_id", all.x = T)

place_clean <- filter(postpartum, !is.na(place_birth))
place_clean <- place_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
place_clean <- filter(place_clean, most_recent_postpartum == T)
place_clean_case <- place_clean %>% group_by(case_id) %>% 
  summarise(place_new = unique(place_birth))
jhark <- merge(jhark, place_clean_case, by = "case_id", all.x = T)

fp_clean <- filter(postpartum, !is.na(using_fp_pp))
fp_clean <- fp_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
fp_clean <- filter(fp_clean, most_recent_postpartum == T)
#This isn't correct, but let's do this for now
fp_clean <- filter(fp_clean, duplicated(case_id) == F)
fp_clean_case <- fp_clean %>% group_by(case_id) %>% 
  summarise(fp_new = unique(place_birth))
jhark <- merge(jhark, fp_clean_case, by = "case_id", all.x = T)

woman_jhark <- filter(jhark, case_type == "pregnant_mother")
write.csv(woman_jhark, file = "woman_jhark.csv", row.names = F)
