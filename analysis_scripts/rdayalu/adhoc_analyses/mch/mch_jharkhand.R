#------------------------------------------------------------------------#
#Create general and specific case tables as described here:
#https://docs.google.com/document/d/1GJA85jrlNd9VYbcHP00zn9G4e6jH-e-IBGBaT_ijLWk/edit#bookmark=id.cqyhlmfcvdwy
#------------------------------------------------------------------------#

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

jhark <- filter(cases, domain == "jharkhand-mch")

#Read in form data
registration <- read.csv(file = "registration.csv", stringsAsFactors = F)
prenatal <- read.csv(file = "pregnancy.csv", stringsAsFactors = F)
baby_pp <- read.csv(file = "child_pp_jhark.csv", stringsAsFactors = F)
#postpartum <- read.csv(file = "postpartum.csv", stringsAsFactors = F)

#------------------------------------------------------------------------#
#Registration
#I checked all "update" possibilities for each of these variables. Don't use them. 
#These are the correct ones to use.
names(registration)[names(registration) == "form.meta.userID"] <- "user_id"
names(registration)[names(registration) == "form.case_open_pregnant_mother_0.case..case_id"] <- "case_id"
names(registration)[names(registration) == "form.meta.timeStart"] <- "time_start"
names(registration)[names(registration) == "form.edd"] <- "edd"
names(registration)[names(registration) == "form.mother_age"] <- "age_woman_response"
names(registration)[names(registration) == "form.whether_previous_preg"] <- "previously_preg"
names(registration)[names(registration) == "form.num_children_for_reg"] <- "num_children"
names(registration)[names(registration) == "form.caste"] <- "caste"
names(registration)[names(registration) == "form.is_pregnant"] <- "is_pregnant"

registration <- select(registration, user_id, case_id, time_start, edd, age_woman_response, 
                       previously_preg, num_children, caste, is_pregnant)

#Prepare registration table for case table
#This should be a one-to-one mapping
nrow(registration)
length(unique(registration$case_id))
#There are just over 100 rows with duplicate case_ids. Remove case_id == "---"
registration <- filter(registration, case_id != "---")
nrow(registration)
length(unique(registration$case_id))
#Registration date based on time stamp
registration$reg_date_form <- as.Date(substr(registration$time_start, 1, 10))
#is_pregnant
registration$is_pregnant[registration$is_pregnant == "yes"] <- T
registration$is_pregnant[registration$is_pregnant == "no"] <- F
registration$is_pregnant <- as.logical(registration$is_pregnant)
#previously_preg
registration$previously_preg[registration$previously_preg == "yes"] <- T
registration$previously_preg[registration$previously_preg == "no"] <- F
registration$previously_preg[registration$previously_preg == "---"] <- NA
registration$previously_preg <- as.logical(registration$previously_preg)
#EDD
registration$edd[registration$edd == ""] <- NA
registration$edd <- as.Date(registration$edd)
#Some EDDs have obviously wrong/weirdly formatted dates. Need to change those to NA 
#If EDD < registration date - 60 days, change EDD to NA
registration$edd[registration$edd < (registration$reg_date_form - 60)] <- NA

#registration <- select(registration, -c(username, time_start, row_num))
registration$has_registration_form <- T
jhark <- merge(jhark, registration, by = "case_id", all.x = T)
jhark$edd_before_dataset_end <- jhark$edd <= jhark$dataset_end_date   
jhark$edd_before_domain_end <- jhark$edd <= jhark$domain_last_form_date

#------------------------------------------------------------------------#

#Prenatal
names(prenatal)[names(prenatal) == "form.meta.userID"] <- "user_id"
names(prenatal)[names(prenatal) == "form.case..case_id"] <- "case_id"
names(prenatal)[names(prenatal) == "form.meta.timeStart"] <- "time_start"
names(prenatal)[names(prenatal) == "form.registration_done"] <- "reg_anm_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.registration_done"] <- "reg_anm_2"
names(prenatal)[names(prenatal) == "form.anc_malaria_test"] <- "malaria_test_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.anc_malaria_test"] <- "malaria_test_2"
names(prenatal)[names(prenatal) == "form.has_malaria"] <- "has_malaria_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.has_malaria"] <- "has_malaria_2"
names(prenatal)[names(prenatal) == "form.tetanus_1"] <- "tetanus_1_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.tetanus_1"] <- "tetanus_1_2"
names(prenatal)[names(prenatal) == "form.tetanus_2"] <- "tetanus_2_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.tetanus_2"] <- "tetanus_2_2"
names(prenatal)[names(prenatal) == "form.hiv_test"] <- "hiv_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.hiv_test"] <- "hiv_2"
names(prenatal)[names(prenatal) == "form.takes_iron_folic"] <- "iron_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.takes_iron_folic"] <- "iron_2"
names(prenatal)[names(prenatal) == "form.IFA"] <- "IFA_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.IFA"] <- "IFA_2"
names(prenatal)[names(prenatal) == "form.prepared_for_cost"] <- "cost_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.prepared_for_cost"] <- "cost_2"
names(prenatal)[names(prenatal) == "form.inst_delivery_plan"] <- "inst_del_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.inst_delivery_plan"] <- "inst_del_2"
#names(prenatal)[names(prenatal) == "form.case.update.child_dob"] <- "child_dob_1" #This is basically the same as the one we are picking below 
#names(prenatal)[names(prenatal) == "form.case.update.date_birth"] <- "child_dob_2" #Definitely don't use this one
names(prenatal)[names(prenatal) == "form.delivery_questions.date_birth"] <- "child_dob"
names(prenatal)[names(prenatal) == "form.takes_nutrition"] <- "nut_1"
names(prenatal)[names(prenatal) == "form.pregnancy_questions.takes_nutrition"] <- "nut_2"
names(prenatal)[names(prenatal) == "form.delivery_questions.place_birth"] <- "place_birth"

prenatal <- select(prenatal, user_id, case_id, time_start, reg_anm_1, reg_anm_2, malaria_test_1, 
                   malaria_test_2, has_malaria_1, has_malaria_2, tetanus_1_1, tetanus_1_2, tetanus_2_1, 
                   tetanus_2_2, hiv_1, hiv_2, iron_1, iron_2, IFA_1, IFA_2, cost_1, cost_2, inst_del_1, 
                   inst_del_2, child_dob, nut_1, nut_2, place_birth)

#Prepare prenatal table for case table
#This will not necessarily have one-to-one mapping, so we need to summarise over case_id
#Number of prenatal forms
prenatal$row_num <- 1:nrow(prenatal)
prenatal$prenatal_date <- as.Date(substr(prenatal$time_start, 1, 10))

#Registered with ANM?
prenatal$reg_anm_1[prenatal$reg_anm_1 == ""] <- NA
prenatal$reg_anm_1[prenatal$reg_anm_1 == "---"] <- NA
prenatal$reg_anm_1[prenatal$reg_anm_1 == "yes"] <- T
prenatal$reg_anm_1[prenatal$reg_anm_1 == "no"] <- F
prenatal$reg_anm_1 <- as.logical(prenatal$reg_anm_1)
prenatal$reg_anm_2[prenatal$reg_anm_2 == "---"] <- NA
prenatal$reg_anm_2[prenatal$reg_anm_2 == "yes"] <- T
prenatal$reg_anm_2[prenatal$reg_anm_2 == "no"] <- F
prenatal$reg_anm_2 <- as.logical(prenatal$reg_anm_2)
prenatal$registered_anm <- prenatal$reg_anm_1
prenatal$registered_anm[is.na(prenatal$registered_anm)] <- 
  prenatal$reg_anm_2[is.na(prenatal$registered_anm)]

#Did you receive a malaria test?
prenatal$malaria_test_1[prenatal$malaria_test_1 != "---" & 
                          prenatal$malaria_test_1 != "yes"] <- F
prenatal$malaria_test_1[prenatal$malaria_test_1 == "---"] <- NA
prenatal$malaria_test_1[prenatal$malaria_test_1 == "yes"] <- T
prenatal$malaria_test_1 <- as.logical(prenatal$malaria_test_1)
prenatal$malaria_test_2[prenatal$malaria_test_2 != "---" & 
                          prenatal$malaria_test_2 != "yes"] <- F
prenatal$malaria_test_2[prenatal$malaria_test_2 == "---"] <- NA
prenatal$malaria_test_2[prenatal$malaria_test_2 == "yes"] <- T
prenatal$malaria_test_2 <- as.logical(prenatal$malaria_test_2)
prenatal$malaria_tested <- prenatal$malaria_test_1
prenatal$malaria_tested[is.na(prenatal$malaria_tested)] <- 
  prenatal$malaria_test_2[is.na(prenatal$malaria_tested)]

#Diagnosed with malaria?
prenatal$has_malaria_1[prenatal$has_malaria_1 == "unknown"] <- NA
prenatal$has_malaria_1[prenatal$has_malaria_1 == "---"] <- NA
prenatal$has_malaria_1[prenatal$has_malaria_1 == "yes"] <- T
prenatal$has_malaria_1[prenatal$has_malaria_1 == "no"] <- F
prenatal$has_malaria_1 <- as.logical(prenatal$has_malaria_1)
prenatal$has_malaria_2[prenatal$has_malaria_2 == "unknown"] <- NA
prenatal$has_malaria_2[prenatal$has_malaria_2 == "---"] <- NA
prenatal$has_malaria_2[prenatal$has_malaria_2 == "yes"] <- T
prenatal$has_malaria_2[prenatal$has_malaria_2 == "no"] <- F
prenatal$has_malaria_2 <- as.logical(prenatal$has_malaria_2)
prenatal$has_malaria <- prenatal$has_malaria_1
prenatal$has_malaria[is.na(prenatal$has_malaria)] <- 
  prenatal$has_malaria_2[is.na(prenatal$has_malaria)]

#Received tetanus 1?
prenatal$tetanus_1_1[prenatal$tetanus_1_1 == ""] <- NA
prenatal$tetanus_1_1[prenatal$tetanus_1_1 == "---"] <- NA
prenatal$tetanus_1_1[prenatal$tetanus_1_1 == "yes"] <- T
prenatal$tetanus_1_1[prenatal$tetanus_1_1 == "no"] <- F
prenatal$tetanus_1_1 <- as.logical(prenatal$tetanus_1_1)
prenatal$tetanus_1_2[prenatal$tetanus_1_2 == "---"] <- NA
prenatal$tetanus_1_2[prenatal$tetanus_1_2 == "yes"] <- T
prenatal$tetanus_1_2[prenatal$tetanus_1_2 == "no"] <- F
prenatal$tetanus_1_2 <- as.logical(prenatal$tetanus_1_2)
prenatal$tetanus_1 <- prenatal$tetanus_1_1
prenatal$tetanus_1[is.na(prenatal$tetanus_1)] <- 
  prenatal$tetanus_1_2[is.na(prenatal$tetanus_1)]

#Received tetanus 2?
prenatal$tetanus_2_1[prenatal$tetanus_2_1 == ""] <- NA
prenatal$tetanus_2_1[prenatal$tetanus_2_1 == "---"] <- NA
prenatal$tetanus_2_1[prenatal$tetanus_2_1 == "yes"] <- T
prenatal$tetanus_2_1[prenatal$tetanus_2_1 == "no"] <- F
prenatal$tetanus_2_1 <- as.logical(prenatal$tetanus_2_1)
prenatal$tetanus_2_2[prenatal$tetanus_2_2 == "---"] <- NA
prenatal$tetanus_2_2[prenatal$tetanus_2_2 == "yes"] <- T
prenatal$tetanus_2_2[prenatal$tetanus_2_2 == "no"] <- F
prenatal$tetanus_2_2 <- as.logical(prenatal$tetanus_2_2)
prenatal$tetanus_2 <- prenatal$tetanus_2_1
prenatal$tetanus_2[is.na(prenatal$tetanus_2)] <- 
  prenatal$tetanus_2_2[is.na(prenatal$tetanus_2)]

#Received any tetanus vaccines?
prenatal$tetanus <- prenatal$tetanus_1 
prenatal$tetanus[is.na(prenatal$tetanus)] <- 
  prenatal$tetanus_2[is.na(prenatal$tetanus)]

#Received HIV test?
prenatal$hiv_1[prenatal$hiv_1 == ""] <- NA
prenatal$hiv_1[prenatal$hiv_1 == "---"] <- NA
prenatal$hiv_1[prenatal$hiv_1 == "yes"] <- T
prenatal$hiv_1[prenatal$hiv_1 == "no"] <- F
prenatal$hiv_1 <- as.logical(prenatal$hiv_1)
prenatal$hiv_2[prenatal$hiv_2 == "---"] <- NA
prenatal$hiv_2[prenatal$hiv_2 == "yes"] <- T
prenatal$hiv_2[prenatal$hiv_2 == "no"] <- F
prenatal$hiv_2 <- as.logical(prenatal$hiv_2)
prenatal$hiv_tested <- prenatal$hiv_1
prenatal$hiv_tested[is.na(prenatal$hiv_tested)] <- 
  prenatal$hiv_2[is.na(prenatal$hiv_tested)]

#Received iron?
prenatal$iron_1[prenatal$iron_1 == "---"] <- NA
prenatal$iron_1[prenatal$iron_1 == "yes"] <- T
prenatal$iron_1[prenatal$iron_1 == "no"] <- F
prenatal$iron_1 <- as.logical(prenatal$iron_1)
prenatal$iron_2[prenatal$iron_2 == "---"] <- NA
prenatal$iron_2[prenatal$iron_2 == "yes"] <- T
prenatal$iron_2[prenatal$iron_2 == "no"] <- F
prenatal$iron_2 <- as.logical(prenatal$iron_2)
prenatal$iron <- prenatal$iron_1
prenatal$iron[is.na(prenatal$iron)] <- 
  prenatal$iron_2[is.na(prenatal$iron)]

#Received IFA?
prenatal$IFA_1[prenatal$IFA_1 == ""] <- NA
prenatal$IFA_1[prenatal$IFA_1 == "---"] <- NA
prenatal$IFA_1[prenatal$IFA_1 == "IFA_yes"] <- T
prenatal$IFA_1[prenatal$IFA_1 == "IFA_no"] <- F
prenatal$IFA_1 <- as.logical(prenatal$IFA_1)
prenatal$IFA_2[prenatal$IFA_2 == "---"] <- NA
prenatal$IFA_2[prenatal$IFA_2 == "IFA_yes"] <- T
prenatal$IFA_2[prenatal$IFA_2 == "IFA_no"] <- F
prenatal$IFA_2 <- as.logical(prenatal$IFA_2)
prenatal$IFA <- prenatal$IFA_1
prenatal$IFA[is.na(prenatal$IFA)] <- 
  prenatal$IFA_2[is.na(prenatal$IFA)]

#Received any iron?
prenatal$iron_any <- prenatal$iron 
prenatal$iron_any[is.na(prenatal$iron_any)] <- 
  prenatal$IFA[is.na(prenatal$iron_any)]

#Preparing for delivery costs?
prenatal$cost_1[prenatal$cost_1 == "---"] <- NA
prenatal$cost_1[prenatal$cost_1 == "yes"] <- T
prenatal$cost_1[prenatal$cost_1 == "no"] <- F
prenatal$cost_1 <- as.logical(prenatal$cost_1)
prenatal$cost_2[prenatal$cost_2 == "---"] <- NA
prenatal$cost_2[prenatal$cost_2 == "yes"] <- T
prenatal$cost_2[prenatal$cost_2 == "no"] <- F
prenatal$cost_2 <- as.logical(prenatal$cost_2)
prenatal$cost <- prenatal$cost_1
prenatal$cost[is.na(prenatal$cost)] <- 
  prenatal$cost_2[is.na(prenatal$cost)]

#Planning instituational delivery?
prenatal$inst_del_1[prenatal$inst_del_1 == ""] <- NA
prenatal$inst_del_1[prenatal$inst_del_1 == "---"] <- NA
prenatal$inst_del_1[prenatal$inst_del_1 == "yes"] <- T
prenatal$inst_del_1[prenatal$inst_del_1 == "no"] <- F
prenatal$inst_del_1 <- as.logical(prenatal$inst_del_1)
prenatal$inst_del_2[prenatal$inst_del_2 == "---"] <- NA
prenatal$inst_del_2[prenatal$inst_del_2 == "yes"] <- T
prenatal$inst_del_2[prenatal$inst_del_2 == "no"] <- F
prenatal$inst_del_2 <- as.logical(prenatal$inst_del_2)
prenatal$inst_del <- prenatal$inst_del_1
prenatal$inst_del[is.na(prenatal$inst_del)] <- 
  prenatal$inst_del_2[is.na(prenatal$inst_del)]

#Format child_dob
prenatal$child_dob <- as.Date(prenatal$child_dob)

#Takes nutritious food?
prenatal$nut_1[prenatal$nut_1 == ""] <- NA
prenatal$nut_1[prenatal$nut_1 == "---"] <- NA
prenatal$nut_1[prenatal$nut_1 == "yes"] <- T
prenatal$nut_1[prenatal$nut_1 == "no"] <- F
prenatal$nut_1 <- as.logical(prenatal$nut_1)
prenatal$nut_2[prenatal$nut_2 == "---"] <- NA
prenatal$nut_2[prenatal$nut_2 == "yes"] <- T
prenatal$nut_2[prenatal$nut_2 == "no"] <- F
prenatal$nut_2 <- as.logical(prenatal$nut_2)
prenatal$nutrition <- prenatal$nut_1
prenatal$nutrition[is.na(prenatal$nutrition)] <- 
  prenatal$nut_2[is.na(prenatal$nutrition)]

#Place of birth
#Very few forms have complete data for this indicator
prenatal$hosp_delivery <- NA
prenatal$hosp_delivery[prenatal$place_birth == "at_home"] <- F
prenatal$hosp_delivery[prenatal$place_birth == "in_pvt_hosp"] <- T
prenatal$hosp_delivery[prenatal$place_birth == "public_hosp"] <- T

prenatal <- prenatal %>% group_by(case_id) %>% 
  mutate(most_recent_prenatal = prenatal_date == max(prenatal_date), 
         reg_anm_ever = sum(registered_anm, na.rm = T) > 0,
         malaria_tested_ever = sum(malaria_tested, na.rm = T) > 0,
         has_malaria_ever = sum(has_malaria, na.rm = T) > 0,
         tet_ever = sum(tetanus, na.rm = T) > 0, 
         hiv_tested_ever = sum(hiv_tested, na.rm = T) > 0,
         iron_ever = sum(iron_any, na.rm = T) > 0, 
         cost_ever = sum(cost, na.rm = T) > 0, 
         inst_del_ever = sum(inst_del, na.rm = T) > 0,
         nutrition_ever = sum(nutrition, na.rm = T) > 0, 
         hiv_1 = sum(hiv_1, na.rm = T) > 0, 
         hiv_2 = sum(hiv_2, na.rm = T) > 0, 
         hosp_delivery = sum(hosp_delivery, na.rm = T) > 0)

#Merge prenatal summaries to case table
prenatal_summary <- prenatal %>% group_by(case_id) %>% 
  summarise(num_prenatal_forms = length(unique(row_num)),
            num_prenatal_visits = length(unique(prenatal_date)),
            #most_recent_prenatal = unique(most_recent_prenatal),
            reg_anm_ever = unique(reg_anm_ever), 
            malaria_tested_ever = unique(malaria_tested_ever), 
            has_malaria_ever = unique(has_malaria_ever), 
            tet_ever = unique(tet_ever), 
            hiv_tested_ever = unique(hiv_tested_ever),
            iron_ever = unique(iron_ever),
            cost_ever = unique(cost_ever),
            inst_del_ever = unique(inst_del_ever),
            nutrition_ever = unique(nutrition_ever), 
            hiv_1 = unique(hiv_1), 
            hiv_2 = unique(hiv_2), 
            hosp_delivery = unique(hosp_delivery))
prenatal_summary$has_prenatal_form <- T
jhark <- merge(jhark, prenatal_summary, by = "case_id", all.x = T)

#------------------------------------------------------------------------#

#Baby postnatal
names(baby_pp)[names(baby_pp) == "info.caseid"] <- "case_id"
names(baby_pp)[names(baby_pp) == "info.started_time"] <- "time_start"
names(baby_pp)[names(baby_pp) == "baby_alive_group.premature_birth"] <- "preterm"
names(baby_pp)[names(baby_pp) == "baby_alive_group.weight"] <- "weight_kg"

baby_pp <- select(baby_pp, case_id, time_start, preterm, weight_kg)
baby_pp$row_num <- 1:nrow(baby_pp)

#Visit date
baby_pp$baby_pp_date <- as.Date(substr(baby_pp$time_start, 1, 10))

#Preterm birth?
baby_pp$preterm[baby_pp$preterm == ""] <- NA
baby_pp$preterm[baby_pp$preterm == "---"] <- NA
baby_pp$preterm[baby_pp$preterm == "yes"] <- T
baby_pp$preterm[baby_pp$preterm == "no"] <- F
baby_pp$preterm <- as.logical(baby_pp$preterm)

#Baby's weight
#Change weight >= 10 kgs and < 0 kgs to NA
baby_pp$weight_kg <- round(as.numeric(baby_pp$weight_kg), digits = 1)
baby_pp$weight_kg[baby_pp$weight_kg >= 10] <- NA
baby_pp$weight_kg[baby_pp$weight_kg < 0] <- NA
#Make LBW variable
baby_pp$lbw <- baby_pp$weight_kg < 2.5

baby_pp <- baby_pp %>% group_by(case_id) %>% 
  mutate(most_recent_baby_pp_date = baby_pp_date == max(baby_pp_date), 
         preterm_ever = sum(preterm, na.rm = T) > 0,
         lbw_ever = sum(lbw, na.rm = T) > 0)

#Merge prenatal summaries to case table
baby_pp_summary <- baby_pp %>% group_by(case_id) %>% 
  summarise(num_pp_child_forms = length(unique(row_num)),
            num_pp_child_visits = length(unique(baby_pp_date)),
            lbw = unique(lbw_ever), 
            preterm = unique(preterm_ever))
baby_pp_summary$has_pp_form <- T
jhark_child <- merge(jhark, baby_pp_summary, by = "case_id", all.x = T)
jhark_child <- filter(jhark_child, case_type == "child")
jhark_child <- select(jhark_child, case_id, parent_id, num_pp_child_visits, lbw, preterm, has_pp_form)
names(jhark_child)[names(jhark_child) == "case_id"] <- "child_case_id"
names(jhark_child)[names(jhark_child) == "parent_id"] <- "case_id"
jhark <- merge(jhark, jhark_child, by = "case_id", all.x = T) #Problem is when one case_id has multiple children
#Keep only woman case types
jhark <- filter(jhark, case_type == "pregnant_mother")

#Count total number of ANC visits per woman, including registration visits 
jhark$num_reg_visits <- 0
jhark$num_reg_visits[jhark$has_registration_form == T] <- 1
jhark$num_prenatal_forms[is.na(jhark$num_prenatal_forms)] <- 0
jhark$num_prenatal_visits[is.na(jhark$num_prenatal_visits)] <- 0
jhark$num_anc_visits <- jhark$num_reg_visits + jhark$num_prenatal_visits

#Format caste variable (scheduled caste/scheduled tribe)
jhark$sc_st <- NA
jhark$sc_st[jhark$caste == "sc"] <- T
jhark$sc_st[jhark$caste == "st"] <- T
jhark$sc_st[jhark$caste == "general"] <- F
jhark$sc_st[jhark$caste == "obc"] <- F
jhark$sc_st[jhark$caste == "others"] <- F

#Remove cases with unknown HIV testing status and unknown pregnancy status
jhark <- filter(jhark, !is.na(hiv_tested_ever))
jhark <- filter(jhark, !is.na(is_pregnant))

#Flag CHWs who have been active for at least 6 months (long-term CHWs) 
users_jhark <- jhark %>% group_by(user_pk) %>% 
  summarise(total_active_months = max(active_months_rolling, na.rm = T))
users_jhark$keep_user <- users_jhark$total_active_months >= 6
users_jhark <- filter(users_jhark, keep_user == T)

#Keep cases that have been created by only long-term CHWs 
jhark_keep <- filter(jhark, user_pk %in% users_jhark$user_pk)

#Separate cases that were registered by newer vs. older CHWs
jhark_lt_6 <- filter(jhark_keep, active_months_rolling < 6)
jhark_ge_6 <- filter(jhark_keep, active_months_rolling >= 6)

#Chi square tests
tbl <- table(jhark_keep$hiv_tested_ever, jhark_keep$inst_del_ever)
chisq.test(tbl)

#T-test
t.test(jhark_keep$active_months_rolling~jhark_keep$hiv_tested_ever)

#Select necessary columns for the final logistical model
glm_jhark <- select(jhark_keep, hiv_tested_ever, age_woman_response, previously_preg, sc_st, malaria_tested_ever, 
                    iron_ever, inst_del_ever, num_anc_visits, active_months_rolling, time_on_cc)

#Run logistic regression using the generalized linear model
#http://www.ats.ucla.edu/stat/r/dae/logit.htm
log_output <- glm(hiv_tested_ever ~ age_woman_response + previously_preg + sc_st + malaria_tested_ever + 
                    iron_ever + inst_del_ever + num_anc_visits + active_months_rolling + time_on_cc, 
                  data = glm_jhark, family = "binomial")

#Calculate odds ratios and 95% CIs and add to dataframe 
#Add p-values from model summary to the dataframe
q7 <- data.frame(cbind(log_output$coefficients, coef(summary(log_output))[,4], 
                       exp(cbind(OR = coef(log_output), confint(log_output)))))
q7 <- round(q7, 3)
names(q7) <- c("Coefficient estimate", "p-value of estimate", "Odds Ratio", "Lower limit of OR - 95% CI", "Upper limit of OR - 95% CI")
rownames(q7) <- c("Intercept", "Age", "Previously pregnant", "SC ST", "Malaria tested", "Takes iron", 
                  "Planning for instit delivery", "Number of ANC visits", "CHW months experience", "Time with CHW")


#------------------------------------------------------------------------#

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

#Bar chart of HIV testing proportion for CHW experience on CC at time of registration
woman_jhark$hiv <- NA
woman_jhark$hiv <- woman_jhark$hiv_tested_ever == T
woman_jhark$hiv[woman_jhark$hiv == F | is.na(woman_jhark$hiv)] <- F


#Bar chart of HIV testing by CHW experience level
dat <- data.frame(
  active = factor(c("< 6 mos",">= 6 mos"), levels=c("< 6 mos",">= 6 mos")),
  hiv = c(0.5, 9.2)
)

g <- ggplot(data=dat, aes(x=active, y=hiv, fill=active)) + 
  geom_bar(colour="black", fill="palegreen4", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("CHW's experience on CommCare at registration (# active mos)") + 
  ylab("Proportion of women that received HIV testing (%)") +
  ggtitle("HIV testing rate (%) by CHW's experience level on CommCare") +
  scale_y_continuous(limits=c(0,15)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size = 12))

#Logistic regression for HIV testing as explained by user experience (in months) and program
#experience (in months)

#First add a varaible for program experience (in months)
#Use date_opened over date_opened_final because min(date_opened) is more realistic
jhark_keep$month_opened <- floor_date(jhark_keep$date_opened, unit = "month")
#remove cases with implausible month_opened
jhark_keep <- filter(jhark_keep, month_opened != as.Date("2079-02-01"))
#Remove test cases
jhark_keep <- filter(jhark_keep, test_case == F)
#Create prog_active_months_rolling
#month_opened <- c(min(jhark_keep$month_opened))
#for(i in 1:22) {
#  d <- month_opened[i]
#  d_new <- d %m+% months(1)
#  month_opened <- c(month_opened, d_new)
#}
prog_exp <- data.frame(sort(unique(jhark_keep$month_opened)))
prog_exp$prog_active_months_rolling <- 1:nrow(prog_exp)
names(prog_exp)[names(prog_exp) == "sort.unique.jhark_keep.month_opened.."] <- "month_opened"
#Merge to jhark_keep
jhark_keep <- merge(jhark_keep, prog_exp, by = "month_opened", all.x = T)
#Removing women with unknown HIV test status
jhark_keep2 <- filter(jhark_keep, !is.na(hiv_tested_ever))

#Select necessary columns for the final logistical model
#glm_jhark <- select(jhark_keep, hiv, active_months_rolling, prog_active_months_rolling)
glm_jhark <- select(jhark_keep2, hiv, active_months_rolling, prog_active_months_rolling)

#Don't need this exclusion anymore since test cases have already been removed just above
#Remove implausible active_months_rolling
#table(glm_jhark$active_months_rolling)
#glm_jhark <- filter(glm_jhark, active_months_rolling < 445)

#Scatterplot of user months by program months
g <- ggplot(glm_jhark, aes(x=prog_active_months_rolling, y=active_months_rolling)) +
  geom_point(shape=1, position=position_jitter(width=0.25,height=.125)) +
  guides(fill=FALSE) +
  xlab("# active program months at client registration") + 
  ylab("# active months per CHW at client registration") +
  ggtitle("# CHW active months by # active program months") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size = 12))

cor(glm_jhark$active_months_rolling, glm_jhark$prog_active_months_rolling, 
    use="complete.obs", method="pearson")

#Summary of HIV testing 
summary(jhark_keep2$hiv_tested_ever)
summary(jhark_keep2$hiv)
table(glm_jhark$prog_active_months_rolling)
table(glm_jhark$active_months_rolling)
#summary(glm_jhark$hiv)

#Stacked bar plots of # women with/without HIV testing by user/program months
glm_jhark$row_num <- 1:nrow(glm_jhark)
glm_jhark$hiv_false <- glm_jhark$hiv == F

jhark_prog <- glm_jhark %>% group_by(prog_active_months_rolling) %>% 
  summarise(nobs = length(unique(row_num)), 
            hiv = sum(hiv), 
            hiv_false = sum(hiv_false))
jhark_prog$hiv_prop <- round((jhark_prog$hiv/jhark_prog$nobs)*100, digits = 1)
jhark_prog_plot <- select(jhark_prog, prog_active_months_rolling, hiv)
test <- select(jhark_prog, prog_active_months_rolling, hiv_false)
names(test) <- c("prog_active_months_rolling", "hiv")
jhark_prog_plot <- rbind(jhark_prog_plot, test)
jhark_prog_plot$hiv_tested <- c(rep("tested", 17), rep("not_tested", 17))
names(jhark_prog_plot) <- c("prog_active_months_rolling", "ncases", "hiv_tested")

g <- ggplot(data=jhark_prog_plot, aes(x=prog_active_months_rolling, y=ncases, fill=hiv_tested)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("tan3", "forestgreen")) + 
  #guides(fill=FALSE) +
  xlab("# active program months at client registration") + 
  ylab("# cases registered") +
  ggtitle("# cases registered by # active program months and HIV test status") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size = 12))

jhark_user <- glm_jhark %>% group_by(active_months_rolling) %>% 
  summarise(nobs = length(unique(row_num)), 
            hiv = sum(hiv), 
            hiv_false = sum(hiv_false))
jhark_user$hiv_prop <- round((jhark_user$hiv/jhark_user$nobs)*100, digits = 1)
jhark_user_plot <- select(jhark_user, active_months_rolling, hiv)
test <- select(jhark_user, active_months_rolling, hiv_false)
names(test) <- c("active_months_rolling", "hiv")
jhark_user_plot <- rbind(jhark_user_plot, test)
jhark_user_plot$hiv_tested <- c(rep("tested", 16), rep("not_tested", 16))
names(jhark_user_plot) <- c("active_months_rolling", "ncases", "hiv_tested")

g <- ggplot(data=jhark_user_plot, aes(x=active_months_rolling, y=ncases, fill=hiv_tested)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("tan3", "forestgreen")) + 
  #guides(fill=FALSE) +
  xlab("# active CHW months at client registration") + 
  ylab("# cases registered") +
  ggtitle("# cases registered by # active CHW months and HIV test status") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size = 12))

#Is a logistic fit called for here?
#http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html
#Plot proportion of hiv_tested/total over CHW/program active months
plot(hiv/nobs ~ active_months_rolling, data=jhark_user)
plot(hiv/nobs ~ prog_active_months_rolling, data=jhark_prog)

#Logistic model with just active program months
log_output <- glm(hiv ~ prog_active_months_rolling, 
                  data = glm_jhark, family = "binomial")

#Calculate odds ratios and 95% CIs and add to dataframe 
#Add p-values from model summary to the dataframe
q7 <- data.frame(cbind(log_output$coefficients, coef(summary(log_output))[,4], 
                       exp(cbind(OR = coef(log_output), confint(log_output)))))
q7 <- round(q7, 3)
names(q7) <- c("Coefficient estimate", "p-value of estimate", "Odds Ratio", "Lower limit of OR - 95% CI", "Upper limit of OR - 95% CI")
rownames(q7) <- c("Intercept", "Program active months")

#Logistic model with just active CHW months
log_output <- glm(hiv ~ active_months_rolling, 
                  data = glm_jhark, family = "binomial")

#Calculate odds ratios and 95% CIs and add to dataframe 
#Add p-values from model summary to the dataframe
q7 <- data.frame(cbind(log_output$coefficients, coef(summary(log_output))[,4], 
                       exp(cbind(OR = coef(log_output), confint(log_output)))))
q7 <- round(q7, 3)
names(q7) <- c("Coefficient estimate", "p-value of estimate", "Odds Ratio", "Lower limit of OR - 95% CI", "Upper limit of OR - 95% CI")
rownames(q7) <- c("Intercept", "CHW active months")

#Logistic model with duplicated explanatory variable
glm_jhark$chw_dup <- glm_jhark$prog_active_months_rolling
log_output <- glm(hiv ~ prog_active_months_rolling + chw_dup, 
                  data = glm_jhark, family = "binomial")

#Calculate odds ratios and 95% CIs and add to dataframe 
#Add p-values from model summary to the dataframe
q7 <- data.frame(cbind(log_output$coefficients, coef(summary(log_output))[,4], 
                       exp(cbind(OR = coef(log_output), confint(log_output)))))
q7 <- round(q7, 3)
names(q7) <- c("Coefficient estimate", "p-value of estimate", "Odds Ratio", "Lower limit of OR - 95% CI", "Upper limit of OR - 95% CI")
rownames(q7) <- c("Intercept", "CHW active months", "CHW dup")

#Run logistic regression using the generalized linear model
#http://www.ats.ucla.edu/stat/r/dae/logit.htm
log_output <- glm(hiv ~ active_months_rolling + prog_active_months_rolling, 
                  data = glm_jhark, family = "binomial")

#Calculate odds ratios and 95% CIs and add to dataframe 
#Add p-values from model summary to the dataframe
q7 <- data.frame(cbind(log_output$coefficients, coef(summary(log_output))[,4], 
                       exp(cbind(OR = coef(log_output), confint(log_output)))))
q7 <- round(q7, 3)
names(q7) <- c("Coefficient estimate", "p-value of estimate", "Odds Ratio", "Lower limit of OR - 95% CI", "Upper limit of OR - 95% CI")
rownames(q7) <- c("Intercept", "CHW active months", "Program active months")

#Write as csv to output directory
write.csv(q7, file = "OR_prog_user_exp.csv", row.names = T)





