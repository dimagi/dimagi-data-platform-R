#------------------------------------------------------------------------#
#Create general and specific case tables as described here:
#https://docs.google.com/document/d/1GJA85jrlNd9VYbcHP00zn9G4e6jH-e-IBGBaT_ijLWk/edit#bookmark=id.cqyhlmfcvdwy
#------------------------------------------------------------------------#

#Use TTC app for wvug
c_wvug <- filter(cases, domain_pk == 182)

#Read in form data
registration <- read.csv(file = "wvug_registration.csv", stringsAsFactors = F)
prenatal <- read.csv(file = "wvug_prenatal.csv", stringsAsFactors = F)
postpartum <- read.csv(file = "wvug_postpartum.csv", stringsAsFactors = F)

#Format wvug form tables
#Registration
names(registration)[names(registration) == "form.case..user_id"] <- "user_id"
names(registration)[names(registration) == "form.case..case_id"] <- "case_id"
names(registration)[names(registration) == "form.meta.timeStart"] <- "time_start"
names(registration)[names(registration) == "form.pregnancy_questions.edd"] <- "edd"
names(registration)[names(registration) == "form.case.create.case_type"] <- "case_type"
names(registration)[names(registration) == "form.mother_dob"] <- "dob_woman"
names(registration)[names(registration) == "form.age_mother_years"] <- "age_woman_response"
names(registration)[names(registration) == "form.previous_pregnancies"] <- "previously_preg"
names(registration)[names(registration) == "form.num_previous_pregnancies"] <- "num_prev_preg"
names(registration)[names(registration) == "form.num_living_children"] <- "num_children"
names(registration)[names(registration) == "form.relationship_status"] <- "marital_status"
names(registration)[names(registration) == "form.is_pregnant"] <- "is_pregnant"
names(registration)[names(registration) == "form.not_pregnant_questions.using_fp"] <- "using_fp"
registration <- select(registration, user_id, case_id, time_start, edd, case_type, dob_woman, age_woman_response, 
                       previously_preg, num_prev_preg, num_children, marital_status, is_pregnant, using_fp)

#Prenatal
names(prenatal)[names(prenatal) == "form.case..user_id"] <- "user_id"
names(prenatal)[names(prenatal) == "form.case..case_id"] <- "case_id"
names(prenatal)[names(prenatal) == "form.meta.timeStart"] <- "time_start"
names(prenatal)[names(prenatal) == "form.visit_questions.iron_folic"] <- "iron"
names(prenatal)[names(prenatal) == "form.visit_questions.tet_1"] <- "tet_1"
names(prenatal)[names(prenatal) == "form.visit_questions.tet_2"] <- "tet_2"
names(prenatal)[names(prenatal) == "form.visit_questions.hiv_tested"] <- "hiv_tested"
prenatal <- select(prenatal, user_id, case_id, time_start, iron, tet_1, tet_2, hiv_tested)


#Postpartum
names(postpartum)[names(postpartum) == "form.case..user_id"] <- "user_id"
names(postpartum)[names(postpartum) == "form.case..case_id"] <- "case_id"
names(postpartum)[names(postpartum) == "form.meta.timeStart"] <- "time_start"
names(postpartum)[names(postpartum) == "form.case_delivery_date"] <- "date_delivery"
names(postpartum)[names(postpartum) == "form.mother_alive"] <- "mother_alive"
names(postpartum)[names(postpartum) == "form.visit_questions.birth_attendant"] <- "birth_attendant"
names(postpartum)[names(postpartum) == "form.visit_questions.place_of_birth"] <- "place_birth"
names(postpartum)[names(postpartum) == "form.visit_questions.problems_breastfeeding"] <- "problems_breastfeeding"
names(postpartum)[names(postpartum) == "form.visit_questions.vitamin_a_mother"] <- "vit_a_birth"
postpartum <- select(postpartum, user_id, case_id, time_start, date_delivery, mother_alive, birth_attendant, place_birth, 
                     problems_breastfeeding, vit_a_birth)

#Prepare registration table for case table
#This should be a one-to-one mapping
nrow(registration)
#3988 rows
length(unique(registration$case_id))
#There are just over 100 rows with duplicate case_ids. Remove case_id == "---"
registration <- filter(registration, case_id != "---")

registration$reg_date <- as.Date(substr(registration$time_start, 1, 10))
registration$dob_woman <- as.Date(registration$dob_woman)
registration$age_woman_calc <- round(as.numeric(registration$reg_date - registration$dob_woman)/365, digits = 0)

registration$previously_preg[registration$previously_preg == "yes"] <- T
registration$previously_preg[registration$previously_preg == "no"] <- F
registration$previously_preg[registration$previously_preg == ""] <- NA
registration$previously_preg <- as.logical(registration$previously_preg)

registration$num_prev_preg <- as.numeric(registration$num_prev_preg)

registration$is_pregnant[registration$is_pregnant == "yes"] <- T
registration$is_pregnant[registration$is_pregnant == "no"] <- F
registration$is_pregnant <- as.logical(registration$is_pregnant)

registration$using_fp[registration$using_fp == "yes"] <- T
registration$using_fp[registration$using_fp == "no"] <- F
registration$using_fp[registration$using_fp == "---"] <- NA
registration$using_fp <- as.logical(registration$using_fp)

registration$edd[registration$edd == "---"] <- NA
registration$edd[registration$edd == ""] <- NA
registration$edd <- as.Date(registration$edd)

registration <- select(registration, -c(user_id, time_start, case_type, reg_date))
registration$has_registration_form <- T
c_wvug <- merge(c_wvug, registration, by = "case_id", all.x = T)
c_wvug$edd_before_dataset_end <- c_wvug$edd <= c_wvug$dataset_end_date   
c_wvug$edd_before_domain_end <- c_wvug$edd <= c_wvug$domain_last_form_date

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
c_wvug <- merge(c_wvug, prenatal_summary, by = "case_id", all.x = T)
c_wvug$num_reg_visits <- 0
c_wvug$num_reg_visits[c_wvug$has_registration_form == T] <- 1
c_wvug$num_prenatal_forms[is.na(c_wvug$num_prenatal_forms)] <- 0
c_wvug$num_anc_visits <- c_wvug$num_reg_visits + c_wvug$num_prenatal_forms

#Prepare postpartum table for case table
#This will not necessarily have one-to-one mapping, so we need to summarise over case_id
#Number of postpartum forms
postpartum$row_num <- 1:nrow(postpartum)
postpartum$postpartum_date <- as.Date(substr(postpartum$time_start, 1, 10))
postpartum$date_delivery <- as.Date(postpartum$date_delivery)

postpartum$mother_alive[postpartum$mother_alive == "---"] <- NA
postpartum$mother_alive[postpartum$mother_alive == "yes"] <- T
postpartum$mother_alive[postpartum$mother_alive == "no"] <- F
postpartum$mother_alive <- as.logical(postpartum$mother_alive)

postpartum$birth_attendant[postpartum$birth_attendant == "---"] <- NA

postpartum$place_birth[postpartum$place_birth == "---"] <- NA

postpartum$problems_breastfeeding[postpartum$problems_breastfeeding == "---"] <- NA
postpartum$problems_breastfeeding[postpartum$problems_breastfeeding == "yes"] <- T
postpartum$problems_breastfeeding[postpartum$problems_breastfeeding == "no"] <- F
postpartum$problems_breastfeeding <- as.logical(postpartum$problems_breastfeeding)

postpartum$vit_a_birth[postpartum$vit_a_birth == "---"] <- NA
postpartum$vit_a_birth[postpartum$vit_a_birth == "yes"] <- T
postpartum$vit_a_birth[postpartum$vit_a_birth == "no"] <- F
postpartum$vit_a_birth <- as.logical(postpartum$vit_a_birth)

postpartum$mother_deceased <- NA
postpartum$mother_deceased[postpartum$mother_alive == F] <- T
postpartum$mother_deceased[postpartum$mother_alive == T] <- F

postpartum <- postpartum %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date), 
         mother_alive_ever = sum(mother_alive, na.rm = T) > 0,
         mother_deceased_ever = sum(mother_deceased, na.rm = T) > 0,
         prob_bf_ever = sum(problems_breastfeeding, na.rm = T) > 0, 
         vit_a_ever = sum(vit_a_birth, na.rm = T) > 0)

pp_case <- postpartum %>% group_by(case_id) %>% 
  summarise(date_delivery = unique(date_delivery), 
            mother_alive_ever = unique(mother_alive_ever), 
            mother_deceased_ever = unique(mother_deceased_ever), 
            prob_bf_ever = unique(prob_bf_ever), 
            vit_a_ever = unique(vit_a_ever), 
            num_pp_forms = length(unique(row_num)))
c_wvug <- merge(c_wvug, pp_case, by = "case_id", all.x = T)

ba_clean <- filter(postpartum, !is.na(birth_attendant))
ba_clean <- ba_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
ba_clean <- filter(ba_clean, most_recent_postpartum == T)
ba_clean_case <- ba_clean %>% group_by(case_id) %>% 
  summarise(ba_new = unique(birth_attendant))
c_wvug <- merge(c_wvug, ba_clean_case, by = "case_id", all.x = T)

place_clean <- filter(postpartum, !is.na(place_birth))
place_clean <- place_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
place_clean <- filter(place_clean, most_recent_postpartum == T)
place_clean_case <- place_clean %>% group_by(case_id) %>% 
  summarise(place_new = unique(place_birth))
c_wvug <- merge(c_wvug, place_clean_case, by = "case_id", all.x = T)

woman_wvug <- filter(c_wvug, case_type == "mother")
write.csv(woman_wvug, file = "woman_wvug.csv", row.names = F)

#------------------------------------------------------------------------#
#OLDER CODE
#------------------------------------------------------------------------#

#Add impact variables to each form table for each case
#Impact 1: Registered by second trimester
#Get final EDD
registration$edd_valid <- registration$edd != "" & registration$edd != "---"
registration$edd_final <- NA
registration$edd_final[registration$edd_valid == T] <- registration$edd[registration$edd_valid == T] 
registration$edd_final <- as.Date(registration$edd_final)
#Get second trimester date
registration$second_tri <- registration$edd_final - 90 
#Get registration date
registration$reg_date <- as.Date(substr(registration$time_start, 1, 10))
#Set impact 1 values
registration$impact_1_met <- registration$reg_date <= registration$second_tri
registration$impact_1_met[is.na(registration$second_tri)] <- F
registration$impact_1_unmet <- registration$reg_date > registration$second_tri
registration$impact_1_unmet[is.na(registration$second_tri)] <- F
registration$impact_1_uk <- is.na(registration$second_tri)

#Impact values by case_id
registration <- filter(registration, case_id != "---")
case_impact <- registration %>% group_by(case_id) %>% 
  summarise(impact_1_met = unique(impact_1_met), 
            impact_1_unmet = unique(impact_1_unmet), 
            impact_1_uk = unique(impact_1_uk), 
            dob_woman = unique(dob_woman), 
            reg_date = unique(reg_date))
case_impact$age_woman <- round(as.numeric(case_impact$reg_date - case_impact$dob_woman)/360, digits = 2)
#summary(rowSums(case_impact[,2:4]))

#Merge impact table to case table
c_wvug <- merge(c_wvug, case_impact, by = "case_id", all.x = T)

#Merge user stats to case table
c_wvug <- merge(c_wvug, visit_stats, by = "user_pk", all.x = T)

#We expect that women who have been registered by second trimester might have more nvisits or n_followup
impact1 <- filter(c_wvug, impact_1_met == T)
summary(impact1$n_followup)

impact1 <- filter(c_wvug, impact_1_unmet == T)
summary(impact1$n_followup)

#Do mothers with impact_1 = met have users with higher lifetime total visits compared to mothers with impact_1 = unmet?
impact1 <- filter(c_wvug, impact_1_met == T | impact_1_unmet == T)
t.test(impact1$total_visits~impact1$impact_1_met)

#VISUALS

#Boxplot of visit stats
boxplot_visits <- data.frame(visit_stats$total_visits)
names(boxplot_visits) <- c("total_visits")
boxplot_visits$group <- 1

g <- ggplot(boxplot_visits, aes(group, total_visits)) + 
  geom_boxplot(aes(fill = group)) + 
  #stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "black") + 
  scale_colour_manual(values="coral") + 
  scale_y_continuous(limits=c(0, 100)) + 
  xlab("") +
  ylab("Total visits/FLW") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        legend.position="none")

#Impact 1 bar graph
dat <- data.frame(
  impact_status = factor(c("Met","Unmet", "Unknown"), levels=c("Met","Unmet", "Unknown")),
  percentage = c(17.25, 26.11, 56.64))

cbbPalette <- c("forestgreen", "brown1", "burlywood1")
ggplot(data=dat, aes(x=impact_status, y=percentage)) +
  geom_bar(stat="identity", fill = cbbPalette) + 
  scale_y_continuous(limits=c(0, 100)) + 
  xlab("Impact 1 status: Registered by second trimester") +
  ylab("Percentage (%) of cases") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        legend.position="none")

#Impact 1 status by FLW
user_activity <- registration %>% group_by(user_id) %>% 
  summarise(ncases = length(unique(case_id)), 
            ncases_met = sum(impact_1_met), 
            ncases_unmet = sum(impact_1_unmet), 
            ncases_uk = sum(impact_1_uk))
user_activity$per_met <- (user_activity$ncases_met/user_activity$ncases)*100
#Merge in total_visits
user_activity <- merge(user_activity, select(visit_stats, user_id, total_visits), by = "user_id", all.x = T)

ggplot(user_activity, aes(x=total_visits, y=per_met)) +
  geom_point(shape=1) +    # Use hollow circles
  scale_x_continuous(limits=c(0,150))