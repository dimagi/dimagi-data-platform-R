#------------------------------------------------------------------------#
#Create general and specific case tables as described here:
#https://docs.google.com/document/d/1GJA85jrlNd9VYbcHP00zn9G4e6jH-e-IBGBaT_ijLWk/edit#bookmark=id.cqyhlmfcvdwy
#------------------------------------------------------------------------#

crs <- filter(cases, domain == "crs-remind")

#Read in form data
registration <- read.csv(file = "registration.csv", stringsAsFactors = F)
postpartum <- read.csv(file = "delivery.csv", stringsAsFactors = F)

#Format wvug form tables
#Registration
names(registration)[names(registration) == "form.case..user_id"] <- "user_id"
names(registration)[names(registration) == "form.case..case_id"] <- "case_id"
names(registration)[names(registration) == "form.meta.timeStart"] <- "time_start"
names(registration)[names(registration) == "form.edd"] <- "edd"
names(registration)[names(registration) == "form.case.create.case_type"] <- "case_type"
names(registration)[names(registration) == "form.age"] <- "age_woman_response"
#names(registration)[names(registration) == "form.previous_pregnancies"] <- "previously_preg"
names(registration)[names(registration) == "form.previous_pregnancies"] <- "num_prev_preg"
names(registration)[names(registration) == "form.living_children"] <- "num_children"
#names(registration)[names(registration) == "form.is_pregnant"] <- "is_pregnant"
#names(registration)[names(registration) == "form.not_pregnant_questions.using_fp"] <- "using_fp"
registration <- select(registration, user_id, case_id, time_start, edd, case_type, age_woman_response, 
                       num_prev_preg, num_children)

#Prenatal
#names(prenatal)[names(prenatal) == "form.case..user_id"] <- "user_id"
#names(prenatal)[names(prenatal) == "form.case..case_id"] <- "case_id"
#names(prenatal)[names(prenatal) == "form.meta.timeStart"] <- "time_start"
#names(prenatal)[names(prenatal) == "form.visit_questions.iron_folic"] <- "iron"
#names(prenatal)[names(prenatal) == "form.visit_questions.tet_1"] <- "tet_1"
#names(prenatal)[names(prenatal) == "form.visit_questions.tet_2"] <- "tet_2"
#names(prenatal)[names(prenatal) == "form.visit_questions.hiv_tested"] <- "hiv_tested"
#prenatal <- select(prenatal, user_id, case_id, time_start, iron, tet_1, tet_2, hiv_tested)

#Postpartum
names(postpartum)[names(postpartum) == "form.case..user_id"] <- "user_id"
names(postpartum)[names(postpartum) == "form.case..case_id"] <- "case_id"
names(postpartum)[names(postpartum) == "form.meta.timeStart"] <- "time_start"
names(postpartum)[names(postpartum) == "form.date_birth"] <- "date_delivery"
names(postpartum)[names(postpartum) == "form.maternal_outcome"] <- "mother_alive"
names(postpartum)[names(postpartum) == "form.birth_delivery_conducted_by"] <- "birth_attendant"
names(postpartum)[names(postpartum) == "form.place_birth"] <- "place_birth"
names(postpartum)[names(postpartum) == "form.pre_term"] <- "pre_term"
names(postpartum)[names(postpartum) == "form.birth_weight"] <- "lbw"
names(postpartum)[names(postpartum) == "form.type_delivery"] <- "delivery_type"
postpartum <- select(postpartum, user_id, case_id, time_start, date_delivery, mother_alive, birth_attendant, place_birth, 
                     pre_term, lbw, delivery_type)

#Prepare registration table for case table
#This should be a one-to-one mapping
nrow(registration)
#27173 rows
length(unique(registration$case_id))

registration$reg_date <- as.Date(substr(registration$time_start, 1, 10))

registration$previously_preg <- NA
registration$previously_preg[registration$num_prev_preg == 0] <- F
registration$previously_preg[registration$num_prev_preg > 0] <- T

registration$edd[registration$edd == "---"] <- NA
registration$edd[registration$edd == ""] <- NA
registration$edd <- as.Date(registration$edd)

registration <- select(registration, -c(user_id, time_start, case_type))
registration$has_registration_form <- T
crs <- merge(crs, registration, by = "case_id", all.x = T)
crs$edd_before_dataset_end <- crs$edd <= crs$dataset_end_date   
crs$edd_before_domain_end <- crs$edd <= crs$domain_last_form_date

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
postpartum$mother_alive[postpartum$mother_alive == ""] <- NA
postpartum$mother_alive[postpartum$mother_alive == "maternal_loss_to_followup"] <- NA
postpartum$mother_alive[postpartum$mother_alive == "maternal_alive"] <- T
postpartum$mother_alive[postpartum$mother_alive == "maternal_dead"] <- F
postpartum$mother_alive <- as.logical(postpartum$mother_alive)

postpartum$birth_attendant[postpartum$birth_attendant == "---"] <- NA
postpartum$birth_attendant[postpartum$birth_attendant == ""] <- NA

postpartum$place_birth[postpartum$place_birth == "---"] <- NA
postpartum$place_birth[postpartum$place_birth == ""] <- NA

postpartum$mother_deceased <- NA
postpartum$mother_deceased[postpartum$mother_alive == F] <- T
postpartum$mother_deceased[postpartum$mother_alive == T] <- F

postpartum$lbw[postpartum$lbw == "---"] <- NA
postpartum$lbw[postpartum$lbw == ""] <- NA

postpartum$pre_term[postpartum$pre_term == "---"] <- NA
postpartum$pre_term[postpartum$pre_term == ""] <- NA

postpartum$delivery_type[postpartum$delivery_type == "---"] <- NA
postpartum$delivery_type[postpartum$delivery_type == ""] <- NA

postpartum <- postpartum %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date), 
         mother_alive_ever = sum(mother_alive, na.rm = T) > 0,
         mother_deceased_ever = sum(mother_deceased, na.rm = T) > 0)
postpartum$most_recent_delivery_date <- NA
postpartum$most_recent_delivery_date[postpartum$most_recent_postpartum == T] <- 
  postpartum$date_delivery[postpartum$most_recent_postpartum == T]
postpartum$most_recent_delivery_date <- as.Date(postpartum$most_recent_delivery_date)

pp_case <- postpartum %>% group_by(case_id) %>% 
  summarise(date_delivery = max(most_recent_delivery_date, na.rm = T), 
            mother_alive_ever = unique(mother_alive_ever), 
            mother_deceased_ever = unique(mother_deceased_ever),
            num_pp_forms = length(unique(row_num)))

crs <- merge(crs, pp_case, by = "case_id", all.x = T)

ba_clean <- filter(postpartum, !is.na(birth_attendant))
ba_clean <- ba_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
ba_clean <- filter(ba_clean, most_recent_postpartum == T)
#Remove dup pp date - this is sloppy... need to keep latest form in future.
ba_clean$dup_pp_date <- duplicated(ba_clean$case_id)
ba_clean <- filter(ba_clean, dup_pp_date == F)
ba_clean_case <- ba_clean %>% group_by(case_id) %>% 
  summarise(ba_new = unique(birth_attendant))
crs <- merge(crs, ba_clean_case, by = "case_id", all.x = T)

place_clean <- filter(postpartum, !is.na(place_birth))
place_clean <- place_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
place_clean <- filter(place_clean, most_recent_postpartum == T)
#Remove dup pp date - this is sloppy... need to keep latest form in future.
place_clean$dup_pp_date <- duplicated(place_clean$case_id)
place_clean <- filter(place_clean, dup_pp_date == F)
place_clean_case <- place_clean %>% group_by(case_id) %>% 
  summarise(place_new = unique(place_birth))
crs <- merge(crs, place_clean_case, by = "case_id", all.x = T)

alive_clean <- filter(postpartum, !is.na(mother_alive))
alive_clean <- alive_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
alive_clean <- filter(alive_clean, most_recent_postpartum == T)
#Remove dup pp date - this is sloppy... need to keep latest form in future.
alive_clean$dup_pp_date <- duplicated(alive_clean$case_id)
alive_clean <- filter(alive_clean, dup_pp_date == F)
alive_clean_case <- alive_clean %>% group_by(case_id) %>% 
  summarise(alive_new = unique(mother_alive))
crs <- merge(crs, alive_clean_case, by = "case_id", all.x = T)

lbw_clean <- filter(postpartum, !is.na(lbw))
lbw_clean <- lbw_clean %>% group_by(case_id) %>% 
  mutate(most_recent_postpartum = postpartum_date == max(postpartum_date))
lbw_clean <- filter(lbw_clean, most_recent_postpartum == T)
#Remove dup pp date - this is sloppy... need to keep latest form in future.
lbw_clean$dup_pp_date <- duplicated(lbw_clean$case_id)
lbw_clean <- filter(lbw_clean, dup_pp_date == F)
lbw_clean_case <- lbw_clean %>% group_by(case_id) %>% 
  summarise(lbw_new = unique(lbw))
crs <- merge(crs, lbw_clean_case, by = "case_id", all.x = T)

woman_crs <- filter(crs, case_type == "pregnant_mother")
write.csv(mother_wvug, file = "mother_wvug.csv", row.names = F)

#Trimester of registration
summary(woman_crs$date_delivery)
summary(woman_crs$reg_date)
woman_crs$months_to_delivery <- as.numeric(difftime(woman_crs$date_delivery, woman_crs$reg_date, units = "days"))/30
woman_crs$reg_by_second_tri <- woman_crs$months_to_delivery > 3

test <- filter(woman_crs, !is.na(reg_by_second_tri))
test1 <- filter(woman_crs, reg_by_second_tri == T)
test2 <- filter(woman_crs, reg_by_second_tri == F)
t.test(test$nvisits_mobile~test$reg_by_second_tri)

#Bar graph of MMR by registration timing
dat <- data.frame(
  reg = factor(c("By 2nd trimester","After 2nd trimester"), levels=c("By 2nd trimester","After 2nd trimester")),
  mmr = c(84, 579)
)

g <- ggplot(data=dat, aes(x=reg, y=mmr, fill=reg)) + 
geom_bar(colour="black", fill="coral", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("Registered by which trimester?") + 
  ylab("MMR (# maternal deaths/100,000 deliveries)") +
  ggtitle("Maternal mortality rate (MMR) by trimester at registration") +
  scale_y_continuous(limits=c(0,650)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        legend.title = element_text(size=12), 
        legend.text = element_text(size = 12))
