table(woman_wvug$place_new, useNA = "always")
table(woman_jhark$place_new, useNA = "always")


#Trimester of registration
summary(woman_jhark$dd_new)
summary(woman_jhark$date_opened_final)
woman_jhark$months_to_delivery <- as.numeric(difftime(woman_jhark$dd_new, woman_jhark$date_opened_final, units = "days"))/30
woman_jhark$reg_by_second_tri <- woman_jhark$months_to_delivery > 3

#Facility delivery vs. home delivery
woman_crs$facility_delivery <- NA
woman_crs$facility_delivery[woman_crs$place_new == "in_hospital"] <- T 
woman_crs$facility_delivery[woman_crs$place_new == "at_home" | woman_crs$place_new == "in_transit"] <- F

#Proportion of facility delivery among women registered by the second trimester vs. later is not very different (~80%)
#Mean/median of f/u visits for women with facility vs. home delivery is not very different (~4 f/u visits)

#Facility delivery and previous pregnancy
summary(woman_jhark$facility_delivery[woman_jhark$previously_preg == T])
summary(woman_jhark$facility_delivery[woman_jhark$previously_preg == F])
#Chi square test
tbl <- table(woman_jhark$facility_delivery, woman_jhark$previously_preg)
chisq.test(tbl)
#Not statistically significant

#Facility delivery and TT1
summary(woman_jhark$facility_delivery[woman_jhark$tet_1_ever == T])
summary(woman_jhark$facility_delivery[woman_jhark$tet_1_ever == F])
#tet_1 == T has very small numbers - no delivery location information for those cases

#HIV testing and CHW experience level
j_preg <- filter(woman_jhark, is_pregnant == T)
summary(j_preg$hiv_tested)
summary(j_preg$active_months_rolling[j_preg$hiv_tested == T])
summary(j_preg$active_months_rolling[j_preg$hiv_tested == F])
summary(j_preg$active_months_rolling[is.na(j_preg$hiv_tested)])

#Time on CC and registration timing
summary(j_preg$time_on_cc[j_preg$reg_by_second_tri == T])
summary(j_preg$time_on_cc[j_preg$reg_by_second_tri == F])

#Percent of daytime visits
woman_jhark$per_daytime <- (woman_jhark$nvisits_daytime/woman_jhark$nvisits_mobile)*100



