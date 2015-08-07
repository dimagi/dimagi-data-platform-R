business_unit <- all_monthly %>% group_by(domain) %>% 
  summarise(business_unit = unique(new_business_unit))

wam_projects <- read.csv(file = "wam_projects.csv", header = F)
wam_projects$row_num <- 1:nrow(wam_projects)
all_projects <- read.csv(file = "all_projects.csv", header = F)
all_projects$row_num <- 1:nrow(all_projects)

wam_projects <- merge(wam_projects, business_unit, by.x = "V1", by.y = "domain", all.x = T)
wam_projects <- arrange(wam_projects, row_num)
all_projects <- merge(all_projects, business_unit, by.x = "V1", by.y = "domain", all.x = T)
all_projects <- arrange(all_projects, row_num)

write.csv(wam_projects, file = "wam_projects_bu.csv")
write.csv(all_projects, file = "all_projects_bu.csv")

nusers <- all_monthly %>% group_by(domain) %>% 
  summarise(nusers_april_2015 = length(unique(user_pk[calendar_month == "2015-04-01"])), 
            nusers_may_2015 = length(unique(user_pk[calendar_month == "2015-05-01"])), 
            nusers_april_2014 = length(unique(user_pk[calendar_month == "2014-04-01"])), 
            nusers_may_2014 = length(unique(user_pk[calendar_month == "2014-05-01"])), 
            nusers_april_2013 = length(unique(user_pk[calendar_month == "2013-04-01"])), 
            nusers_may_2013 = length(unique(user_pk[calendar_month == "2013-05-01"])))

nusers$delta_april_may_2015 <- nusers$nusers_may_2015 - nusers$nusers_april_2015
nusers$delta_april_may_2014 <- nusers$nusers_may_2014 - nusers$nusers_april_2014
nusers$delta_april_may_2013 <- nusers$nusers_may_2013 - nusers$nusers_april_2013
nusers <- arrange(nusers, desc(delta_april_may_2015))


#Merge domain facets to all_monthly
facets_to_merge <- select(domain, name, domain_id, country_final, sector_final, 
                          subsector_final, self_start, domain_has_amplifies_workers, 
                          domain_has_amplifies_project, new_business_unit, is_test, domain_start_date)
bu <- merge(monthly_table, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)
names(bu)[names(bu) == "domain_id"] = "domain_numeric"

bu$new_business_unit[is.na(bu$new_business_unit)] <- "None"

missing_bu <- filter(bu, new_business_unit == "None")
assign_bu <- unique(missing_bu$country_final)
table(bu$new_business_unit, useNA = "always")

bu$new_business_unit[bu$country_final == "{BR,HT,IN,KE,MW,PE,ZA,TH,UG,ZW}"] <- "DLAC"
bu$new_business_unit[bu$country_final == "{IN}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{US}"] <- "Inc"
bu$new_business_unit[bu$country_final == "{NE}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{MM}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{NG}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{SN}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{NP}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{HT}"] <- "DLAC"
bu$new_business_unit[bu$country_final == "{ID}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{TZ}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{ET}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{GD}"] <- "DLAC"
bu$new_business_unit[bu$country_final == "{ZA}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{ML,SN}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{LA}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{AF}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{GT}"] <- "DLAC"
bu$new_business_unit[bu$country_final == "{KE}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{BF}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{ZM}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{TH}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{SL}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{CO}"] <- "DLAC"
bu$new_business_unit[bu$country_final == "{BJ}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{BJ}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{GN}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{ET,UG}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{LS}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{GH}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{MW}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{MY}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{SS}"] <- "DSA"
bu$new_business_unit[bu$country_final == "Syria"] <- "Inc"
bu$new_business_unit[bu$country_final == "{SY}"] <- "Inc"
bu$new_business_unit[bu$country_final == "{IQ}"] <- "Inc"
bu$new_business_unit[bu$country_final == "{BD}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{JO}"] <- "Inc"
bu$new_business_unit[bu$country_final == "{MZ}"] <- "DMOZ"
bu$new_business_unit[bu$country_final == "{CN,GN}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{ZA,UG}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{VN}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{TR}"] <- "Inc"
bu$new_business_unit[bu$country_final == "{BZ,GT,HN}"] <- "DLAC"
bu$new_business_unit[bu$country_final == "{LR}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{BF,TD,NE}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{PK}"] <- "DSI"
bu$new_business_unit[bu$country_final == "{ML}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{SY,TR}"] <- "Inc"
bu$new_business_unit[bu$country_final == "{TD}"] <- "DWA"
bu$new_business_unit[bu$country_final == "{MG}"] <- "DSA"
bu$new_business_unit[bu$country_final == "{PH,TH}"] <- "DSI"
