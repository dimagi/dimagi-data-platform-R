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
