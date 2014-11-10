nusers <- monthly_table %.% 
  group_by(domain, month.index) %.% 
  summarise (nusers = length(unique(user_id)),
             nandroid = sum(summary_device_type == "Android", na.rm = T),
             nnokia = sum(summary_device_type == "Nokia", na.rm = T),
             ncloudcare = sum(summary_device_type == "Cloudcare", na.rm = T),
             nmulti = sum(summary_device_type == "Multi", na.rm = T),
             nnone = sum(summary_device_type == "None", na.rm = T),
             nother = sum(summary_device_type == "Other", na.rm = T),
             nmissing = sum(is.na(summary_device_type)))

nusers_aug <- filter(nusers, month.index == "Aug 2014")

#Merge domain facets from domain table into all_monthly table
to_merge <- select(domains_HQ, Project, X..Active.Mobile.Workers, X..Mobile.Workers, 
                   X..Mobile.Workers..Submitted.Form., X..Web.Users)

nusers_aug <- merge(nusers_aug, to_merge, by.x = "domain", 
                     by.y = "Project", all.x = T)

nusers_aug <- arrange(nusers_aug, desc(nusers))

nusers_aug$nusers_diff <- nusers_aug$nusers - nusers_aug$X..Active.Mobile.Workers

write.csv(nusers_aug, file = "nusers_dp_hq_v2.csv")