#Analysis for Amelia
#11/12/15
#She wants to flag a domain as j2me = TRUE if any user in any month submitted via j2me
#I will create a table of all domains with all summary device typye into

device_type <- monthly_table %>% group_by(domain) %>% 
  summarise(uses_android = sum(summary_device_type == "Android") > 0, 
            uses_nokia = sum(summary_device_type == "Nokia") > 0,
            uses_sms = sum(summary_device_type == "Sms") > 0,
            uses_cloudcare = sum(summary_device_type == "Cloudcare") > 0, 
            uses_multi = sum(summary_device_type == "Multi") > 0,
            uses_other = sum(summary_device_type == "None") > 0 | sum(summary_device_type == "Other") > 0, 
            uses_na = sum(is.na(summary_device_type)) > 0)

write.csv(device_type, file = "device_types_by_domain.csv", row.names = F)