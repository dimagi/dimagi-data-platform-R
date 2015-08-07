#This code is to create the BU table for the WAM ingredients folder

#Upload project space csv files from the DB folder to workspace
#May need to zip the files first if importing is a problem

#Next import as objects to global environment
domain <- read.csv(file = "project_spaces_hq.csv")

#Rename columns for objects as needed
names(domain)[names(domain) == "Project"] = "domain"
names(domain)[names(domain) == "Deployment.Country"] = "countries"
names(domain)[names(domain) == "First.Form.Submission"] = "cp_first_form"
names(domain)[names(domain) == "Sector"] = "sector_final"
names(domain)[names(domain) == "Sub.Sector"] = "subsector_final"
names(domain)[names(domain) == "Self.Starter."] = "self_start"
names(domain)[names(domain) == "Business.Unit"] = "new_business_unit"
names(domain)[names(domain) == "Test.Project."] = "is_test"

#Format domain table as needed
domain <- select(domain, domain, countries, cp_first_form, sector_final, subsector_final, self_start, new_business_unit, is_test)
#Remove last three rows which contain only the summary for the project spaces
domain <- head(domain,-3)
#Domain names
domain$domain <- as.character(domain$domain)
#Countries
domain$countries <- as.character(domain$countries)
domain$countries[domain$countries == "No countries"] <- NA
domain$countries <- gsub("u'", "", domain$countries)
domain$countries <- gsub("'", "", domain$countries)
domain$countries <- gsub('u"', "", domain$countries)
domain$countries <- gsub('"', "", domain$countries)
domain$countries <- gsub('[[]', "", domain$countries)
domain$countries <- gsub('[]]', "", domain$countries)

#Convert countries variable to a corresponsing list, separating out the comma
#separated names
country_list <- as.list(strsplit(domain$countries, ", "))

#Create vector of unique country names for BU file to be added to WAM ingredients folder
countries_for_bu <- data.frame(unique(unlist(country_list, use.names = FALSE)))
names(countries_for_bu) <- "country"
countries_for_bu$business_unit <- NA
countries_for_bu <- arrange(countries_for_bu, country)
#Remove country = NA and country = UNITED REPUBLIC OF. This was TANZANIA, UNITED REPUBLIC OF
#which is getting parsed out as a separate country because of the comma. We don't need it since 
#Tanzania will already be counted
countries_for_bu <- filter(countries_for_bu, country != "UNITED REPUBLIC OF")

#Group countries using BU definitions modified from Yedi's DP list here:
#https://github.com/dimagi/dimagi-data-platform-R/blob/bf6e55f639abfd84a4310bc0bfa6578c8c3aa01c/function_libraries/report_utils.R#L169-L192
inc_list <- toupper(c("Canada", "United Kingdom", "United States", "United States of America", "Wales", "France", "Spain", "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND", "Egypt", "SYRIAN ARAB REPUBLIC", "Turkey", "Iraq", "Jordan"))
dsi_list <- toupper(c("Afghanistan", "Bangladesh", "Burma", "India", "Indonesia", "Laos", "Myanmar", "Nepal", "Pakistan", "Philippines", "Thailand", "Sri Lanka", "LAO PEOPLES DEMOCRATIC REPUBLIC", "Malaysia", "TIMOR-LESTE", "VIET NAM"))
dsa_list <- toupper(c("Angola", "Burundi", "Lesotho", "Madagascar", "Rwanda", "South Africa", "South Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe", "ethiopia", "kenya", "malawi", "Namibia"))
dwa_list <- toupper(c("Benin", "Burkina Faso", "Ghana", "Guinea", "Mali", "Niger and Burkina Faso", "Nigeria", "Senegal", "Sierra Leone", "Togo", "Cameroon", "CHAD", "Gambia", "Liberia", "Niger"))
dlac_list <- toupper(c("Brazil", "Belize", "Colombia", "Dominican Republic", "Grenada", "Guatemala", "Haiti", "Mexico", "Nicaragua", "Honduras", "Peru")) 
dmoz_list <- toupper(c("Mozambique"))

#Add BU to countries_for_bu
countries_for_bu$business_unit[countries_for_bu$country %in% inc_list] <- "INC"
countries_for_bu$business_unit[countries_for_bu$country %in% dsi_list] <- "DSI"
countries_for_bu$business_unit[countries_for_bu$country %in% dsa_list] <- "DSA"
countries_for_bu$business_unit[countries_for_bu$country %in% dwa_list] <- "DWA"
countries_for_bu$business_unit[countries_for_bu$country %in% dlac_list] <- "DLAC"
countries_for_bu$business_unit[countries_for_bu$country %in% dmoz_list] <- "DMOZ"

write.csv(countries_for_bu, file = "country_bu_mapping.csv", row.names = F)



