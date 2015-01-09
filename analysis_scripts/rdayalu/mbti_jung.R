#MBTI analysis for INC summit and beyond
#This code is to analyze the Jung/MBTI results from the Dimagi staff personality survey
#1/8/15

#Import survey results
mbti <- read.csv(file="mbti_results.csv")
#Import file with hire date for each employee
staff <- read.csv(file="staff_data.csv")
staff <- select(staff, email, hire_date)

#Merge survey results with hire date
mbti <- merge(mbti, staff, by.x = "email", by.y = "email", all.x = T)

#Convert hire_date to date variable
#Manually replace NA hire_dates with correct hire dates
mbti$hire_date <- as.Date(mbti$hire_date, "%m/%d/%Y")
mbti$hire_date[mbti$email == "Mcanty@dimagi.com"] <- "2014-10-01"
mbti$hire_date[mbti$email == "ashekar@dimagi.com"] <- "2013-04-03"
mbti$hire_date[mbti$email == "biyeun@dimagi.com"] <- "2011-10-17"
mbti$hire_date[mbti$email == "kumar@dimagi.com"] <- "2005-01-01"
mbti$hire_date[mbti$email == "ssnyder@dimagi.com"] <- "2012-03-26"

#Manually replace BUs for people who reported > 1 BU. Use INC as default
mbti$bu[mbti$bu == "DWA, INC"] <- "INC"
mbti$bu[mbti$bu == "DLAC, INC"] <- "INC"

