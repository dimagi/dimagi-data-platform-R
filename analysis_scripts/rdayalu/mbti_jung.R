#MBTI analysis for INC summit and beyond
#This code is to analyze the Jung/MBTI results from the Dimagi staff personality survey
#1/8/15

#Import survey results
mbti <- read.csv(file="mbti_results.csv")
#Import file with hire date for each employee
staff <- read.csv(file="staff_data.csv")
staff <- select(staff, name, email, hire_date)

#Manually change wrong email addresses
mbti$email[mbti$email == "Mcanty@dimagi.com"] <- "mcanty@dimagi.com"
mbti$email[mbti$email == "kumar@dimagi.com"] <- "vkumar@dimagi.com"

staff$email[staff$email == "ashekar@gmail.com"] <- "ashekar@dimagi.com"
staff$email[staff$email == "bbuczyk@dimagi.com"] <- "biyeun@dimagi.com"
staff$email[staff$email == "ssynder@dimagi.com"] <- "ssnyder@dimagi.com"

#Merge survey results with hire date
mbti <- merge(mbti, staff, by.x = "email", by.y = "email", all.x = T)

#Convert hire_date to date variable
mbti$hire_date <- as.Date(mbti$hire_date, "%m/%d/%Y")
mbti$hire_date[mbti$email == "Mcanty@dimagi.com"] <- "2014-10-01"
mbti$hire_date[mbti$email == "ashekar@dimagi.com"] <- "2013-04-03"
mbti$hire_date[mbti$email == "biyeun@dimagi.com"] <- "2011-10-17"
mbti$hire_date[mbti$email == "kumar@dimagi.com"] <- "2005-01-01"
mbti$hire_date[mbti$email == "ssnyder@dimagi.com"] <- "2012-03-26"

#Manually replace BUs for people who reported > 1 BU. Use INC as default
mbti$bu[mbti$bu == "DWA, INC"] <- "INC"
mbti$bu[mbti$bu == "DLAC, INC"] <- "INC"

#Code MBTI results to NBI
mbti$nbi[mbti$mbti %in% c("INTJ", "ESTP", "ESFP", "INFJ")] <- "L1"
mbti$nbi[mbti$mbti %in% c("ISTJ", "ISFJ", "ESTJ", "ENTJ")] <- "L2"
mbti$nbi[mbti$mbti %in% c("INTP", "ENTP", "ENFP", "ISTP")] <- "R1"
mbti$nbi[mbti$mbti %in% c("ISFP", "INFP", "ENFJ", "ESFJ")] <- "R2"

#INC Summit workshop numbers
#Variable "name" is the list of 56 people who are attending the INC summit
#As of 1/11/15, we have 50 people who took the survey
inc <- filter(mbti, !(name == ""))
inc <- filter(test, !is.na(name))
table(inc$nbi, useNA = "always")
test <- filter(inc, nbi == "L2")
test <- filter(inc, team == "Ops")



