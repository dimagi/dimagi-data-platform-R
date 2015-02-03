#MBTI analysis for INC summit and beyond
#This code is to analyze the Jung/MBTI results from the Dimagi staff personality survey
#1/8/15

#Import survey results
#Mike O'Donnell submitted results twice, so I deleted one submission of his
mbti <- read.csv(file="mbti_results.csv")
#Import file with hire date for each employee
staff <- read.csv(file="staff_data.csv")
staff <- select(staff, name, email)
staff2 <- read.csv(file="staff_data2.csv")
staff2 <- select(staff2, email, hire_date)

#Manually change wrong email addresses
#mbti$email[mbti$email == "Mcanty@dimagi.com"] <- "mcanty@dimagi.com"
#mbti$email[mbti$email == "kumar@dimagi.com"] <- "vkumar@dimagi.com"
#staff$email[staff$email == "ashekar@gmail.com"] <- "ashekar@dimagi.com"
#staff$email[staff$email == "bbuczyk@dimagi.com"] <- "biyeun@dimagi.com"
#staff$email[staff$email == "ssynder@dimagi.com"] <- "ssnyder@dimagi.com"

#Merge survey results with names and hire date
mbti <- merge(mbti, staff, by.x = "email", by.y = "email", all.x = T)
mbti <- merge(mbti, staff2, by.x = "email", by.y = "email", all.x = T)

#Convert hire_date to date variable
mbti$hire_date <- as.Date(mbti$hire_date, "%m/%d/%Y")

#Manually replace BUs for people who reported > 1 BU. Use INC as default
mbti$bu[mbti$bu == "DWA, INC"] <- "INC"
mbti$bu[mbti$bu == "DLAC, INC"] <- "INC"

#Code MBTI results to NBI
mbti$nbi[mbti$mbti %in% c("INTJ", "ESTP", "ESFP", "INFJ")] <- "L1"
mbti$nbi[mbti$mbti %in% c("ISTJ", "ISFJ", "ESTJ", "ENTJ")] <- "L2"
mbti$nbi[mbti$mbti %in% c("INTP", "ENTP", "ENFP", "ISTP")] <- "R1"
mbti$nbi[mbti$mbti %in% c("ISFP", "INFP", "ENFJ", "ESFJ")] <- "R2"

#Extract each MBTI letter
mbti$one <- as.factor(substr(mbti$mbti, 1, 1))
mbti$two <- as.factor(substr(mbti$mbti, 2, 2))
mbti$three <- as.factor(substr(mbti$mbti, 3, 3))
mbti$four <- as.factor(substr(mbti$mbti, 4, 4))

#Extract hire year and calculate total # hires each year
mbti$hire_year <- year(mbti$hire_date)
new_hires <- mbti %>% group_by(hire_year) %>% summarise(nhires = length(hire_year),
                                                        nhires_L1 = sum(nbi=="L1"),
                                                        nhires_L2 = sum(nbi=="L2"),
                                                        nhires_R1 = sum(nbi=="R1"),
                                                        nhires_R2 = sum(nbi=="R2"))
new_hires$per_L1 <- (new_hires$nhires_L1/new_hires$nhires)*100
new_hires$per_L2 <- (new_hires$nhires_L2/new_hires$nhires)*100
new_hires$per_R1 <- (new_hires$nhires_R1/new_hires$nhires)*100
new_hires$per_R2 <- (new_hires$nhires_R2/new_hires$nhires)*100
new_hires <- new_hires[!is.na(new_hires$hire_year),]
new_hires <- new_hires[new_hires$hire_year != 2015,]
new_hires <- new_hires[new_hires$hire_year >= 2010,]
new_hires_graph <- data.frame(rep(new_hires$hire_year,4), 
                              c(new_hires$per_L1, new_hires$per_L2, 
                                new_hires$per_R1, new_hires$per_R2),
                              c(rep("L1",5), rep("L2",5), rep("R1",5), rep("R2",5))) 
names(new_hires_graph) <- c("hire_year", "percentage_nbi", "NBI")

colours <- c("coral", "darkorchid1", "cornflowerblue", "forestgreen")
g <- ggplot(new_hires_graph, aes(x=hire_year, y=percentage_nbi, colour = NBI)) + 
  geom_line(size = 1.5) + 
  xlab("Year of hire") +
  ylab("Percentage (%) NBI of new hires") +
  theme(axis.title.x=element_text(size=14), axis.text.x=element_text(size=14, colour = "black")) + 
  theme(axis.title.y=element_text(size=14), axis.text.y=element_text(size=14, colour = "black"))


#Table of counts for MBTI types
mbti_counts <- mbti %>% group_by(mbti) %>% summarise(count=length(mbti))
mbti_counts <- mbti %>% group_by(one) %>% summarise(count=length(one))
mbti_counts <- mbti %>% group_by(two) %>% summarise(count=length(two))
mbti_counts <- mbti %>% group_by(three) %>% summarise(count=length(three))
mbti_counts <- mbti %>% group_by(four) %>% summarise(count=length(four))

#MBTI results to first_half and second_half for balloon plot
mbti_counts$first_half_mbti <- as.factor(substr(mbti_counts$mbti, 1, 2))
mbti_counts$second_half_mbti <- as.factor(substr(mbti_counts$mbti, 3, 4))

#Balloon plot
g <- ggplot(mbti_counts, aes(x=second_half_mbti, y=first_half_mbti)) + 
  geom_point(aes(size=count), shape=21, colour="black", fill="cornsilk") + 
  scale_size_area(max_size=20, guide=FALSE) + 
  geom_text(aes(y=as.numeric(mbti_counts$first_half_mbti)-sqrt(count)/12, label=count), 
            vjust=1, colour="grey60", size=4) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

#Bar charts
g <- ggplot(mbti_counts, aes(x=two, y=count, fill=two)) +
  geom_bar(stat = "identity", width = 0.5) + 
  theme(axis.title.x=element_blank())

#Table of counts for NBI types
nbi_counts <- mbti %>% group_by(nbi) %>% summarise(count=length(nbi))

#NBI results to first_half and second_half for balloon plot
nbi_counts$first_half_nbi <- as.factor(substr(nbi_counts$nbi, 1, 1))
nbi_counts$second_half_nbi <- as.factor(substr(nbi_counts$nbi, 2, 2))

#Balloon plot
nbi_counts$second_half_nbi = with(nbi_counts, factor(second_half_nbi, levels = rev(levels(second_half_nbi))))
nbi_counts$nbi <- as.factor(nbi_counts$nbi)
colours <- c("coral", "darkorchid1", "cornflowerblue", "forestgreen")
g <- ggplot(nbi_counts, aes(x=first_half_nbi, y=second_half_nbi)) + 
  geom_point(aes(size=count), shape=21, colour="black", fill=colours) + 
  scale_size_area(max_size=20, guide=FALSE) + 
  geom_text(aes(y=as.numeric(nbi_counts$second_half_nbi)-sqrt(count)/20, label=count), 
            vjust=1, colour="grey60", size=4) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none")



#BY TEAM/BU
#Table of counts for NBI types
team <- filter(mbti, team == "Business Development")
bu <- filter(mbti, bu == "DSI")
nbi_counts <- bu %>% group_by(nbi) %>% summarise(count=length(nbi))

#NBI results to first_half and second_half for balloon plot
nbi_counts$first_half_nbi <- as.factor(substr(nbi_counts$nbi, 1, 1))
nbi_counts$second_half_nbi <- as.factor(substr(nbi_counts$nbi, 2, 2))

#Balloon plot
nbi_counts$second_half_nbi = with(nbi_counts, factor(second_half_nbi, levels = rev(levels(second_half_nbi))))
#nbi_counts$nbi <- as.factor(nbi_counts$nbi)
colours <- c("coral", "darkorchid1", "cornflowerblue", "forestgreen")
#colours <- c("coral", "darkorchid1", "forestgreen")
#colours <- c("darkorchid1", "cornflowerblue", "forestgreen")
g <- ggplot(nbi_counts, aes(x=first_half_nbi, y=second_half_nbi)) + 
  geom_point(aes(size=count), shape=21, colour="black", fill = colours) + 
  scale_size_area(max_size=20, guide=FALSE) + 
  geom_text(aes(y=as.numeric(nbi_counts$second_half_nbi)-sqrt(count)/10, label=count), 
            vjust=1, colour="grey60", size=4) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none")



#INC Summit workshop numbers
#Variable "name" is the list of 59 people who are attending the INC summit
#We have 52 (identified) people who took the survey. 
#3 people did not given their email addresses, but they are with INC, so I am 
#including them in the workshop total, assuming that they will attend
inc <- filter(mbti, !(name == "") | is.na(name))
table(inc$nbi, useNA = "always")
test <- filter(inc, nbi == "L2")
test <- filter(inc, team == "Ops")





