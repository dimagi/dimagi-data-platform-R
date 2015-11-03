# 5/22/15
# This script is for an analysis of MCH domain data, looking at CommCare activity and how it relates
# to impact data. 
# Outline of analysis here:
# https://docs.google.com/document/d/1GJA85jrlNd9VYbcHP00zn9G4e6jH-e-IBGBaT_ijLWk/edit

# Get db connection
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))
db <- get_db_connection(system_conf)

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

#Get domain table
domain <- get_domain_table(db)
names(domain)[names(domain) == "id"] = "domain_pk"
names(domain)[names(domain) == "name"] = "domain"
domain$subsector <- lapply(domain$subsector, as.character)
domains_mch_subsector <- seq_along(domain$subsector)[sapply(domain$subsector, FUN=function(X) "Maternal, Newborn, & Child Health" %in% X)]
domain$mch_subsector <- F
domain$mch_subsector[domains_mch_subsector] <- T

# Import visit table
visit <- tbl(db, "visit")
visit <- collect(visit)

# Format visit table
names(visit)[names(visit) == "id"] <- "visit_id"
names(visit)[names(visit) == "user_id"] <- "user_pk"
names(visit)[names(visit) == "domain_id"] <- "domain_pk"

#Merge in domain table and keep only MCH domains with is_test = F
visit <- merge(visit, select(domain, domain_pk, domain, is_test, mch_subsector), by = "domain_pk", all.x = T)
visit <- filter(visit, mch_subsector == T)
visit <- filter(visit, is_test == "false")

#Import user table and user_type table
#We are going to keep only mobile users
users <- tbl(db, "users") 
users <- collect(users)
names(users)[names(users) == "id"] = "user_pk"
users <- select(users, user_pk, user_id, username, email)

user_type <- get_user_type_table(db)
user_type <- collect(user_type)
user_type <- select(user_type, user_pk, user_type)

users <- merge(users, user_type, by = "user_pk", all.x = T)
visit <- merge(visit, users, by = "user_pk", all.x = T)

#Create table of nvisits, nusers by domain
domain_size <- visit %>% group_by(domain) %>% 
  summarise(nusers = length(unique(user_pk)), 
            nvisits = length(unique(visit_id)))
domain_size <- arrange(domain_size, desc(nvisits))

#Import form table and form annotations
form <- tbl(db, "form")
form <- collect(form)
form_annotations <- read.csv(file = "form_annotations.csv", stringsAsFactors = F)

#Import monthly table
monthly_table <- tbl(db, "aggregate_monthly_interactions")
monthly_table <- collect(monthly_table)

#------------------------------------------------------------------------#
#For wvug:
#Try the following 3 impact variables:
#Registered by second trimester
#Institutional delivery
#Takes IFA daily
#------------------------------------------------------------------------#

monthly_wvug <- filter(monthly_table, domain == "wvug")

visit_stats <- monthly_wvug %>% group_by(user_id) %>% 
  summarise(total_visits = sum(nvisits, na.rm = T), 
            med_visits = median(nvisits, na.rm = T), 
            total_months = length(unique(month.index)))
visit_stats <- arrange(visit_stats, desc(total_visits))

#Boxplot of visit stats
boxplot_visits <- data.frame(visit_stats$total_visits)
names(boxplot_visits) <- c("total_visits")
boxplot_visits$group <- 1

g <- ggplot(boxplot_visits, aes(group, total_visits)) + 
  geom_boxplot(aes(fill = group)) + 
  #stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "black") + 
  scale_colour_manual(values="coral") + 
  scale_y_continuous(limits=c(0, 100)) + 
  xlab("") +
  ylab("Total visits/FLW") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        legend.position="none")

#Import registration and postpartum forms for woman
registration <- read.csv(file = "wvug_registration.csv", stringsAsFactors = F)
prenatal <- read.csv(file = "wvug_prenatal.csv", stringsAsFactors = F)
postpartum <- read.csv(file = "wvug_postpartum.csv", stringsAsFactors = F)

#Format wvug form tables
#Registration
names(registration)[names(registration) == "form.case..user_id"] <- "user_id"
names(registration)[names(registration) == "form.case..case_id"] <- "case_id"
names(registration)[names(registration) == "form.meta.timeStart"] <- "time_start"
names(registration)[names(registration) == "form.pregnancy_questions.edd"] <- "edd"
names(registration)[names(registration) == "form.case.create.case_type"] <- "case_type"
#Prenatal
names(prenatal)[names(prenatal) == "form.case..user_id"] <- "user_id"
names(prenatal)[names(prenatal) == "form.case..case_id"] <- "case_id"
names(prenatal)[names(prenatal) == "form.meta.timeStart"] <- "time_start"
names(prenatal)[names(prenatal) == "form.visit_questions.iron_folic"] <- "iron"
#Postpartum
names(postpartum)[names(postpartum) == "form.case..user_id"] <- "user_id"
names(postpartum)[names(postpartum) == "form.case..case_id"] <- "case_id"
names(postpartum)[names(postpartum) == "form.meta.timeStart"] <- "time_start"
names(postpartum)[names(postpartum) == "form.visit_questions.place_of_birth"] <- "place_birth"

#Add impact variables to each form table for each case

#Impact 1: Registered by second trimester
#Get final EDD
registration$edd_valid <- registration$edd != "" & registration$edd != "---"
registration$edd_final <- NA
registration$edd_final[registration$edd_valid == T] <- registration$edd[registration$edd_valid == T] 
registration$edd_final <- as.Date(registration$edd_final)
#Get second trimester date
registration$second_tri <- registration$edd_final - 90 
#Get registration date
registration$reg_date <- as.Date(substr(registration$time_start, 1, 10))
#Set impact 1 values
registration$impact_1_met <- registration$reg_date <= registration$second_tri
registration$impact_1_met[is.na(registration$second_tri)] <- F
registration$impact_1_unmet <- registration$reg_date > registration$second_tri
registration$impact_1_unmet[is.na(registration$second_tri)] <- F
registration$impact_1_uk <- is.na(registration$second_tri)

#Impact 1 values by case_id
case_impact <- registration %>% group_by(case_id) %>% 
  summarise(nFLW = length(unique(user_id)),
            impact_1_met = unique(impact_1_met), 
            impact_1_unmet = unique(impact_1_unmet), 
            impact_1_uk = unique(impact_1_uk))
summary(rowSums(case_impact[,2:4]))

#Impact 1 bar graph
dat <- data.frame(
  impact_status = factor(c("Met","Unmet", "Unknown"), levels=c("Met","Unmet", "Unknown")),
  percentage = c(17.25, 26.11, 56.64))

cbbPalette <- c("forestgreen", "brown1", "burlywood1")
ggplot(data=dat, aes(x=impact_status, y=percentage)) +
  geom_bar(stat="identity", fill = cbbPalette) + 
  scale_y_continuous(limits=c(0, 100)) + 
  xlab("Impact 1 status: Registered by second trimester") +
  ylab("Percentage (%) of cases") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        legend.position="none")

#Impact 1 status by FLW
user_activity <- registration %>% group_by(user_id) %>% 
  summarise(ncases = length(unique(case_id)), 
            ncases_met = sum(impact_1_met), 
            ncases_unmet = sum(impact_1_unmet), 
            ncases_uk = sum(impact_1_uk))
user_activity$per_met <- (user_activity$ncases_met/user_activity$ncases)*100
#Merge in total_visits
user_activity <- merge(user_activity, select(visit_stats, user_id, total_visits), by = "user_id", all.x = T)

ggplot(user_activity, aes(x=total_visits, y=per_met)) +
  geom_point(shape=1) +    # Use hollow circles
  scale_x_continuous(limits=c(0,150))

#Impact 2: Registered by second trimester
#Get final EDD


