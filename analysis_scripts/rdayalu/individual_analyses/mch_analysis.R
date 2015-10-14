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

#Import form table and form annotations
#form_table <- tbl(db, "form")
#form_table <- collect(form_table)
#form_annotations <- read.csv(file = "form_annotations.csv", stringsAsFactors = F)

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

#Merge in domain table
visit <- merge(visit, select(domain, domain_pk, domain, is_test, subsector), by = "domain_pk", all.x = T)


