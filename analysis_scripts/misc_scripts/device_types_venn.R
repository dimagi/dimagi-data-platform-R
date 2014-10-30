library(reshape2)

source("http://www.bioconductor.org/biocLite.R")
biocLite("limma")
library(limma)
library(dplyr)

setwd("/datadrive/dimagi-data-platform/output/misc")
devices <- read.csv("user_device_type.csv", header=T, sep=',')
names(devices) <- c('user_id','device')
devices$val <- TRUE
head(devices)

devices_wide <-dcast(devices, user_id ~ device, value.var="val")
devices_wide[is.na(devices_wide)] <- F
head(devices_wide)
colSums(devices_wide)

v <- vennCounts(devices_wide[,2:5])
vennDiagram(v)

# date range files
devices_90days <- read.csv("user_device_type_may15-14tojun30-14.csv", header=T, sep=',')
names(devices_90days) <- c('user_id','device')
devices_90days$val <- TRUE
devices_wide_90days <-dcast(devices_90days, user_id ~ device, value.var="val")
devices_wide_90days[is.na(devices_wide_90days)] <- F
head(devices_wide_90days)
colSums(devices_wide_90days)

v <- vennCounts(devices_wide_90days[,2:5])
vennDiagram(v)


devices_by_domain <- read.csv("device_type_by_domain.csv", header=T, sep=',')
devices_by_domain <-dcast(devices_by_domain, name ~ device, value.var="count")
devices_by_domain[is.na(devices_by_domain)] <- 0
colSums(devices_by_domain[,-1])
write.csv(devices_by_domain,"device_type_by_domain_wide.csv")

devices_90days_by_domain <- read.csv("device_type_by_domain_may15-14tojun30-14.csv", header=T, sep=',')
devices_90days_by_domain <-dcast(devices_90days_by_domain, name ~ device, value.var="count")
devices_90days_by_domain[is.na(devices_90days_by_domain)] <- 0
colSums(devices_90days_by_domain[,-1])
write.csv(devices_90days_by_domain,"device_type_by_domain_may15-14tojun30-14_wide.csv")