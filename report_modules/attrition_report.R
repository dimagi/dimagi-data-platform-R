# Attrition report
# July 17, 2014
# The purpose of this script is to create an FLW attrition report module for the
# data platform. This report will be based on the aggregate FLW monthly tables
# which were computed from the data platform visit tables.
#------------------------------------------------------------------------------#

# Clear workspace and attach packages
rm(list = ls())
suppressPackageStartupMessages
library(zoo) #work with mm/yy calendar dates without day
library(ggplot2) #graphing across multiple domains
library(gridExtra) #graphing plots in columns/rows for ggplot
library(plyr)
library(RColorBrewer) #Color palettes

# Set working directories here
# Where my data tables are saved and where R output is saved
# Work laptop
read_directory = "/Users/Rashmi/Dropbox (Dimagi)/R analysis/Rashmi/Sample monthly tables/"
output_directory = "C:/Users/Rashmi/Dropbox (Dimagi)/R analysis/Visuals/DP demo/July 7 2014"
# Home laptop
read_directory = "/Users/rdayalu/Dropbox (Dimagi)/R analysis/Rashmi/Sample monthly tables/"
output_directory = "/Users/rdayalu/Dropbox (Dimagi)/R analysis/Visuals/DP demo/July 7 2014"
#------------------------------------------------------------------------#

# Read in monthly tables - merged table
filename = paste(read_directory, "monthly_merge.csv", sep="")
all_monthly = read.csv(file=filename, header = TRUE, as.is = TRUE)
setwd(output_directory)

#Change column names names as needed
colnames(all_monthly)[1] = "row_num"
colnames(all_monthly)[2] = "calendar_month"

# Convert relevant indicators to numeric
# all_monthly$active_days_percent= as.numeric(all_monthly$active_days_percent)
# all_monthly$visits = as.numeric(all_monthly$visits)

#Convert calendar_month (character) to yearmon class since as.Date won't work 
#without a day. Sort by calendar_month for each FLW and then label each 
#month for each FLW in chronological order. 
all_monthly$calendar_month = as.yearmon(all_monthly$calendar_month, "%b-%y")

#Create monthly index for each FLW
#Note: The code below doesn't care if an FLW has an inactive month. Only the
#months with observations are counted, so no FLW will have any gaps, which
#isn't correct. We need to rewrite this code.
all_monthly = all_monthly[order(all_monthly$user_id,all_monthly$calendar_month),]
all_monthly$obsnum = sequence(rle(all_monthly$user_id)$lengths)

#Create a new domain variable with a character in the front so that it 
#can be used as a column name later without a problem
all_monthly$domain_char = paste("d", all_monthly$domain.index, sep="")

#Create new indicators as needed
# % of registration visits
#Actually, this variable doesn't make much sense. Will just use # of registered
# cases instead.
all_monthly$reg_visits_per = (all_monthly$case_registered/all_monthly$visits)*100
# % of unique cases followed-up (of cumulative open cases at month start)
# Note that cum_case_registered is a cumulative of ALL cases registered by a FLW
# We need to change this to include only open cases belonging to a FLW in any
# given month, otherwise the denominator will continue to inflate.
# Will just use # of followed-up unique cases until we get the correct deonominator
all_monthly$case_fu_per = (all_monthly$follow_up_unique_case/all_monthly$cum_case_registered)*100

# Extract unique levels from obsnum and domain_char
f1 = as.factor(all_monthly$obsnum)
obsnum_levels = as.numeric(levels(f1))
f2 = as.factor(all_monthly$domain_char)
domain_levels = levels(f2)

# compute numeric month index (remove month.index.numeric if it is already there)
all_monthly$numeric_index = 1

# this calculates number of months of a given Date from the origin
monnb = function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
mondf = function(d1, d2) { monnb(d1) - monnb(d2) }
# this returns the distance between the current and the previous month
moncalc = function(x) { # x = length of active month of a user
  if (length(x) > 1) {
    for (i in 2:length(x)) 
      y[i] = mondf(x[i], x[i-1]) + y[i-1]    
  }
  return(result)
}
all_monthly$first_visit_date = as.Date(all_monthly$first_visit_date, "%m/%d/%y")
all_monthly = ddply(all_monthly, .(user_id), transform, numeric_index = moncalc(first_visit_date))

#Perform the following function by flw
#If the flw has been around for only one month
#That month will always have retained = F and addition = T
#If the flw has been around for more than one month
#The last month for each flw will always have retained = F
#The first month for each flw will always have addition = T 
#The next t row should always be just one step up from the previous t row (retained = T)
#If not equal, then the FLW was not retained (retained = F) 
#The next t row should always be just one step up from the previous t row (addition = F)
#If not equal, then the flw was added (addition = T) 
df2 = ddply(all_monthly, .(user_id), function(x) {
    x = x[order(x$obsnum), ]
    if (length(x$obsnum) == 1) {
        x$retained <- FALSE
        x$addition <- TRUE
    }
    else {
        x$retained <- c(x$obsnum[1:(length(x$obsnum)-1)] + 1 == x$obsnum[2:length(x$obsnum)], FALSE)
        x$addition <- c(TRUE, x$obsnum[2:length(x$obsnum)] != x$obsnum[1:(length(x$obsnum)-1)] + 1)
    }
    return(x)
})

#This gives the attrition rate in the next month, using the # in obsnum as the denominator
#Numerator is # of flws that are not retained in the next month from the previous month
#This also gives the addition rate in the obsnum month, using # in obsnum as the denominator
#Numerator is # of flws that were added in for that obsnu  
attrition_table = ddply(df2, .(obsnum), function(x) c(attrition=mean(!x$retained)*100, 
additions=mean(x$addition)*100))
    

#-----------------------------------------------------------------------------#

#CREATE PLOTS

#-----------------------------------------------------------------------------#


