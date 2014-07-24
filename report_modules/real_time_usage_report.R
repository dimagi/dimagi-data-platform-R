# Real-time usage report
# July 2, 2014
# The purpose of this script is to create a real-time report module for the
# data platform. This report will be based on the aggregate FLW monthly tables
# which were computed from the data platform visit tables.
# The batch entry section of this report must only be created for domains 
# that have manual form annotations. We can look at median visit duration and 
# visit time of day for all domains, regardless of hand annotations.
#------------------------------------------------------------------------------#

# Clear workspace and attach packages
rm(list = ls())
suppressPackageStartupMessages
library(zoo) #work with mm/yy calendar dates without day
library(ggplot2) #graphing across multiple domains
library(gridExtra) #graphing plots in columns/rows for ggplot

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

# Extract unique levels from obsnum and domain_char
f1 = as.factor(all_monthly$obsnum)
obsnum_levels = as.numeric(levels(f1))
f2 = as.factor(all_monthly$domain_char)
domain_levels = levels(f2)
#-----------------------------------------------------------------------------#

#CREATE PLOTS

#-----------------------------------------------------------------------------#
#Batch entry % by monthly index
#Batch entry should be calculated in the monthly tables only for travel visits
#between unrelated cases. The denominator is the total # of travel visits. 

#By multiple domains
#Calculate median within each obsum & domain level. This creates an array.
array_active = with(all_monthly, tapply(batch_entry_percent, 
                                        list(obsnum, domain_char), median, 
                                        na.rm = T))
#Convert array to a data frame
df_intermediate = as.data.frame (array_active)
# Initialize a vector and then append all medians together in this vector
vector_median = c() 
for (i in 1:length(domain_levels)) {
    vector_median = append(vector_median, df_intermediate[,i])
}
# Convert vector to dataframe - add domain numbers and obsnum back in, based on
# the obsnum_levels and domain_levels
df = as.data.frame (vector_median)
colnames(df) = c("batch_per_med")
df$domain_num = rep(1:length(domain_levels), each = length(obsnum_levels))   
df$obsnum = rep(1:length(obsnum_levels), length(domain_levels))
df$domain_num <- as.factor(df$domain_num)
g_batch_per_domains = (
    ggplot(data=df, aes(x=obsnum, y=batch_per_med)) +
        geom_line(aes(group=domain_num, colour=domain_num))) + 
    scale_y_continuous(limits = c(0, 100)) + 
    ggtitle("Batch entry (%) of travel visits by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Batch entry (%), median") +
    theme(axis.text=element_text(size=12), 
    axis.title=element_text(size=14,face="bold")) 

#-----------------------------------------------------------------------------#

#Visit duration (mins) by obsnum

#By multiple domains
#Calculate median within each obsum & domain level. This creates an array
array_active = with(all_monthly, tapply(median_visit_duration, 
                                        list(obsnum, domain_char), median, 
                                        na.rm = T))
#Convert array to a data frame
df_intermediate = as.data.frame(array_active)

# Initialize vector and then append all medians together
vector_median = c() 
for (i in 1:(length(domain_levels))) {
    vector_median = append(vector_median, df_intermediate[,i])
}
# Convert vector to dataframe - add domain numbers and obsnum back in, based on
# the obsnum_levels and domain_levels 
df = as.data.frame(vector_median)
colnames(df) = c("visit_dur")
df$domain_num = rep(1:length(domain_levels), each = length(obsnum_levels))   
df$obsnum = rep(1:length(obsnum_levels), length(domain_levels))
df$domain_num <- as.factor(df$domain_num)
g_visit_dur_domain = (
    ggplot(data=df, aes(x=obsnum, y=visit_dur)) +
        geom_line(aes(group=domain_num, colour=domain_num))) +
    scale_y_continuous(limits = c(0, (max(df$visit_dur, na.rm = T) + 5))) + 
    ggtitle("Visit duration (mins) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visit duration (mins), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold")) 

#-----------------------------------------------------------------------------#

#Morning/afternoon visits by monthly index

#Calculate the total % of morning/afternoon visits
morn_aft_per = as.data.frame(cbind(all_monthly$morning, all_monthly$afternoon)) 
colnames(morn_aft_per) = c("morn", "aft")
morn_aft_per$morn_aft = rowSums(morn_aft_per, na.rm=T)
all_monthly$morn_aft_per = morn_aft_per$morn_aft

#By multiple domains
#Calculate median within each obsum & domain level. This creates an array.
array_active = with(all_monthly, tapply(morn_aft_per, 
                                        list(obsnum, domain_char), median, 
                                        na.rm = T))
#Convert array to a data frame
df_intermediate = as.data.frame (array_active)
# Initialize a vector and then append all medians together in this vector
vector_median = c() 
for (i in 1:length(domain_levels)) {
    vector_median = append(vector_median, df_intermediate[,i])
}
# Convert vector to dataframe - add domain numbers and obsnum back in, based on
# the obsnum_levels and domain_levels
df = as.data.frame (vector_median)
colnames(df) = c("morn_aft_per_med")
df$domain_num = rep(1:length(domain_levels), each = length(obsnum_levels))   
df$obsnum = rep(1:length(obsnum_levels), length(domain_levels))
df$domain_num <- as.factor(df$domain_num)
g_morn_aft_per_domains = (
    ggplot(data=df, aes(x=obsnum, y=morn_aft_per_med)) +
        geom_line(aes(group=domain_num, colour=domain_num))) +  
    ggtitle("Morning/afternoon visits (%) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Morning/afternoon visits (%), median") +
    theme(axis.text=element_text(size=12), 
    axis.title=element_text(size=14,face="bold")) 

#-----------------------------------------------------------------------------#

#PRINT PLOTS AND EXPORT TO PDF

#-----------------------------------------------------------------------------#
require(gridExtra)

pdf("Batch_entry_percent.pdf")
grid.arrange(g_batch_per_overall, g_batch_per_domains, nrow=2)
dev.off()

pdf("Visit_duration.pdf")
grid.arrange(g_visit_dur_overall, g_visit_dur_domain, nrow=2)
dev.off()

pdf("Morn_aft_percent.pdf")
grid.arrange(g_morn_aft_per_overall, g_morn_aft_per_domains, nrow=2)
dev.off()

