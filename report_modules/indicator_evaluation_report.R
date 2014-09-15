#The purpose of this code is to create quantitative tests to evaluate our
#usage indicators. See the following document for more detail:
#https://docs.google.com/a/dimagi.com/document/d/1hP-ewigPuUwuac8K9Tx-VC9Z8epC03lMrnwqzNWveY8/edit

library(zoo) #work with mm/yy calendar dates without day
library(lubridate)
library(ggplot2)

#------------------------------------------------------------------------#
#DATA MANAGEMENT
#------------------------------------------------------------------------#

#Set report_options
report = "monthly_usage_report"
report_options <- get_report_options(run_conf,report)

#Remove demo users
#We also need to find a way to exclude admin/unknown users
all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
#Remove any dates before report start_date and after report end_date
names (all_monthly)[names(all_monthly) == "first_visit_date.x"] = "first_visit_date"
all_monthly$first_visit_date = as.Date(all_monthly$first_visit_date)
all_monthly$last_visit_date = as.Date(all_monthly$last_visit_date)
start_date = as.Date(report_options$start_date)
end_date = as.Date(report_options$end_date)
all_monthly = subset(all_monthly, all_monthly$first_visit_date >= start_date
                     & all_monthly$last_visit_date <= end_date)
#Convert calendar_month (character) to yearmon class since as.Date won't work 
#without a day.
all_monthly$month.index = as.yearmon(all_monthly$month.index, "%b %Y")
#Change column names as needed
names (all_monthly)[names(all_monthly) == "X"] = "row_num"
names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names (all_monthly)[names(all_monthly) == "active_day_percent"] = "active_days_percent"
names (all_monthly)[names(all_monthly) == "domain"] = "domain_char"
names (all_monthly)[names(all_monthly) == "numeric_index"] = "obsnum"
#Convert median visit duration to minutes
all_monthly$median_visit_duration = round(all_monthly$median_visit_duration/60,
                                          digits=2) 
# Convert relevant indicators to percentages
all_monthly$active_days_percent= (all_monthly$active_days_percent)*100
#Convert user_id to numeric to create a "red herring" indicator
#Note that nlevels of user_id higher than length(unique(user_id)) because we
#deleted demo_user and some other users that were out of the
#reporting range after importing all_monthly
all_monthly$user_numeric = as.numeric(all_monthly$user_id)
#Convert calendar month to character because dplyr doesn't like yearmon class
all_monthly$calendar_month = as.character(all_monthly$calendar_month)
#Convert first visit date to first day of the month
all_monthly$month_abbr <- as.Date(as.yearmon(all_monthly$first_visit_date, 
                                             frac = 0))

#Merge domain facets from domain table into all_monthly table
facets_to_merge <- select(domain_table, name, country, Sector, Sub.Sector,
                          business_unit)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain_name", 
                     by.y = "name", all.x = T)

#------------------------------------------------------------------------#
#Tests for usage indicator evaluation
#------------------------------------------------------------------------#

indicators_to_test = c("visits", "active_days_percent", "median_visits_per_active_day", 
                       "case_registered", "follow_up_unique_case", "median_visit_duration", 
                       "nforms_per_month", "user_numeric")

#Health sector domains to exclude because median # cases followed-up per domain = 0
#There are 22 of these domains of all health sector domains, giving us 73 domains
domains_to_exclude <- c("a5288-study",
                        "agada-tufts-nnos",
                        "aphiaplusnc-2012",
                        "bihar-project",
                        "cmmhr",
                        "ekam",
                        "gsid",
                        "ict-women-health",
                        "iicp",
                        "itech-etc",
                        "jhccpmz",
                        "mc-inscale",
                        "mchip-haryana",
                        "msf-demo",
                        "operation-smile",
                        "projectbom",
                        "reach-india",
                        "sneha-mnh",
                        "stjohns-soukhya",
                        "union-jharkhand",
                        "wits-ca",
                        "wvmozambique")

all_monthly <- all_monthly[!(all_monthly$domain_name %in% domains_to_exclude),]

#Pick random 10% of these 73 domains for our training dataset of 8 domains
#sample_domains <- sample(unique(all_monthly$domain_name), 8)
#This generated the following vector on first run
sample_domains <- c("afguinea", "nsf-lifefirst", "yonsei-emco", "keiskamma",
                    "image-sa", "ictwomenhealth", "fenway", "tulasalud") 
training_set <- all_monthly[all_monthly$domain_name %in% sample_domains,]
all_monthly <- training_set
#------------------------------------------------------------------------#
#TEST 1
#Calculate CVs by project by calendar_month
#First detach plyr because it's doing weird things with sd calculation in dplyr, then
#check # of rows with sd = NA to make sure that there really is only one observation
#for that domain for that month
#nrow(all_monthly[all_monthly$calendar_month == "Feb 2011" & all_monthly$domain_name == "mvp-sauri",])
#The rows with sd = NA (because of only one month worth of observation) 
#will be excluded from all CV calculations, which is the correct thing to do.
detach(package:dplyr)
detach(package:plyr)
library(dplyr)
source('s_dplyr.R')

test_1 <- function(indicator, data) {

    test_1_compute <- data %.%
    group_by(domain_name, calendar_month) %.%
    s_summarise(paste0('mean_indicator=mean(', indicator, ', na.rm=TRUE)'), 
                paste0('sd_indicator=sd(', indicator, ', na.rm=TRUE)'))

    test_1_compute$cv = (test_1_compute$sd_indicator/test_1_compute$mean_indicator)*100

    #Compute CV of CVs by project
    test_1_gp_cv = group_by(test_1_compute, domain_name)
    test_1_compute_cv = summarise(test_1_gp_cv,
                              mean_indicator = mean(cv, na.rm = T),
                              sd_indicator = sd(cv, na.rm=T))
    test_1_compute_cv$cv = (test_1_compute_cv$sd_indicator/test_1_compute_cv$mean_indicator)*100
    test_1_score = median(test_1_compute_cv$cv)
    return(test_1_score)
}

#Create vector of test scores
test_1_score_vector <- sapply(indicators_to_test, test_1, data=all_monthly)
print(test_1_score_vector)

#Can also calculate differences between months per project, but need to consider cases of 
#skipped months - we are assuming no skipped months for now (too simplistic)
#Also, might want count diff in variance between one month and all other months -
#will this be a more robust measure rather than just diff between consecutive months?
#list_diff = tapply(test_1_compute$cv, test_1_compute$domain_name, diff)
#test_1_score = median(unlist(lapply(list_diff, median, na.rm=T)))
#------------------------------------------------------------------------#

#------------------------------------------------------------------------#
#TEST 2
#Group by project and then by obsnum
#Similar to (2), users with just one month on CC will be excluded from this analysis
#Check sd = NA with the following code:
#nrow(raw_percentile[raw_percentile$user_id == "c536bc72043e1c225ed9e30884c5641a",])

percentile <- function(x) ecdf(x)(x)

test_2 <- function(y) {
    percentile_s <- paste0('percentile=percentile(', y, ')')
    raw_percentile <- all_monthly %.%
        group_by(domain_name, calendar_month) %.%
        s_mutate(percentile_s)
    
    test_2_compute <- raw_percentile %.%
        group_by(user_id) %.%
        summarise(
            mean_indicator=mean(percentile, na.rm=TRUE),
            sd_indicator=sd(percentile, na.rm=TRUE)
        )
    
    test_2_compute$cv <- (test_2_compute$sd_indicator / test_2_compute$mean_indicator) * 100
    test_2_score <- median(test_2_compute$cv, na.rm=TRUE)
    return(test_2_score)
}

#Create vector of test scores
test_2_score_vector <- sapply(indicators_to_test, test_2)
print(test_2_score_vector)

#------------------------------------------------------------------------#

#------------------------------------------------------------------------#
#TEST 3
#Subset data frames for holiday months and active months
#Then calculate median indicator values per user for each dataframe
#For now, we will work with just India domains, counting November as the main
#holiday month. Confirming with Devu and Mohini if we also need to include 
#December as a holiday month. 

#Calculate % change in median indicator values for holiday vs. active months
#Based on India holiday month = November for now
#To select more than one month, use perl = T. For example:
#all_monthly[grep("Nov|Dec", all_monthly$calendar_month, perl=T), ]

test_3 <- function(indicator, data) {
    holiday_subset <- data[grep("Oct|Nov", data$calendar_month, perl=T), ]
    active_subset <- data[grep("Oct|Nov", data$calendar_month, invert = T, perl = T), ]
    
    test_3_compute_holiday <- holiday_subset %.%
        group_by(domain_name, user_id) %.%
        s_summarise(paste0('median_indicator_holiday=median(', indicator, ', na.rm=TRUE)'))
    
    test_3_compute_active <- active_subset %.%
        group_by(domain_name, user_id) %.%
        s_summarise(paste0('median_indicator_active=median(', indicator, ', na.rm=TRUE)'))
    
    #Merge the two data frames by domain and user_id and then compute % change
    test_3_compute <- merge(test_3_compute_holiday, test_3_compute_active,
                            by=c("domain_name", "user_id"))
    test_3_compute$per_change <-
        (test_3_compute$median_indicator_holiday-test_3_compute$median_indicator_active)/
        test_3_compute$median_indicator_active *100
    test_3_score <- median(test_3_compute$per_change, na.rm = T)
    return(test_3_score)
}

test_3_score_vector <- sapply(indicators_to_test, test_3, data=all_monthly)
print(test_3_score_vector)

#------------------------------------------------------------------------#

#------------------------------------------------------------------------#
#TEST 4
#Subset data frames for one month before attrition vs. active months
#http://r.789695.n4.nabble.com/adding-in-missing-values-in-a-sequence-td839900.html

library(reshape)

#Count NAs in indicators to test before running this test because we are later on 
#adding NAs as indicator values whenever there is an inactive month. There should be
#none/very few NA values in our indicators to test before we add in the inactive months
#for this test
all_monthly_check <- select(all_monthly, domain_name, user_id, obsnum, visits, active_days_percent, median_visits_per_active_day, 
                            case_registered, follow_up_unique_case, median_visit_duration, 
                            nforms_per_month, user_numeric)
sapply(all_monthly_check, summary)

#Create new all_monthly with missing obsnum filled in, by user_id
#First get max obsnum per user_id
all_monthly_max <- all_monthly %.%
  group_by(domain_name, user_id) %.%
  summarise(obsnum_max = max(obsnum),
            obsnum_max_plus = obsnum_max+1)
#Create list of full obsnum sequence based on max obsnum per user_id
all_monthly_new <- lapply(split(all_monthly_max, list(all_monthly_max$domain_name, 
                                                      all_monthly_max$user_id), drop = T),
                          function(x) seq(x$obsnum_max_plus))
#One row per user - with one column per obsnum
column_per_obsnum <- do.call(rbind,lapply(all_monthly_new, 
                             function(x) c(as.numeric(x),
                                           rep(NA,max(sapply(all_monthly_new,length)-length(x)))))) 
#Convert columns to rows - one row per obsnum per user
mdata <- melt(column_per_obsnum, id=c("row.names"))
#Sort by domain.user (X1) and then by obsnum. This is a matrix.
mdata <- arrange(mdata, X1, value)
#Pull apart domain_name and user_id from X1
mat = as.matrix(mdata[,1])
mat2 = apply(mat, 2, function(x) unlist(strsplit(x, ".", fixed = TRUE)))
mat_user_id = mat2[c(F,T),]
mat_domain_name = mat2[c(T,F),]
#Column bind user_id and domain_name back to mdata and convert to dataframe
#Rename columns and delete rows with obsnum = NA
mdata = cbind(mdata, mat_user_id)
mdata = as.data.frame(cbind(mdata, mat_domain_name))
mdata = select(mdata, -X2)
names(mdata) = c("domain_user", "obsnum", "user_id", "domain_name")
mdata = filter(mdata, obsnum != "NA")
#Merge with all_monthly_check by domain_name, user_id, and obsnum: keeping all rows in mdata
mdata <- merge(mdata, all_monthly_check, by=c("domain_name", "user_id", "obsnum"), all.x = TRUE)
#Check # inactive months by looking at # NA's for all indicators (they should be the same)
sapply(mdata, summary)
#Also check the last row of mdata to make sure it contains NAs
mdata[6874,]
#Sort by domain_name, user_id and obsnum
mdata <- arrange(mdata, domain_name, user_id, obsnum)
#Create logic vector of inactive months
mdata$inactive_month <- is.na(mdata$visits)
#Create logic vector of months before all inactive months
#Choose only the second element in inactive_month onwards and then append "F"
mdata2 <- group_by(mdata, domain_name, user_id)
mdata2 <- mutate(mdata2, 
                before_inactive = append(c(inactive_month[2:length(inactive_month)]), F))

test_4 <- function(indicator, data) {
    active_subset <- mdata2[mdata2$before_inactive == F, ]
    before_inactive_subset <- mdata2[mdata2$before_inactive == T, ]
    
    test_4_compute_active <- active_subset %.%
        group_by(domain_name, user_id) %.%
        s_summarise(paste0('median_indicator_active=median(', indicator, ', na.rm=TRUE)'))
    
    test_4_compute_before_active <- before_inactive_subset %.%
        group_by(domain_name, user_id) %.%
        s_summarise(paste0('median_indicator_before_inactive=median(', indicator, ', na.rm=TRUE)'))
    
    #Merge the two data frames by domain and user_id and then compute % change
    test_4_compute <- merge(test_4_compute_active, test_4_compute_before_active,
                            by=c("domain_name", "user_id"))
    test_4_compute$per_change <-
        (test_4_compute$median_indicator_before_inactive-test_4_compute$median_indicator_active)/
        test_4_compute$median_indicator_active *100
    test_4_score <- median(test_4_compute$per_change, na.rm = T)
    return(test_4_score)
}

test_4_score_vector <- sapply(indicators_to_test, test_4, data=mdata2)
print(test_4_score_vector)

#Combine all test vectors into dataframe
test_scores <- as.data.frame(cbind(test_1_score_vector, test_2_score_vector, 
                     test_3_score_vector, test_4_score_vector))




#------------------------------------------------------------------------#
#Check all health sector domains to remove any with median # cases f/u == 0
#------------------------------------------------------------------------#
overall_split = ddply(all_monthly, .(domain_name), summarise,
                      cases_fu_med = median(follow_up_unique_case, na.rm=T))
overall_split = arrange(overall_split, cases_fu_med)

