#The purpose of this code is to create quantitative tests to evaluate our
#usage indicators. See the following document for more detail:
#https://docs.google.com/a/dimagi.com/document/d/1hP-ewigPuUwuac8K9Tx-VC9Z8epC03lMrnwqzNWveY8/edit

library(zoo) #work with mm/yy calendar dates without day
library(lubridate)
library(ggplot2)

#------------------------------------------------------------------------#
#DATA MANAGEMENT
#------------------------------------------------------------------------#
#Import all_monthly
output_directory <- tmp_report_pdf_dir
read_directory <- aggregate_tables_dir
source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
source(file.path("aggregate_tables","monthly_func.R", fsep = .Platform$file.sep))
all_monthly <- merged_monthly_table (domains_for_run, read_directory)
all_monthly <- add_splitby_col(all_monthly,domain_table,report_options$split_by)

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

#------------------------------------------------------------------------#
#Tests for usage indicator evaluation
#------------------------------------------------------------------------#

indicators_to_test = c("visits", "active_days_percent", "median_visits_per_active_day", 
                       "case_registered", "follow_up_unique_case", "median_visit_duration", 
                       "nforms_per_month", "user_numeric")

#------------------------------------------------------------------------#
#TEST 1
#Calculate CVs by project by calendar_month
#First detach plyr because it's doing weird things with sd calculation in dplyr, then
#check # of rows with sd = NA to make sure that there really is only one observation
#for that domain for that month
#nrow(all_monthly[all_monthly$calendar_month == "Feb 2011" & all_monthly$domain_name == "mvp-sauri",])
#The rows with sd = NA (because of only one month worth of observation) 
#will be excluded from all CV calculations, which is the correct thing to do.
detach(package:plyr)

test_1 <- function(x) {
test_1_gp = group_by(all_monthly, domain_name, calendar_month)
test_1_compute = summarise(test_1_gp,
      mean_indicator = mean(x, na.rm = T),
      sd_indicator = sd(x, na.rm=T))
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

#Initialize test score vector
test_1_score_vector = c()
#Fill test score vector
for (i in indicators_to_test){
    test_1_score <- test_1(as.name(i))
    test_1_score_vector <- append(test_1_score_vector, test_1_score)
}
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
detach(package:dplyr)
require(plyr)
require(dplyr)
test_2 <- function(x,y) {
raw_percentile = ddply(all_monthly, .(domain_name, calendar_month), 
             function(x,y) transform(x, percentile=ecdf(x$y)(x$y)))
test_2_gp = group_by(raw_percentile, user_id)
test_2_compute = summarise(test_2_gp,
                           mean_indicator = mean(percentile, na.rm = T),
                           sd_indicator = sd(percentile, na.rm=T))
test_2_compute$cv = (test_2_compute$sd_indicator/test_2_compute$mean_indicator)*100
test_2_score = median(test_2_compute$cv, na.rm=T)
return(test_2_score)
}
#Initialize test score vectors
test_2_score_vector = c()
for (i in indicators_to_test){
    test_2_score <- test_2(all_monthly, as.name(i))
    test_2_score_vector <- append(test_2_score_vector, test_2_score)
}
#------------------------------------------------------------------------#

#------------------------------------------------------------------------#
#TEST 3
#Subset data frames for holiday months and active months
#Then calculate median indicator values per user for each dataframe
#For now, we will work with just India domains, counting November as the main
#holiday month. Confirming with Devu and Mohini if we also need to include 
#December as a holiday month. 
detach(package:dplyr)
detach(package:plyr)
library(dplyr)

#Calculate median indicator for holiday and active subsets based on 
#holiday month = November
#To select more than one month, use perl = T. For example:
#all_monthly[grep("Nov|Dec", all_monthly$calendar_month, perl=T), ]

holiday_subset <- all_monthly[grep("Nov", all_monthly$calendar_month, perl=T), ]
active_subset <- all_monthly[grep("Nov", all_monthly$calendar_month, invert = T, perl = T), ]

test_3 <- function(x) {
test_3_compute_holiday <- holiday_subset %.%
    group_by(domain_name, user_id) %.%
    summarise(median_indicator_holiday=median(x, na.rm=TRUE))

test_3_compute_active <- active_subset %.%
    group_by(domain_name, user_id) %.%
    summarise(median_indicator_active=median(x, na.rm=TRUE))

#Merge the two data frames by domain and user_id and then compute % change
test_3_compute <- merge(test_3_compute_holiday, test_3_compute_active, 
                        by=c("domain_name", "user_id"))
test_3_compute$per_change <- 
    (test_3_compute$median_indicator_holiday-test_3_compute$median_indicator_active)/
    test_3_compute$median_indicator_active *100
test_3_score <- median(test_3_compute$per_change, na.rm = T)
return(test_3_score)
}

test_3_score_vector <- sapply(indicators_to_test, test_3)
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
  summarise(obsnum_max = max(obsnum))
#Create list of full obsnum sequence based on max obsnum per user_id
all_monthly_new <- lapply(split(all_monthly_max, list(all_monthly_max$domain_name, 
                                                      all_monthly_max$user_id), drop = T),
                          function(x) seq(x$obsnum_max))
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

test_4 <- function(x) {
#Create logic vector of inactive months
1:10 %in% c(1,3,5,9)

}

