#The purpose of this code is to create quantitative tests to evaluate our
#usage indicators. See the following document for more detail:
#https://docs.google.com/a/dimagi.com/document/d/1hP-ewigPuUwuac8K9Tx-VC9Z8epC03lMrnwqzNWveY8/edit

library(zoo)
library(lubridate)
library(ggplot2)
library(scales) #to customize ggplot axis labeling
library(gridExtra) #graphing plots in columns/rows for ggplot
library(RColorBrewer) #Color palettes
source('s_dplyr.R')

#------------------------------------------------------------------------#
#DATA MANAGEMENT
#------------------------------------------------------------------------#

all_monthly <- monthly_table

#Set report_options
report = "monthly_usage_report"
report_options <- get_report_options(run_conf,report)

#Remove demo users
#We also need to find a way to exclude admin/unknown users
all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
#Remove any dates before report start_date and after report end_date
names (all_monthly)[names(all_monthly) == "date_first_visit"] = "first_visit_date"
names (all_monthly)[names(all_monthly) == "date_last_visit"] = "last_visit_date"
all_monthly$first_visit_date = as.Date(all_monthly$first_visit_date)
all_monthly$last_visit_date = as.Date(all_monthly$last_visit_date)
start_date = as.Date(report_options$start_date)
end_date = as.Date(report_options$end_date)
all_monthly = subset(all_monthly, all_monthly$first_visit_date >= start_date
                     & all_monthly$last_visit_date <= end_date)

#Change column names as needed
names (all_monthly)[names(all_monthly) == "X"] = "row_num"
names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names (all_monthly)[names(all_monthly) == "active_day_percent"] = "active_days_percent"

#Round median visit duration to one decimal place
all_monthly$median_visit_duration <- round(all_monthly$median_visit_duration,
                                           digits = 1)
# Convert relevant indicators to percentages
all_monthly$active_days_percent= (all_monthly$active_days_percent)*100

#Create "red herring" indicators
#user_id to numeric: First convert from character to factor
all_monthly$user_id <- as.factor(all_monthly$user_id)
all_monthly$user_numeric = as.numeric(all_monthly$user_id)
#domain to numeric: First convert from character to factor
all_monthly$domain <- as.factor(all_monthly$domain)
all_monthly$domain_numeric = as.numeric(all_monthly$domain)
#Random numbers per row, using wide range (5x nrow), not specifying distribution, mean or sd
all_monthly$sample_undefined <- sample(1:(5*nrow(all_monthly)), nrow(all_monthly), replace=F)
#Random numbers per row, normal distribution, defining mean and sd
all_monthly$sample_normal <- rnorm(nrow(all_monthly), mean = 10, sd = 1)

#Merge domain facets from domain table into all_monthly table
facets_to_merge <- select(domain_table, name, country, Sector, Sub.Sector,
                          business_unit, Test.Project., active)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#Create obsnum here since we can't create this at the aggregate table stage 
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')

add_index <- function(x) {
  start <- min(x$calendar_month, na.rm=TRUE)
  x$numeric_index <- sapply(x$calendar_month, function(end) interval(start, end) %/% months(1))
  return(x)
}

df2 <- all_monthly %.% group_by(domain, user_id) %.% do(add_index(.))
df2$obsnum <- df2$numeric_index + 1
all_monthly <- select(df2, -numeric_index)

all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

#------------------------------------------------------------------------#
#Tests for usage indicator evaluation
#------------------------------------------------------------------------#

indicators_to_test = c("nvisits", "active_days_percent", "median_visits_per_day", 
                       "ncases_registered", "nunique_followups", "median_visit_duration", 
                       "nforms", "user_numeric", "domain_numeric", "sample_undefined",
                       "sample_normal")

#Health sector domains to exclude because median 
#cases followed-up per domain == 0
#Note that we will also want to exclude forms submitted by atypical FLW apps
#https://docs.google.com/a/dimagi.com/spreadsheets/d/1QwkgRZPR81rQF9h-E7END_ontZye8xQW_WojAObKsgU/edit#gid=0
#This exclusion will need to happen at the form/app level, so we need to figure
#out how to do it.

domains_to_exclude <- c("a5288-study",
                        "aed-togo",
                        "agada-tufts-nnos",
                        "aiha-ca",
                        "aphiaplusnc-2012",
                        "bihar-project",
                        "cidrz",
                        "cmmhr",
                        "deoghar",
                        "ekam",
                        "fh-mozambique",
                        "gsid",
                        "ict-women-health",
                        "iicp",
                        "itech-etc",
                        "jhccpmz",
                        "kawok-vc-desarrollo",
                        "mc-inscale",
                        "mc-socialautopsy",
                        "mchip-haryana",
                        "mgh-india",
                        "msf-demo",
                        "mtsinai",
                        "oneworld",
                        "operation-smile",
                        "projectbom",
                        "promot",
                        "reach-india",
                        "sneha-mnh",
                        "special-olympics",
                        "stjohns-soukhya",
                        "union-jharkhand",
                        "wits-ca",
                        "wvmozambique")

all_monthly <- all_monthly[!(all_monthly$domain %in% domains_to_exclude),]

#------------------------------------------------------------------------#
#Training dataset
#------------------------------------------------------------------------#

#Picked random 10% of these 73 domains for our training dataset of 8 domains
#sample_domains <- sample(unique(all_monthly$domain_name), 8)
#This generated the following vector on first run (below)
#Note that the list of health domains has increased, but we will stick with this
#training dataset of 8 domains.
sample_domains <- c("afguinea", "nsf-lifefirst", "yonsei-emco", "keiskamma",
                    "image-sa", "ictwomenhealth", "fenway", "tulasalud") 
training_set <- all_monthly[all_monthly$domain %in% sample_domains,]
master_set <- all_monthly
all_monthly <- training_set

#Create dataset for smallest domain of training set - nsf-lifefirst
all_monthly_nsf <- subset(all_monthly[all_monthly$domain == "nsf-lifefirst",])
#Create dataset for largest domain of training set - tulasalud
all_monthly_tula <- subset(all_monthly[all_monthly$domain == "tulasalud",])

#------------------------------------------------------------------------#
#Descriptive stats and distribution histograms for the dataset
#------------------------------------------------------------------------#

all_monthly %.% group_by(domain) %.% summarise(nusers = length(unique(user_id)))
all_monthly %.% group_by(domain) %.% summarise(nobs = length(user_id))
all_monthly %.% group_by(domain) %.% summarise(nmonth = length(unique(calendar_month)))

# Histogram overlaid with kernel density curve
# Overlay with transparent density plot
# Include all observations
myhist <- ggplot(all_monthly_tula, aes(x=nvisits)) + 
    geom_histogram(binwidth=0.5, colour="black", fill="lightblue") +
    geom_vline(aes(xintercept=median(nvisits, na.rm=T)),
               color="red", linetype="dashed", size=1)

# Exclude outliters
no_outliers <- filter(all_monthly_tula, nvisits <= 100)
myhist <- ggplot(no_outliers, aes(x=nvisits)) + 
    geom_histogram(binwidth=1, colour="black", fill="lightblue") +
    geom_vline(aes(xintercept=median(nvisits, na.rm=T)),
               color="red", linetype="dashed", size=1)
    #scale_x_continuous(limits=c(0,20))

#Histograms for training dataset(s)
myhist <- hist(all_monthly_nsf$sample_normal)
multiplier <- myhist$counts / myhist$density
mydensity <- density(all_monthly_nsf$sample_normal)
mydensity$y <- mydensity$y * multiplier[1]
plot(myhist)
lines(mydensity)
abline(v = mean(all_monthly_nsf$sample_normal), col = "blue", lwd = 2)
text(11, 7, labels = paste0("sd=", sd(all_monthly_nsf$sample_normal)))

myhist <- hist(all_monthly_nsf$sample_undefined)
multiplier <- myhist$counts / myhist$density
mydensity <- density(all_monthly_nsf$sample_undefined)
mydensity$y <- mydensity$y * multiplier[1]
plot(myhist)
lines(mydensity)
abline(v = mean(all_monthly_nsf$sample_undefined), col = "blue", lwd = 2)
text(150000, 10, labels = paste0("sd=", sd(all_monthly_nsf$sample_undefined)))

#------------------------------------------------------------------------#
#Function codes for tests
#------------------------------------------------------------------------#

#TEST 1
#Calculate CVs by project by calendar_month
#First detach plyr because it's doing weird things with sd calculation in dplyr, then
#check # of rows with sd = NA to make sure that there really is only one observation
#for that domain for that month
#nrow(all_monthly[all_monthly$calendar_month == "Feb 2011" & all_monthly$domain_name == "mvp-sauri",])
#The rows with sd = NA (because of only one month worth of observation) 
#will be excluded from all CV calculations, which is the correct thing to do.

source('s_dplyr.R')

test_1 <- function(indicator, data) {

    test_1_compute <- data %.%
    group_by(domain, calendar_month) %.%
    s_summarise(paste0('mean_indicator=mean(', indicator, ', na.rm=TRUE)'), 
                paste0('sd_indicator=sd(', indicator, ', na.rm=TRUE)'))

    test_1_compute$cv = (test_1_compute$sd_indicator/test_1_compute$mean_indicator)*100

    #Compute CV of CVs by project
    test_1_gp_cv = group_by(test_1_compute, domain)
    test_1_compute_cv = summarise(test_1_gp_cv,
                              mean_indicator = mean(cv, na.rm = T),
                              sd_indicator = sd(cv, na.rm=T))
    test_1_compute_cv$cv = (test_1_compute_cv$sd_indicator/test_1_compute_cv$mean_indicator)*100
    test_1_score = median(test_1_compute_cv$cv)
    return(test_1_score)
}

#Create vector of test scores
test_1_score_vector <- sapply(indicators_to_test, test_1, data=all_monthly_tula)
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

test_2 <- function(indicator, data) {
    percentile_s <- paste0('percentile=percentile(', indicator, ')')
    raw_percentile <- data %.%
        group_by(domain, calendar_month) %.%
        s_mutate(percentile_s)
    
    test_2_compute <- raw_percentile %.%
        group_by(domain, user_id) %.%
        summarise(
            mean_indicator=mean(percentile, na.rm=TRUE),
            sd_indicator=sd(percentile, na.rm=TRUE)
        )
    
    test_2_compute$cv <- (test_2_compute$sd_indicator / test_2_compute$mean_indicator) * 100
    test_2_score <- median(test_2_compute$cv, na.rm=TRUE)
    return(test_2_score)
}

#Create vector of test scores
test_2_score_vector <- sapply(indicators_to_test, test_2, data = all_monthly_tula)
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
#Create datasets for country/countries based on known holiday months
#To select more than one month, use perl = T. For example:
#all_monthly[grep("Nov|Dec", all_monthly$calendar_month, perl=T), ]

all_monthly_india <- all_monthly[all_monthly$country == "India",] #data1
all_monthly_other <- all_monthly[all_monthly$country != "India",] #data2

test_3 <- function(indicator, data1, data2) {
    holiday_subset1 <- data1[data1$month_abbr == "Oct" | data1$month_abbr == "Nov", ]
    active_subset1 <- data1[!(data1$month_abbr == "Oct" | data1$month_abbr == "Nov"), ]
    
    holiday_subset2 <- data2[data2$month_abbr == "Dec", ]
    active_subset2 <- data2[!(data2$month_abbr == "Dec"), ]
    
    test_3_compute_holiday1 <- holiday_subset1 %.%
        group_by(domain, user_id) %.%
        s_summarise(paste0('median_indicator_holiday=median(', indicator, ', na.rm=TRUE)'))
    
    test_3_compute_holiday2 <- holiday_subset2 %.%
        group_by(domain, user_id) %.%
        s_summarise(paste0('median_indicator_holiday=median(', indicator, ', na.rm=TRUE)'))
    
    test_3_compute_active1 <- active_subset1 %.%
        group_by(domain, user_id) %.%
        s_summarise(paste0('median_indicator_active=median(', indicator, ', na.rm=TRUE)'))
    
    test_3_compute_active2 <- active_subset2 %.%
        group_by(domain, user_id) %.%
        s_summarise(paste0('median_indicator_active=median(', indicator, ', na.rm=TRUE)'))
    
    
    #Merge the two data frames by domain and user_id and then compute % change
    merge1 <- merge(test_3_compute_holiday1, test_3_compute_active1,
                            by=c("domain", "user_id"))
    merge2 <- merge(test_3_compute_holiday2, test_3_compute_active2,
                    by=c("domain", "user_id"))
    
    test_3_compute <- rbind(merge1, merge2)
        
    test_3_compute$per_change <-
        (test_3_compute$median_indicator_holiday-test_3_compute$median_indicator_active)/
        test_3_compute$median_indicator_active *100
    test_3_score <- median(test_3_compute$per_change, na.rm = T)
    return(test_3_score)
}

test_3_score_vector <- sapply(indicators_to_test, test_3, 
                              data1 = all_monthly_india, data2 = all_monthly_other)
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
all_monthly_check <- 
    select(all_monthly_tula, domain, user_id, obsnum, nvisits, active_days_percent, 
           median_visits_per_day, ncases_registered, nunique_followups,
           median_visit_duration, nforms, user_numeric, domain_numeric,
           sample_undefined, sample_normal)
sapply(all_monthly_check, summary)

#Create new all_monthly with missing obsnum filled in, by user_id
#First make two subsets: (1) users with last calendar_month == last month 
# of data pull (2) users with last calendar_month != last month of data pull

print(as.Date(run_conf$reports$end_date)) #print last month of data pull

pre_test_4 <- function(data) {
data$ongoing <- data$calendar_month == "2014-08-01"
domain_ongoing <- data$domain[data$ongoing]
user_id_ongoing <- data$user_id[data$ongoing]
users_ongoing <- data.frame(domain_ongoing, user_id_ongoing)

#Initialize dataframe
all_monthly_ongoing <- as.data.frame(matrix(ncol=40, nrow=10))
names(all_monthly_ongoing) <- names(data)

#Populate with all rows for all ongoing users by domain
for(i in 1:nrow(users_ongoing)) {
    rows_ongoing <- data[(data$domain == users_ongoing$domain_ongoing[i] &
                              data$user_id == users_ongoing$user_id_ongoing[i]),]
    
    all_monthly_ongoing <- rbind(all_monthly_ongoing, rows_ongoing)
}

all_monthly_ongoing <- all_monthly_ongoing[11:nrow(all_monthly_ongoing),]
all_monthly_ongoing$calendar_month <- as.Date(all_monthly_ongoing$calendar_month)

all_monthly_lost <- rbind(data, all_monthly_ongoing)
dup <- duplicated(all_monthly_lost) | duplicated(all_monthly_lost, fromLast = TRUE) 
all_monthly_lost <- all_monthly_lost[!dup, ] 

#Now get max obsnum per user_id
#Add one month for the users that were lost before end date of report pull
#Keep original max for users that stayed on till end date of report pull
max_ongoing <- all_monthly_ongoing %.%
  group_by(domain, user_id) %.%
  summarise(obsnum_max = max(obsnum))

max_lost <- all_monthly_lost %.%
    group_by(domain, user_id) %.%
    summarise(obsnum_max = max(obsnum)+1)

all_monthly_max <- rbind(max_ongoing, max_lost)

#Create list of full obsnum sequence based on max obsnum per user_id
all_monthly_new <- lapply(split(all_monthly_max, list(all_monthly_max$domain, 
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
names(mdata) = c("domain_user", "obsnum", "user_id", "domain")
mdata = filter(mdata, obsnum != "NA")
#Merge with all_monthly_check by domain_name, user_id, and obsnum: keeping all rows in mdata
mdata <- merge(mdata, all_monthly_check, 
               by=c("domain", "user_id", "obsnum"), all.x = TRUE)
#Check # inactive months by looking at # NA's for all indicators (they should be the same)
sapply(mdata, summary)
#Sort by domain_name, user_id and obsnum
mdata <- arrange(mdata, domain, user_id, obsnum)
#Create logic vector of inactive months
mdata$inactive_month <- is.na(mdata$nvisits)
#Create logic vector of months before all inactive months
#Choose only the second element in inactive_month onwards and then append "F"
mdata$before_inactive <- c(mdata$inactive_month[-1], F)
return(mdata)
}

mdata <- pre_test_4(all_monthly_tula)

test_4 <- function(indicator, data) {
    active_subset <- data[data$before_inactive == F, ]
    before_inactive_subset <- data[data$before_inactive == T, ]
    
    test_4_compute_active <- active_subset %.%
        group_by(domain, user_id) %.%
        s_summarise(paste0('median_indicator_active=median(', indicator, ', na.rm=TRUE)'))
    
    test_4_compute_before_active <- before_inactive_subset %.%
        group_by(domain, user_id) %.%
        s_summarise(paste0('median_indicator_before_inactive=median(', indicator, ', na.rm=TRUE)'))
    
    #Merge the two data frames by domain and user_id and then compute % change
    test_4_compute <- merge(test_4_compute_active, test_4_compute_before_active,
                            by=c("domain", "user_id"))
    test_4_compute$per_change <-
        (test_4_compute$median_indicator_before_inactive-test_4_compute$median_indicator_active)/
        test_4_compute$median_indicator_active *100
    test_4_score <- median(test_4_compute$per_change, na.rm = T)
    return(test_4_score)
}

test_4_score_vector <- sapply(indicators_to_test, test_4, data=mdata)
print(test_4_score_vector)

#Combine all test vectors into dataframe
test_scores <- data.frame(cbind(test_1_score_vector, test_2_score_vector, 
                     test_3_score_vector, test_4_score_vector))

write.csv(test_scores, file = "test_scores.csv")

#------------------------------------------------------------------------#
#Test 3 code for single domain
#------------------------------------------------------------------------#

test_3 <- function(indicator, data) {
    holiday_subset1 <- data[data$month_abbr == "Dec", ]
    active_subset1 <- data[!(data$month_abbr == "Dec"), ]
    
    test_3_compute_holiday1 <- holiday_subset1 %.%
        group_by(domain, user_id) %.%
        s_summarise(paste0('median_indicator_holiday=median(', indicator, ', na.rm=TRUE)'))
    
    test_3_compute_active1 <- active_subset1 %.%
        group_by(domain, user_id) %.%
        s_summarise(paste0('median_indicator_active=median(', indicator, ', na.rm=TRUE)'))
    
    
    #Merge the two data frames by domain and user_id and then compute % change
    test_3_compute <- merge(test_3_compute_holiday1, test_3_compute_active1,
                            by=c("domain", "user_id"))
    
    test_3_compute$per_change <-
        (test_3_compute$median_indicator_holiday-test_3_compute$median_indicator_active)/
        test_3_compute$median_indicator_active *100
    test_3_score <- median(test_3_compute$per_change, na.rm = T)
    return(test_3_score)
}

test_3_score_vector <- sapply(indicators_to_test, test_3, data = all_monthly_tula)
print(test_3_score_vector)

#------------------------------------------------------------------------#
#Check all health sector domains to remove any with median # cases f/u == 0
#------------------------------------------------------------------------#
overall_split = ddply(all_monthly, .(domain_name), summarise,
                      cases_fu_med = median(follow_up_unique_case, na.rm=T))
overall_split = arrange(overall_split, cases_fu_med)



#------------------------------------------------------------------------#
#Older test code
#------------------------------------------------------------------------#

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


