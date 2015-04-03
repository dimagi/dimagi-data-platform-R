## IMPORT / LOADING PACKAGES
# if(!require(installr)) {
# install.packages("installr"); require(installr)}

library(plyr)
library(dplyr)
library(reshape) # loading this package for the purpose of renaming variables
library(zoo) # converting month.index format 

data = tbl_df(read.csv("blog_data_2_2_15.csv", stringsAsFactors = FALSE))
d_1 = data
d_1 = data %>%
  filter(., calendar_month <= as.Date("2014-12-01")) # the rating surveys are done before the end of 2014

d_1_domain = unique(d_1$domain_numeric)

# read in multiple flw rating sheets
main_dir = getwd()
flw_rating_dir = file.path(getwd(), "flw_rating")
filenames = list.files(path = flw_rating_dir)
flw_rating_data = list()
for (i in 1:length(filenames)){
  flw_rating_data[[i]] = read.csv(paste(flw_rating_dir, "/", filenames[i], sep = ""), 
                                  stringsAsFactors = FALSE,
                                  nrows = 21)
}

# data cleaning
for(i in 1:length(filenames)){
  flw_rating_data[[i]] = tbl_df(flw_rating_data[[i]])
  flw_rating_data[[i]] = select(flw_rating_data[[i]], -starts_with("X"))
  flw_rating_data[[i]]$domain_numeric = as.numeric(gsub(".csv","",filenames[[i]]))
  #	flw_rating_data[[i]] = filter(flw_rating_data[[i]], as.numeric(Username %in% "") == 0)
  flw_rating_data[[i]] = filter(flw_rating_data[[i]], as.numeric(Education.background %in% "") == 0)
  flw_rating_data[[i]] = select(flw_rating_data[[i]], 
                                Username, Gender, Age.range, Education.background, 
                                Ability.to.use.CommCare,Overall.performance.as.an.FLW,Previous.work.experience.in.maternal.and.child.health,domain_numeric)
  
}

merged_rating = rbind(flw_rating_data[[2]], flw_rating_data[[1]])
for (i in 3:length(filenames)) {
  merged_rating = rbind(merged_rating, flw_rating_data[[i]])
}

# clean up values for each variable (believe it or not i did it manually)
# truncate all good* to good
merged_rating$Overall.performance.as.an.FLW = gsub("Good - meets or exceeds expectations", "Good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Good, meets or exceeds expectations ", "Good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Very good, exceeds most expectations", "Very good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Very good - exceeds most expectations", "Very good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("NA", "Unknown", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Needs improvement, does not meet expectations", "Needs improvement", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Newly involved not yet performed ", "Unknown", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("1. Very good", "Very good",merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("2. Good", "Good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("3. Needs improvement - does not meet expectations", "Needs improvement", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("4. Unknown", "Unknown",merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Average", "Needs improvement", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Excellent", "Very good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Satisfactory", "Good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Very good,survey was done throughly,  all the forms filled up completely, ", "Very good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Very, good, sincerely done the survey, all the forms filled up meticulosly", "Very good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW = gsub("Very Good", "Very good", merged_rating$Overall.performance.as.an.FLW)
merged_rating$Overall.performance.as.an.FLW[which(as.numeric(is.na(merged_rating$Overall.performance.as.an.FLW)) == 1)] <- "Unknown"
merged_rating$Overall.performance.as.an.FLW[merged_rating$Overall.performance.as.an.FLW==""] <- "Unknown" # set all blank cells to NA 


merged_rating$Ability.to.use.CommCare = gsub("Good - meets or exceeds expectations", "Good", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Good, meets or exceeds expectations ", "Good", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Very good, exceeds most expectations", "Very good", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Very good - exceeds most expectations", "Very good", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Needs improvement - does not meet expectations", "Needs improvement", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Needs improvement, does not meet expectations", "Needs improvement", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Newly involved not yet performed ", "Unknown", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("1. Very good", "Very good",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("2. Good", "Good", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("3. Needs improvement - does not meet expectations", "Needs improvement", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("3. Needs improvement", "Needs improvement", merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("4. Unknown", "Unknown",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Able to use effectively", "Very good",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Could not use effectively", "Needs improvement",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Able to manage", "Good",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("She is able to use CommCare", "Good",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Yes", "Good",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Has ability to use ", "Good",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Able to use CommCare", "Good",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("Very effectively use", "Very good",merged_rating$Ability.to.use.CommCare)
merged_rating$Ability.to.use.CommCare = gsub("YES ,Very efficiently", "Very good",merged_rating$Ability.to.use.CommCare)

# data check
unique(merged_rating$Overall.performance.as.an.FLW)
unique(merged_rating$Ability.to.use.CommCare)

# filter domains from monthly table
monthly = tbl_df(read.csv("monthly.csv"))
users = tbl_df(read.csv("users.csv"))

# import domain table from live db connection and match domain_numeric in merged_rating with domain names in domain table
colnames(domains)[2] <- c("domain_numeric") 
merged_rating = left_join(merged_rating, domains)
colnames(merged_rating)[1] <- c("username") # this adds dmain names to the merged_rating data table
merged_rating = select(merged_rating,
                       username, domain_numeric, name, 
                       Ability.to.use.CommCare, Overall.performance.as.an.FLW, 
                       Gender, Age.range, Education.background,
                       Previous.work.experience.in.maternal.and.child.health)
merged_rating = rename(merged_rating, c(name = "domain"))

# match username with user_id
domain_list = unique(merged_rating$domain)
monthly_sub = filter(monthly, domain %in% domain_list) # lata-medical is missing

userNameSplit = function(x){
  unlist(strsplit(x,"@"))[1]	
}
domainNameSplit = function(x){
  temp = unlist(strsplit(x, "@"))[2]
  unlist(strsplit(temp, "[.]"))[1]
}

users$uname = sapply(as.character(users$username), userNameSplit)
users$domain = sapply(as.character(users$username), domainNameSplit)
merged_rating = rename(merged_rating, c(username = "uname"))
merged_rating_2 = inner_join(merged_rating, users, by = c("uname", "domain"))

# for each user_id, retrieve monthly data
merged_rating_3 = left_join(merged_rating_2, monthly, by = c("user_id", "domain"))
merged_data = select(merged_rating_3, uname, domain_numeric, domain, 
                     Ability.to.use.CommCare, Overall.performance.as.an.FLW, user_id, username, user_pk, month.index, 
                     date_first_visit, date_last_visit, nvisits, active_days, active_day_percent, nforms, ncases_touched,
                     Gender, Age.range, Education.background, Previous.work.experience.in.maternal.and.child.health)

merged_data = rename(merged_data, c(Ability.to.use.CommCare = "ability_to_use_commcare",
                                    Overall.performance.as.an.FLW = "overall_performance_as_an_flw",
                                    Gender = "gender",
                                    Age.range = "age_range",
                                    Education.background = "edu_background",
                                    Previous.work.experience.in.maternal.and.child.health = "prev_experience"
))

# order data by domain
merged_data = arrange(merged_data, domain_numeric, user_pk, month.index)
n_distinct(merged_data$user_pk)

# drop users who have user_id but have no user_pk (lata-medical was removed)
to_drop = which(is.na(merged_data$user_pk)) 
# this would be the first working data set: One row for every active FLW month who was in the FLW rating during the time range
merged_data_2 = tbl_df(merged_data[-to_drop,]) 
merged_data_2 = filter(merged_data_2, as.Date(month.index) <= as.Date("2015-02-01")) # there are odd data points with a month index in 2023
merged_data_2$month.index = factor(as.Date(as.yearmon(merged_data_2$month.index)))

working_data_1 = select(merged_data_2, domain_numeric, domain, ability_to_use_commcare, overall_performance_as_an_flw,
                        gender, age_range, edu_background, prev_experience,
                        user_pk, month.index, active_days, active_day_percent, ncases_touched) # keeping activity measures of interest only
working_data_1 = arrange(working_data_1, domain_numeric, user_pk, month.index)
working_data_1$month.index = as.Date(as.yearmon(working_data_1$month.index))
#working_data_1 = working_data_1 %>%
#					filter(., month.index >= as.Date("2014-04-01")) %>%
#					filter(., month.index <= as.Date("2014-11-01"))

# working_data_1 = na.omit(working_data_1) # For now i will keep NA as a value


# working dataset 2
# One row for every month (active or not) in the time period for each FLW in the  FLW ratings. 
# This is similar to working_data_1, but the activity measures should have zero for inactive months

# create a sequence of months
# get the first and the last active month for each user
working_data_1 = arrange(working_data_1, domain_numeric, user_pk, month.index)
user_first_last = working_data_1 %>% 
  group_by(domain_numeric, user_pk) %>%
  do(data.frame(
    head(.,n=1)$month.index,
    tail(.,n=1)$month.index))
names(user_first_last) = c("domain_numeric", "user_pk", "first_month", "last_month")

# for each user, create a sequence of months
user_month_seq = list()
user_month_full = list() # adding inactive months back for each user
for(i in 1:nrow(user_first_last)){
  user_month_seq[[i]] = seq(as.Date(user_first_last$first_month[i]), 
                            as.Date(user_first_last$last_month[i]), 
                            by="mon")
  user_month_full[[i]] = data.frame(rep(user_first_last$user_pk[i],length(user_month_seq[i])),
                                    user_month_seq[i])
  names(user_month_full[[i]]) = c("user_pk", "month_seq")
}

user_month_full = tbl_df(do.call(rbind, user_month_full))
colnames(user_month_full)[2] = c("month.index")
# join the full-month table with working_data_1
working_data_2 = left_join(user_month_full, working_data_1, by = c("user_pk","month.index"))
# NA should be replaced with 0 (except domain_numeric, should be replaced )


# constructing user-month table
by_user_1 = group_by(working_data_1, domain_numeric, user_pk)
user_months_1 = summarise(by_user_1, um_1 = n_distinct(by_user$month.index)) # total active months per user
by_user_2 = group_by(working_data_2, user_pk) # Note here we do NOT need to order by domain_numeric
user_months_2 = summarise(by_user_2, um_2 = n_distinct(by_user_2$month.index)) # total months (active+inactive) per user
user_months = merge(user_months_1, user_months_2) 
user_months$gap_rate = round((user_months$um_2 - user_months$um_1)/user_months$um_2, digits = 2) # the low the gap rate, the more consistent usage of commcare 



# working dataset 3
# one row per FLW 
# create aggregate usage statistics for: active_days, ncases_touched
wd1_rm = which(is.na(working_data_1$active_days))
working_data_1_sub = tbl_df(working_data_1[-wd1_rm,])
n_distinct(working_data_1_sub$user_pk) # seems a few users are dropped
working_data_3 = working_data_1_sub %>%
  group_by(domain_numeric, user_pk) %>%
  do(data.frame(
    summarise(., round(median(active_days))),
    summarise(., round(median(ncases_touched))),
    summarise(., sum(active_days)),
    summarise(., sum(ncases_touched)),
    summarise(., round(mean(active_days))),
    summarise(., round(mean(ncases_touched)))
  ))

working_data_2$active_days = ifelse(is.na(working_data_2$active_days)==TRUE, 0, working_data_2$active_days)
working_data_2$ncases_touched = ifelse(is.na(working_data_2$ncases_touched)==TRUE, 0, working_data_2$ncases_touched)
working_data_4 = working_data_2 %>%
  group_by(user_pk) %>%
  do(data.frame(
    summarise(., round(median(active_days))),
    summarise(., round(median(ncases_touched)))
  ))
colnames(working_data_4)[2:3] = c("median_ad_including_inactive_months","median_nt_including_inactive_months")

# join working_data_3 and working_data_4 into one FLW table
working_data_4 = inner_join(working_data_3, working_data_4, by = "user_pk")
working_data_4 = rename(working_data_4, c(round.median.active_days..= "median_ad",
                                          round.median.ncases_touched.. = "median_nt",
                                          sum.active_days. = "sum_ad",
                                          sum.ncases_touched. = "sum_nt",
                                          round.mean.active_days.. = "average_ad",
                                          round.mean.ncases_touched.. = "average_nt"))

working_data_4 = rename(working_data_4, c(sum.ncases_touched. = "sum_nt",mean.ncases_touched. = "average_nt"))

# join performance measure with rating measure
user_rating = unique(select(merged_data_2, uname, user_pk, ability_to_use_commcare, overall_performance_as_an_flw))
working_data_5 = tbl_df(inner_join(working_data_4, user_rating, by = c("user_pk")))

# add gender, age_range, edu, prev_experience back to the FLW table
working_data_2$gender = gsub("1. Female", "Female", working_data_2$gender)
working_data_2$gender = gsub("2. Male", "Male", working_data_2$gender)
edu_levels = c("1. No schooling completed",
               "2. Nursery to 8th class",
               "3. Some high school with no degree",
               "4. Class 10",
               "5. CLass 12",
               "6. Some college with no degree",
               "7. Vocational training",
               "8. Bachelor's degree",
               "9. Master's degree or more",
               "10. Unknown")
prev_experience_levels = c("1. Very experienced",
                           "2. Some experience",
                           "3. Little experience",
                           "4. Unknown")
age_levels = c("Below 20", # left closed, right open
               "20-25",
               "25-30",
               "30-35",
               "35-40",
               "40-45",
               "45-50",
               "Older than 50")

working_data_2$age_range = factor(working_data_2$age_range, 
                                  levels = age_levels)
working_data_2$prev_experience = factor(working_data_2$prev_experience,
                                        levels = prev_experience_levels)

working_data_2$prev_experience = gsub("No experience, has work experience in disability field", prev_experience_levels[3], working_data_2$prev_experience)
working_data_2$prev_experience = gsub("Work directly with mother and child but not through CommCare", prev_experience_levels[2], working_data_2$prev_experience)
working_data_2$prev_experience = gsub("Mahila Sanghatana Adolsecnt Health specially in schools for 1 year,  Family survey special emphasis on maternal aspects about ante natal care, health education , immunization among children for last 4 years", prev_experience_levels[1], working_data_2$prev_experience)
working_data_2$prev_experience = gsub("ASHA (frontline Health worker)", prev_experience_levels[1], working_data_2$prev_experience)
working_data_2$prev_experience = factor(working_data_2$prev_experience)

working_data_2$age_range = gsub("37 years", age_levels[5], working_data_2$age_range)
working_data_2$age_range = gsub("54 years", age_levels[8], working_data_2$age_range)
working_data_2$age_range = gsub("45 years", age_levels[7], working_data_2$age_range)
working_data_2$age_range = gsub("44 years", age_levels[6], working_data_2$age_range)
working_data_2$age_range = gsub("30 years", age_levels[4], working_data_2$age_range)
working_data_2$age_range = gsub("35 years", age_levels[5], working_data_2$age_range)
working_data_2$age_range = gsub("40 years", age_levels[6], working_data_2$age_range)
working_data_2$age_range = gsub("46 years", age_levels[7], working_data_2$age_range)
working_data_2$age_range = gsub("55 years", age_levels[8], working_data_2$age_range)
working_data_2$age_range = gsub("31 -32years", age_levels[4], working_data_2$age_range)
working_data_2$age_range = gsub("1. Below 20", age_levels[1], working_data_2$age_range)
working_data_2$age_range = gsub("2. 20-25", age_levels[2], working_data_2$age_range)
working_data_2$age_range = gsub("3. 25-30", age_levels[3], working_data_2$age_range)
working_data_2$age_range = gsub("4. 30-35", age_levels[4], working_data_2$age_range)
working_data_2$age_range = gsub("5. 35-40", age_levels[5], working_data_2$age_range)
working_data_2$age_range = gsub("6. 40-45", age_levels[6], working_data_2$age_range)
working_data_2$age_range = gsub("7. 45-50", age_levels[7], working_data_2$age_range)
working_data_2$age_range = gsub("8. Older than 50", age_levels[8], working_data_2$age_range)

working_data_2$age_range = gsub("25", age_levels[3], working_data_2$age_range)
working_data_2$age_range = gsub("26", age_levels[3], working_data_2$age_range)
working_data_2$age_range = gsub("28", age_levels[3], working_data_2$age_range)
working_data_2$age_range = gsub("30", age_levels[4], working_data_2$age_range)
working_data_2$age_range = gsub("32", age_levels[4], working_data_2$age_range)
working_data_2$age_range = gsub("29", age_levels[3], working_data_2$age_range)
working_data_2$age_range = gsub("27", age_levels[3], working_data_2$age_range)


# DOMAIN TABLE
# columns: domain_numeric; overall_perf_score per level, total users per level
wd_5 = working_data_5
wd_5$overall_performance_as_an_flw = factor(wd_5$overall_performance_as_an_flw,
                                            levels=c("Unknown", "Needs improvement", "Good", "Very good"))
wd_5$overall_performance_as_an_flw_n = factor(wd_5$overall_performance_as_an_flw, 
                                              labels=(1:length(levels(factor(wd_5$overall_performance_as_an_flw)))))
wd_5$ability_to_use_commcare = factor(wd_5$ability_to_use_commcare,
                                      levels=c("Unknown", "Needs improvement", "Good", "Very good"))
wd_5$ability_to_use_commcare_n = factor(wd_5$ability_to_use_commcare, 
                                        labels=(1:length(levels(factor(wd_5$ability_to_use_commcare)))))

# for each domain, sum up the overall score at each performance level 
perf_score_long = wd_5 %>%
  group_by(domain_numeric, overall_performance_as_an_flw) %>%
  summarise(nusers = n_distinct(user_pk),
            perf_score = sum(overall_performance_as_an_flw_n))

# for each domain, get the median active days at each level of overall performance level
mad_by_perf_domain = wd_5 %>%
  group_by(domain_numeric, overall_performance_as_an_flw) %>%
  summarise(mad = median(median_ad))
tbl_1 = inner_join(perf_score_long, mad_by_perf_domain)
# reshape: long - wide format
tbl_2 = reshape(tbl_1, timevar = "overall_performance_as_an_flw", idvar = "domain_numeric", direction = "wide")
tbl_2[is.na(tbl_2)] <- 0 # replace all NA values with 0
tbl_3 = select(tbl_2, domain_numeric, contains("nusers"))
names(tbl_3) = c("domain_numeric", "Needs Improvement", "Good", "Very Good", "Unknown")
tbl_4 = select(tbl_2, domain_numeric, contains("mad"))
names(tbl_4) = names(tbl_3)
tbl_5 = select(tbl_2, domain_numeric, contains("score"))
names(tbl_5) = names(tbl_3)
# for each domain, calculate the total perf score at all performance level
perf_score_domain_sum = perf_score_long %>%
  group_by(domain_numeric) %>%
  summarise(perf_score_sum = sum(perf_score)) %>%
  arrange(., desc(perf_score_sum))

# split the data into multiple data frames by performance level
perf_score_levels = split(perf_score_long, as.numeric(perf_score_long$overall_performance_as_an_flw))

# FLW TABLE
# add relative activity level to each user with their domain
names(wd_5)[3:6] = c("median_ad", "median_nt", "sum_ad", "sum_nt")
wd_5 = wd_5 %>%
  group_by(domain_numeric) %>%
  mutate(ntile_mad = ntile(median_ad, 4)) # the higher the number, the higher the median active day value is
domain_size = perf_score_long %>%
  group_by(domain_numeric) %>%
  summarise(tot_users = sum(nusers))
domain_size$dsize = ifelse(domain_size$tot_users >= 10, "big", "small")
wd_6 = left_join(wd_5, domain_size)

# CONTINGENCY TABLE 1
c1 = select(wd_6, domain_numeric, overall_performance_as_an_flw, ability_to_use_commcare)
c1 = table(c3$overall_performance_as_an_flw, c3$ability_to_use_commcare,
           dnn = c("Overall performance as an FLW", "Ability to use CommCare"))
c1 = ftable(c1)

# CONTINGENCY TABLE 2
c2 = select(wd_6, domain_numeric, overall_performance_as_an_flw, ability_to_use_commcare, dsize)
c2 = ftable(c2$overall_performance_as_an_flw, c2$ability_to_use_commcare, c2$dsize)
names(attr(c2, "row.vars")) = c("Overall performance", "Ability to use CommCare")
c2

# DETECTION OF CONFOUNDING / EFFECT MODIFICATION


# REGRESSION
# Explanatory factor: monthly active days
# Outcome factor: overall performance as an flw
# Confounders: domain_numeric
# Effect modifiers: domain size, ability to use commcare, within-domain relative activity level as an flw

# converting categorical var domain_numeric to 13 dummy factors. same for domain size
ologit_dat = select(wd_6, domain_numeric, user_pk, median_ad, round.mean.active_days.., overall_performance_as_an_flw_n, ability_to_use_commcare, ntile_mad, dsize)
ologit_dat$domain_numeric = as.factor(ologit_dat$domain_numeric)		
ologit_dat$overall_performance_as_an_flw_n = as.numeric(ologit_dat$overall_performance_as_an_flw_n)			
domain_dummy = model.matrix(~domain_numeric, data = ologit_dat)
ologit_dat = cbind(ologit_dat, domain_dummy)
perf_glm_1 = glm(overall_performance_as_an_flw_n~median_ad, data = ologit_dat)
perf_glm_2 = glm(overall_performance_as_an_flw_n~median_ad + domain_numeric, data = ologit_dat)
perf_glm_3 = glm(overall_performance_as_an_flw_n~median_ad + domain_numeric + ability_to_use_commcare, data = ologit_dat)
perf_glm_4 = glm(overall_performance_as_an_flw_n~median_ad + domain_numeric + ability_to_use_commcare + domain_numeric*ability_to_use_commcare, data = ologit_dat)



ologit_dat = select(wd_6, user_pk, domain_numeric, overall_performance_as_an_flw, ability_to_use_commcare, dsize, round.mean.active_days.., median_ad, ntile_mad)
names(ologit_dat)[6] = c("mean_ad")
ologit_ft = ftable(ologit_dat$overall_performance_as_an_flw, ologit_dat$ability_to_use_commcare, ologit_dat$dsize)
names(attr(ologit_ft, "row.vars")) = c("Overall performance", "Ability to use CommCare")
domain_dummy = model.matrix(~domain_numeric, data = ologit_dat)
ologit_dat_2 = cbind(ologit_dat, domain_dummy)





# LOG-LINEAR ANALYSIS
odat = select(wd_6, domain_numeric, user_pk, median_ad, dsize,
              overall_performance_as_an_flw, 
              overall_performance_as_an_flw_n, 
              ability_to_use_commcare,
              ability_to_use_commcare_n)
odat$perf = ifelse(as.numeric(odat$overall_performance_as_an_flw_n) <= 2, "Not good enough", "Good enough")
odat$ability = ifelse(as.numeric(odat$ability_to_use_commcare_n) <= 2, "Not good enough", "Good enough" )
odat$active = ifelse(odat$median_ad >= 5, "More active", "Less active")

# frequency tables
f1 = table(odat$overall_performance_as_an_flw, dnn = c("Overall Performance"))
grp_f1 = table(odat$dsize, odat$overall_performance_as_an_flw, dnn = c("Domain size", "Overall Performance"))


# 2-way crosstabs
c1 = table(odat$median_ad, 
           odat$overall_performance_as_an_flw, dnn = c("Median active days per month", "Overall Performance"))
tbl_odat = table(odat$perf, odat$ability, odat$active)
op_oa = margin.table(tbl_odat, c(3,1)) # 2-way contingency table 
chisq.test(op_oa)  # as a building block to log-linear analysis, chi-square test is performed to test if performance and active levels are independent from each other
likelihood_test = op_oa[2]/margin.table(op_oa, 1)[2] / (op_oa[1]/margin.table(op_oa, 1)[1])

c2 = table(odat$ability_to_use_commcare,
           odat$overall_performance_as_an_flw, dnn = c("Ability", "Performance"))

op_ab = margin.table(tbl_odat, c(2,1))
chisq.test(op_ab)
likelihood_test_2 = op_ab[1]/margin.table(op_ab, 1)[1] / (op_ab[2]/margin.table(op_ab, 1)[2])

# GRAPHIC DISPLAY
# Freq distribution of overall performance
b_f1 = barchart(f1, xlab="Number of FLW", ylab="Overall Performance")
f2 = table(odat$median_ad)
b_f2 = barchart(f2, xlab="Number of FLW", ylab="Active days per month")
grid.arrange(b_f1, b_f2)

# barplot
b1 = barplot(t(c1), beside=TRUE, 
             horiz=TRUE, 
             legend=rownames(t(c1)), 
             col=c("lightblue","lightcyan","lavender","mistyrose"),
             xlim=c(0,max(c1)))	
# 10x4 mosaicplot for 2-way contingency table
m1 = mosaicplot(t(c1), shade=TRUE)

# 2x2x2 mosaicplot 
m2 = mosaicplot(tbl_odat, shade=TRUE)

