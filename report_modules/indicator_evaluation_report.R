#Usage indicator evaluation

indicators_to_test = c("visits", "active_days_percent", "median_visits_per_active_day", 
                       "case_registered", "follow_up_unique_case", "median_visit_duration")

#Convert calendar month to character because dplyr doesn't like yearmon class
all_monthly$calendar_month = as.character(all_monthly$calendar_month)

test_1_score_vector = c()


test_1 <- function(x) {
#TEST 1
#Calculate CVs by project by calendar_month
test_1_gp = group_by(all_monthly, domain_name, calendar_month)
test_1_compute = summarise(test_1_gp,
      mean_indicator = mean(x, na.rm = T),
      sd_indicator = sd(x, na.rm=T))
test_1_compute$cv = (test_1_compute$sd_indicator/test_1_compute$mean_indicator)*100
#Calculate CV of CVs by project
test_1_gp_CV = group_by(test_1_compute, domain_name)
test_1_compute_CV = summarise(test_1_gp_CV,
                              mean_indicator = mean(cv, na.rm = T),
                           sd_indicator = sd(cv, na.rm=T))
test_1_compute_CV$cv = (test_1_compute_CV$sd_indicator/test_1_compute_CV$mean_indicator)*100
test_1_score_vector = append(test_1_score_vector, median(test_1_compute_CV$cv))
return(test_1_score_vector)
}


#(TO DO)For some reason, SD is NA for some obsnum even though the aggregate table appears
#to have multiple rows for that obsnum. Need to explore what's happening here.
#With calendar_months with just one row, it seems to be okay: 
#we are just excluding these months from the eventual mean calculation
#Can also calculate differences between months per project, but need to consider cases of 
#skipped months - we are assuming no skipped months for now (too simplistic)
#Also, might want count diff in variance between one month and all other months -
#will this be a more robust measure rather than just diff between consecutive months?
#list_diff = tapply(test_1_compute$cv, test_1_compute$domain_name, diff)
#test_1_score = median(unlist(lapply(list_diff, median, na.rm=T)))


#TEST 2
#Group by project and then by obsnum
#Similar to (2), users with just one month on CC will be excluded from this analysis 
raw_percentile = ddply(all_monthly, .(domain_name, calendar_month), 
             function(x) transform(x, percentile=ecdf(x$visits)(x$visits)))
test_2_gp = group_by(raw_percentile, user_id)
test_2_compute = summarise(test_2_gp,
                           mean_indicator = mean(percentile, na.rm = T),
                           sd_indicator = sd(percentile, na.rm=T))
test_2_compute$cv = (test_2_compute$sd_indicator/test_2_compute$mean_indicator)*100
test_2_score = median(test_2_compute$cv, na.rm=T)
