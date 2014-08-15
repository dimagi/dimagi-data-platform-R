#Usage indicator evaluation

#TEST 1
#Convert calendar month to character because dplyr doesn't like yearmon class
all_monthly$calendar_month = as.character(all_monthly$calendar_month)
#Group by project and then by obsnum
test_1_gp = group_by(all_monthly, domain_name, obsnum)
test_1_compute = summarise(test_1_gp,
      mean_indicator = mean(visits, na.rm = T),
      sd_indicator = sd(visits, na.rm=T))
#For some reason, SD is NA for some obsnum even though the aggregate table appears
#to have multiple rows for that obsnum. Need to explore what's happening here.
test_1_compute$cv = (test_1_compute$sd_indicator/test_1_compute$mean_indicator)*100
#Calculate differences between months per project, but need to consider cases of 
#skipped months - we are assuming no skipped months for now (too simplistic)
#Also, might want count diff in variance between one month and all other months -
#will this be a more robust measure rather than just diff between consecutive months?
list_diff = tapply(test_1_compute$cv, test_1_compute$domain_name, diff)
test_1_score = median(unlist(lapply(list_diff, median, na.rm=T)))

#TEST 2
#Group by project and then by obsnum
raw_percentile = ddply(all_monthly, .(domain_name, obsnum), 
             function(x) transform(x, percentile=ecdf(x$visits)(x$visits)))
test_2_gp = group_by(raw_percentile, user_id)
test_2_compute = summarise(test_2_gp,
                           mean_indicator = mean(percentile, na.rm = T),
                           sd_indicator = sd(percentile, na.rm=T))
test_2_compute$cv = (test_2_compute$sd_indicator/test_2_compute$mean_indicator)*100
test_2_score = median(test_2_compute$cv, na.rm=T)
