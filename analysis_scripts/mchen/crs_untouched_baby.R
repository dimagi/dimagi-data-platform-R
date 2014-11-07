# a breakdown of inactive baby cases 
# domain: crs-remind

# FU rate: cases touched in last 120 / total open cases 

# calc months that a given case has been open for  
x_temp <- get_life_length(x_last, export_date)
x_temp <- get_age_range(x_temp, 90, 180, 360) 
table(x_temp$age_range); nrow(x_temp) 

# cases touched in last 120 
x_touched_120 <- get_subset_120(x_temp, "yes")
table(x_touched_120$age_range)

# open cases that are untouched in last 120 
x_untouched_120 <- get_subset_120(x_temp, "no")
x_open_untouched_120 <- x_untouched_120[which(x_untouched_120$last_visit_closed == FALSE),] 
table(x_open_untouched_120$age_range)


# average visits to cases created in last 3/6/9/12 months
test <- ddply(x_temp, .(age_range), summarize, mean(total_visits))

