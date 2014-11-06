# a bit deeper look into inactive baby cases in crs-remind domain

# a breakdown of not-touched baby cases by the number of times they were ever touched
x_untouched_60 <- x_last[which(x_last$touched_60 == "no"),]
x_untouched_60$total_visits_breakdown <- ifelse(x_untouched_60$total_visits > median(x_untouched_60$total_visits), "1", "0")  
table(x_untouched_60$total_visits_breakdown)

x_untouched_120 <- x_last[which(x_last$touched_120 == "no"),]
x_untouched_120$total_visits_breakdown <- ifelse(x_untouched_120$total_visits > median(x_untouched_120$total_visits), "1", "0")  
table(x_untouched_120$total_visits_breakdown)


# a breakdown of how old not-touched baby cases are
x_untouched_60$life_length <- as.numeric(as.Date("2014-10-12") - as.Date(x_untouched_60$first_visit))
x_untouched_60$age_range <- ifelse(x_untouched_60$life_length < 90, "1", 
                                   ifelse(x_untouched_60$life_length >= 90 & x_untouched_60$life_length < 180, "2",
                                          ifelse(x_untouched_60$life_length >= 180 & x_untouched_60$life_length < 360, "3", "4")))
                                   
count(x_untouched_60, c("x_untouched_60$age_range", "x_untouched_60$total_visits_breakdown")) # cross tabulation


# same steps but for touched cases
x_touched_60 <- x_last[which(x_last$touched_60 == "yes"),]
x_touched_60$total_visits_breakdown <- ifelse(x_touched_60$total_visits > median(x_touched_60$total_visits), "1", "0")  
table(x_touched_60$total_visits_breakdown)

x_touched_60$life_length <- as.numeric(as.Date("2014-10-12") - as.Date(x_touched_60$first_visit))
x_touched_60$age_range <- ifelse(x_touched_60$life_length < 90, "1", 
                                   ifelse(x_touched_60$life_length >= 90 & x_touched_60$life_length < 180, "2",
                                          ifelse(x_touched_60$life_length >= 180 & x_touched_60$life_length < 360, "3", "4")))

count(x_touched_60, c("x_touched_60$age_range", "x_touched_60$total_visits_breakdown")) 
