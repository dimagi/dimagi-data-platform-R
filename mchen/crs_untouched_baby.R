# a breakdown of inactive baby cases 
# domain: crs-remind

# a breakdown of not-touched baby cases by the number of times they were ever touched
x_untouched_120 <- get_subset_120(x_last, "no")
table(x_untouched_120$total_visits_breakdown)
x_untouched_120 <- get_life_length(x_untouched_120, export_date)
x_untouched_120 <- get_age_range(x_untouched_120, 90, 180, 360)
count(x_untouched_120, c("x_untouched_120$age_range", "x_untouched_120$total_visits_breakdown"))



# same steps but for touched cases
x_touched_120 <- get_subset_120(x_last, "yes")
table(x_touched_120$total_visits_breakdown)
x_touched_120 <- get_life_length(x_touched_120, export_date)
x_touched_120 <- get_age_range(x_touched_120, 90, 180, 360)
count(x_touched_120, c("x_touched_120$age_range", "x_touched_120$total_visits_breakdown"))

