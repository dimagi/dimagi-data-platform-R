# % difference in indicators for each user for consectutive months
# This isn't for truly consecutive months, so later on, 
# we will only use rows with previous_month_active == T
#This will be used for test 1b

training_typical <- arrange(training_typical, user_pk, calendar_month)
users <- unique(training_typical$user_pk)

per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$nvisits)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_nvisits/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_nvisits <- per_diff_indicator


per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$active_day_percent)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_active_day_percent/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_active_day_percent <- per_diff_indicator

per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$nforms)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_nforms/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_nforms <- per_diff_indicator


per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$median_visit_duration)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_median_visit_duration/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_median_visit_duration <- per_diff_indicator


per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$median_visits_per_day)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_median_visits_per_day/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_median_visits_per_day <- per_diff_indicator

per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$time_using_cc)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_time_using_cc/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_time_using_cc <- per_diff_indicator


per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$ninteractions)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_ninteractions/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_ninteractions <- per_diff_indicator


per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$ncases_registered)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_ncases_registered/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_ncases_registered <- per_diff_indicator


per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$register_followup)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_register_followup/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_register_followup <- per_diff_indicator

per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$case_register_followup_rate)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_case_register_followup_rate/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_case_register_followup_rate <- per_diff_indicator

per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$ncases_touched)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_ncases_touched/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_ncases_touched <- per_diff_indicator

per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$nunique_followups)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_nunique_followups/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_nunique_followups <- per_diff_indicator

per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$sample_increase)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_sample_increase/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_sample_increase <- per_diff_indicator


per_diff_indicator <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  prev_indicator <- c()
  prev_indicator <- append(NA, single_user$sample_decrease)
  prev_indicator <- prev_indicator[-length(prev_indicator)]
  per_diff <- c()
  per_diff <- (single_user$diff_sample_decrease/prev_indicator)*100
  per_diff_indicator <- append(per_diff_indicator, per_diff)
  is.na(per_diff_indicator) <- is.nan(per_diff_indicator)
  is.na(per_diff_indicator) <- per_diff_indicator == Inf
}
training_typical$per_diff_sample_decrease <- per_diff_indicator