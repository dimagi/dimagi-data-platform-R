require(dplyr)
#source('s_dplyr.R')
source(file.path("report_modules","s_dplyr.R", fsep = .Platform$file.sep))

all_monthly <- read.csv('all_monthly.csv')

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

indicators_to_test <- c(
    "visits",
    "active_days_percent",
    "median_visits_per_active_day",
    "case_registered",
    "follow_up_unique_case",
    "median_visit_duration",
    "nforms_per_month",
    "user_numeric"
    )
test_2_score_vector <- sapply(indicators_to_test, test_2)
print(test_2_score_vector)
