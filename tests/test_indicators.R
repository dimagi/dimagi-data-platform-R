# TODO: It's a little kludgy that we have to set the working directory
# here.
setwd('..')
source('indicators.R')

test_that("aggregate works as expected", {
    data <- data.frame(a=c(1, 1, 2), b=c(1, 2, 3))
    grouped.data <- data %.% group_by(a)
    result <- aggregate(grouped.data, list(nvisits='nrow'))
    expect_that(result$nvisits, equals(c(2, 1)))
})

test_that("date indicators return accurate results", {
    visit_date <- as.Date(c('2000-01-01', '2000-01-02', '2000-01-05'))
    data <- data.frame(visit_date=visit_date)
    expect_that(date_first_visit(data), equals(as.Date('2000-01-01')))
    expect_that(date_last_visit(data), equals(as.Date('2000-01-05')))
    expect_that(days_on_cc(data), equals(5))
    expect_that(active_days(data), equals(3))
    expect_that(active_day_percent(data), equals(3 / 5))
    expect_that(median_visits_per_day(data), equals(1))

    data$visit_date[1] <- as.Date('1999-12-05')
    expect_that(calendar_month_on_cc(data), equals(2))

    data2 <- data.frame(time_start=c("2013-02-22 06:54:43", "2013-03-01 12:24:14", "2013-03-13 03:30:06", "2013-02-21 07:17:01"))
    data2$case_id <- c(1, 1, 1, 2)
    expect_that(median_time_btw_followup(data2), equals(10409.52, tolerance=0.01))
})
