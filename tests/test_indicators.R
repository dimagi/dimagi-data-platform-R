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
})
