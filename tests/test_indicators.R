# TODO: It's a little kludgy that we have to set the working directory
# here.
setwd('..')
source('indicators.R')

test_that("aggregate works as expected", {
    data <- data.frame(a=c(1, 1, 2), b=c(1, 2, 3))
    grouped.data <- data %.% group_by(a)
    result <- aggregate(grouped.data, c('nvisits'))
    expect_that(result$nvisits, equals(c(2, 1)))
})
