library(lubridate)

test_that("floor_date works for different units", {
    base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
    is_time <- function(x) equals(as.POSIXct(x, tz = "UTC"))
    floor_base <- function(unit) floor_date(base, unit)
    expect_that(floor_base("second"), is_time("2009-08-03 12:01:59"))
    expect_that(floor_base("minute"), is_time("2009-08-03 12:01:00"))
    expect_that(floor_base("hour"), is_time("2009-08-03 12:00:00"))
    expect_that(floor_base("day"), is_time("2009-08-03 00:00:00"))
    expect_that(floor_base("week"), is_time("2009-08-02 00:00:00"))
    expect_that(floor_base("month"), is_time("2009-08-01 00:00:00"))
    expect_that(floor_base("year"), is_time("2009-01-01 00:00:00"))
})
