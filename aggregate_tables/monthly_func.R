require(plyr)

# this calculates number of months of a given Date from the origin
monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  return(lt$year*12 + lt$mon)
} 

# compute next_month_1/3/5 outcome indicators
perf_predict <- function(x, n){ # n is the 1/3/5 or whatever months from current month that we want to look at if a user is active
  y <- rep(0, length(x))
  temp <- x + n
  index <- which(temp %in% x)
  y[index] <- 1
  return(y)
}

