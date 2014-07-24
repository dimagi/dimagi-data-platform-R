# compute next_month_1/3/5 outcome indicators

perf_predict <- function(x, n){ # n is the 1/3/5 or whatever months from current month that we want to look at if a user is active
  y <- rep(0, length(x))
  for (i in 1:length(x)) {
    temp[i] <- x[i] + n
  }
  index <- which(temp %in% x)
  y[index] <- 1
  return(y)
}

monthly_merge <- ddply(monthly_merge, .(user_id), transform, next_mon_1 = perf_predict(numeric_index, 1))
monthly_merge <- ddply(monthly_merge, .(user_id), transform, next_mon_3 = perf_predict(numeric_index, 3))
monthly_merge <- ddply(monthly_merge, .(user_id), transform, next_mon_5 = perf_predict(numeric_index, 5))

